# Price model, timing curves and deal score -------------------------------------
#
# Question: for comparable seats, how does the resale price change as the show
# gets closer, and how good was a given purchase compared with similar sales?
#
# Model: log(price) ~ seat tier + show features + quantity + row
#                     + a separate smooth of days-before-show for each tier
#                     (+ a random effect per show, for scoring known shows)
#
# Validation: leave-one-CITY-out cross-validation. Every show in the held-out
# city is predicted by a model that never saw that city, which is the honest
# test for "a new tour stop".
#
# Input : data/processed/listings.rds, data/processed/shows.csv
# Output: data/processed/listings_scored.rds/.csv, models/price_gam.rds,
#         reports/model_results.md, reports/figures/*.png
#
# Run from the project root:  Rscript R/02_model.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(mgcv)
  library(ggplot2)
  library(scales)
  library(here)
})

set.seed(404)
fig_dir <- here("reports", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(here("models"), showWarnings = FALSE)

# 1. Features ------------------------------------------------------------------

# Row labels are numbers at most venues, letters at some (A = 1, AA = 27).
row_to_num <- function(r) {
  r <- str_to_upper(str_squish(r))
  out <- suppressWarnings(as.numeric(if_else(str_detect(r, "^\\d+$"), r, NA_character_)))
  one <- str_detect(r, "^[A-Z]$") & r != "GA"
  two <- str_detect(r, "^([A-Z])\\1$")
  out[which(one)] <- match(r[which(one)], LETTERS)
  out[which(two)] <- 26 + match(str_sub(r[which(two)], 1, 1), LETTERS)
  out
}

shows <- read_csv(here("data", "processed", "shows.csv"), show_col_types = FALSE)

d <- readRDS(here("data", "processed", "listings.rds")) |>
  filter(seat_tier != "Other") |>
  left_join(shows |> select(show_id, weekday, n_shows_in_city, city_night), by = "show_id") |>
  mutate(
    seat_tier  = droplevels(seat_tier),
    vip_package = droplevels(vip_package),
    pkg_bhive     = as.numeric(vip_package == "B-Hive"),
    pkg_risers    = as.numeric(vip_package == "Risers"),
    pkg_vip_floor = as.numeric(vip_package == "VIP floor"),
    log_price  = log(price),
    weekend    = factor(weekday %in% c("Friday", "Saturday"), c(FALSE, TRUE), c("Weeknight", "Fri/Sat")),
    multi_night = factor(n_shows_in_city > 1, c(FALSE, TRUE), c("Single night", "Multi-night city")),
    later_night = factor(city_night > 1, c(FALSE, TRUE), c("First night", "Later night")),
    canada     = factor(country == "CA", c(FALSE, TRUE), c("US", "Canada")),
    qty_group  = factor(pmin(qty, 5), 1:5, c("1", "2", "3", "4", "5+")),
    row_num    = pmin(row_to_num(row), 40),
    row_known  = factor(!is.na(row_num), c(FALSE, TRUE), c("No row", "Row")),
    row_num    = coalesce(row_num, median(row_num, na.rm = TRUE)),  # flagged by row_known
    sale_day   = as.numeric(price_date - as.Date("2023-02-01")),   # calendar time since on-sale
    show_f     = factor(show_id),
    city_f     = factor(city)
  )

# 2. Candidate models -------------------------------------------------------------

show_terms <- "weekend + multi_night + later_night + canada"
# VIP package effects are measured against Club Renaissance and are 0 outside VIP
# (a "Not VIP" level would duplicate the seat-tier terms).
seat_terms <- "pkg_bhive + pkg_risers + pkg_vip_floor + qty_group + row_known + s(row_num, k = 6)"

specs <- list(
  "1. Seat tier only"                  = "log_price ~ seat_tier",
  "2. + show & seat features"          = paste("log_price ~ seat_tier +", show_terms, "+", seat_terms),
  "3. + timing curve (by tier)"        = paste("log_price ~ seat_tier +", show_terms, "+", seat_terms,
                                               "+ s(days_before, by = seat_tier, k = 10)"),
  "4. + calendar trend"                = paste("log_price ~ seat_tier +", show_terms, "+", seat_terms,
                                               "+ s(days_before, by = seat_tier, k = 10) + s(sale_day, k = 10)")
)

fit_gam <- function(formula, data) bam(as.formula(formula), data = data, method = "fREML", discrete = TRUE)

# 3. Leave-one-city-out cross-validation -----------------------------------------

metrics <- function(pred_log, d) {
  tibble(
    rmse_log  = sqrt(mean((d$log_price - pred_log)^2)),
    r2_log    = 1 - sum((d$log_price - pred_log)^2) / sum((d$log_price - mean(d$log_price))^2),
    mdae_usd  = median(abs(d$price - exp(pred_log))),
    mape_pct  = 100 * median(abs(d$price - exp(pred_log)) / d$price)
  )
}

cities <- levels(d$city_f)
cv_preds <- map(names(specs), function(m) {
  map(cities, function(cty) {
    train <- filter(d, city_f != cty)
    test  <- filter(d, city_f == cty)
    fit <- fit_gam(specs[[m]], train)
    tibble(listing_id = test$listing_id, model = m, pred = as.numeric(predict(fit, test)))
  }) |> list_rbind()
}) |> list_rbind()

cv_results <- cv_preds |>
  left_join(d |> select(listing_id, log_price, price, seat_tier, city), by = "listing_id") |>
  group_by(model) |>
  group_modify(\(g, k) metrics(g$pred, g)) |>
  ungroup()

cv_by_city <- cv_preds |>
  filter(model == names(specs)[2]) |>
  left_join(d |> select(listing_id, log_price, price, city), by = "listing_id") |>
  group_by(city) |>
  group_modify(\(g, k) metrics(g$pred, g)) |>
  ungroup() |> arrange(desc(rmse_log))

print(cv_results)

# 3b. Known-show cross-validation (10 random folds) ---------------------------------
# Scoring a sale at a show we already have data for is the deal-score use case.
# Here every model gets a show random effect, so timing terms are judged on
# within-show price movement rather than on differences between shows.

folds <- sample(rep(1:10, length.out = nrow(d)))
known_cv <- map(names(specs)[-1], function(m) {
  f <- paste(specs[[m]], "+ s(show_f, bs = 're')")
  pred <- numeric(nrow(d))
  for (k in 1:10) {
    fitk <- fit_gam(f, d[folds != k, ])
    pred[folds == k] <- predict(fitk, d[folds == k, ])
  }
  metrics(pred, d) |> mutate(model = paste(m, "+ show effect"), .before = 1)
}) |> list_rbind()
print(known_cv)

# 4. Final model on all data, with a random effect per show -------------------------
# The show effect absorbs everything about a specific night (venue, local demand,
# currency) that the show-level features miss. It is used for scoring sales at
# known shows. The CV above shows how well the model does without it.

final_formula <- paste(specs[[4]], "+ s(show_f, bs = 're')")
fit <- fit_gam(final_formula, d)
saveRDS(fit, here("models", "price_gam.rds"))

s_fit <- summary(fit)
resid_sd <- sqrt(fit$sig2)

# Show-level feature effects, as % price differences
param_tbl <- tibble(term = names(s_fit$p.coeff), est = s_fit$p.coeff, se = s_fit$se[names(s_fit$p.coeff)]) |>
  filter(term != "(Intercept)") |>
  mutate(effect_pct = 100 * (exp(est) - 1),
         lo_pct = 100 * (exp(est - 1.96 * se) - 1),
         hi_pct = 100 * (exp(est + 1.96 * se) - 1))

# 5. Timing curves: price vs days before show, per tier ----------------------------
# Within a show, calendar date = show date - days before, so the straight-line
# parts of the two smooths can't be told apart once a show effect is in the model.
# A buyer waiting for one show moves along both at once, and that combined path
# IS identified. So the curve follows a buyer of a typical mid-tour show (the
# median show date): each day closer to the show is also one calendar day later.
# Hold everything else at a typical sale (2 tickets, row 15, weeknight,
# single-night US show), leave out the show effect, and express each tier's curve
# relative to its own price 120 days out.

ref_show_date <- median(shows$event_date)
ref_show_day  <- as.numeric(ref_show_date - as.Date("2023-02-01"))

ref <- d |> slice(1) |> mutate(
  weekend = factor("Weeknight", levels(d$weekend)), multi_night = factor("Single night", levels(d$multi_night)),
  later_night = factor("First night", levels(d$later_night)), canada = factor("US", levels(d$canada)),
  qty_group = factor("2", levels(d$qty_group)), row_known = factor("Row", levels(d$row_known)), row_num = 15)

# VIP curve is drawn for Club Renaissance, the package with the most sales.
vip_ref_pkg <- "Club Renaissance"

max_days <- 180
grid <- expand_grid(seat_tier = factor(levels(d$seat_tier), levels(d$seat_tier)), days_before = 0:max_days) |>
  cross_join(ref |> select(-seat_tier, -days_before, -sale_day)) |>
  mutate(sale_day = ref_show_day - days_before,
         vip_package = factor(if_else(seat_tier == "VIP", vip_ref_pkg, "Not VIP"), levels(d$vip_package)),
         pkg_bhive = 0, pkg_risers = 0, pkg_vip_floor = 0)

Xp <- predict(fit, grid, type = "lpmatrix", exclude = "s(show_f)")
beta <- coef(fit); V <- vcov(fit)
grid$fit <- as.numeric(Xp %*% beta)

anchor_idx <- with(grid, which(days_before == 120))
tier_idx <- as.integer(grid$seat_tier)
rel_curve <- function(lp) lp - lp[anchor_idx[tier_idx]]

# Intervals: show-level bootstrap. Sales at the same show (and on the same day)
# are correlated, so the model's own standard errors are too narrow. Resample
# whole shows with replacement, refit, and recompute the curves.
n_boot <- 200
show_ids <- levels(d$show_f)
show_level_terms <- c("weekend", "multi_night", "later_night", "canada")
pkg_levels <- levels(d$vip_package)
boot_one <- function() {
  repeat {  # redraw samples that miss a level of a show feature (e.g. no Canadian show)
    pick <- sample(show_ids, replace = TRUE)
    db <- map(seq_along(pick), \(i) filter(d, show_id == pick[i]) |> mutate(show_f = paste0("b", i))) |>
      list_rbind() |> mutate(show_f = factor(show_f))
    if (all(map_int(db[show_level_terms], n_distinct) == 2) &&
        n_distinct(db$vip_package) == length(pkg_levels)) break
  }
  fb <- fit_gam(final_formula, db)
  gb <- mutate(grid, show_f = factor(levels(db$show_f)[1], levels(db$show_f)))
  rel_curve(as.numeric(predict(fb, gb, exclude = "s(show_f)")))
}
boot_rel <- map(seq_len(n_boot), \(b) boot_one()) |> do.call(what = cbind)

grid <- grid |> mutate(
  rel     = exp(rel_curve(fit)) - 1,
  rel_lo  = exp(apply(boot_rel, 1, quantile, 0.025)) - 1,
  rel_hi  = exp(apply(boot_rel, 1, quantile, 0.975)) - 1,
  price   = exp(fit + resid_sd^2 / 2)   # mean price on the dollar scale
)

# Final-month question: compared with buying 30 days out, how much does waiting
# until the last week or show day change the price? (bootstrap 95% interval)
at <- \(day) which(grid$days_before == day)
late_window <- map(levels(d$seat_tier), function(t) {
  i30 <- intersect(at(30), which(grid$seat_tier == t))
  i7  <- intersect(at(7),  which(grid$seat_tier == t))
  i0  <- intersect(at(0),  which(grid$seat_tier == t))
  ci  <- \(x) exp(quantile(x, c(0.025, 0.975))) - 1
  tibble(seat_tier = t,
         wait_to_7 = exp(grid$fit[i7] - grid$fit[i30]) - 1,
         wait_to_7_lo = ci(boot_rel[i7, ] - boot_rel[i30, ])[1], wait_to_7_hi = ci(boot_rel[i7, ] - boot_rel[i30, ])[2],
         wait_to_0 = exp(grid$fit[i0] - grid$fit[i30]) - 1,
         wait_to_0_lo = ci(boot_rel[i0, ] - boot_rel[i30, ])[1], wait_to_0_hi = ci(boot_rel[i0, ] - boot_rel[i30, ])[2])
}) |> list_rbind()

tier_n <- d |> count(seat_tier)
timing_summary <- grid |>
  group_by(seat_tier) |>
  summarise(
    cheapest_day     = days_before[which.min(fit)],
    vs_120_at_best   = rel[which.min(fit)],
    vs_120_at_30     = rel[days_before == 30],
    vs_120_at_7      = rel[days_before == 7],
    vs_120_at_0      = rel[days_before == 0],
    lo_0 = rel_lo[days_before == 0], hi_0 = rel_hi[days_before == 0],
    .groups = "drop") |>
  left_join(tier_n, by = "seat_tier")

tier_cols <- c(VIP = "#7b2cbf", Floor = "#c9184a", Lower = "#e85d04",
               `Club/Middle` = "#2a9d8f", Upper = "#1d4e89", Suite = "#6c757d")

p_timing <- grid |>
  filter(seat_tier != "Suite") |>
  ggplot(aes(days_before, rel, colour = seat_tier, fill = seat_tier)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4) +
  geom_ribbon(aes(ymin = rel_lo, ymax = rel_hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  scale_x_reverse(breaks = c(180, 120, 90, 60, 30, 14, 0)) +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_manual(values = tier_cols, aesthetics = c("colour", "fill")) +
  facet_wrap(~ seat_tier, nrow = 1) +
  labs(title = "How resale prices move as the show approaches",
       subtitle = sprintf("Price vs. 120 days out for a comparable seat at a mid-tour show (%s); model-adjusted, show-bootstrap 95%% interval",
                          format(ref_show_date, "%b %d")),
       x = "Days before show", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))
ggsave(file.path(fig_dir, "timing_curves.png"), p_timing, width = 11, height = 4, dpi = 200, bg = "white")

# 6. Deal score ---------------------------------------------------------------------
# Expected price = what the final model predicts for that exact seat tier, show,
# row, quantity and day. The score is how much less than that the buyer paid,
# turned into a percentile: 90 means cheaper relative to expectation than 90% of
# all sales. Seat price itself never enters the score, so expensive tiers aren't
# favoured by construction.

d <- d |> mutate(
  expected_price = exp(as.numeric(predict(fit, d)) + resid_sd^2 / 2),
  pct_vs_expected = price / expected_price - 1,
  deal_score = round(100 * (1 - percent_rank(log_price - log(expected_price)))),
  good_deal = deal_score >= 80
)

good_deal_by_window <- d |>
  mutate(window = cut(days_before, c(-1, 3, 7, 14, 30, 60, 120, 240),
                      labels = c("0-3", "4-7", "8-14", "15-30", "31-60", "61-120", "121+"))) |>
  group_by(seat_tier, window) |>
  summarise(pct_good = mean(good_deal), n = n(), .groups = "drop")

show_effects <- tibble(show_id = levels(d$show_f),
                       show_effect = coef(fit)[str_detect(names(coef(fit)), "s\\(show_f\\)")]) |>
  mutate(show_premium_pct = 100 * (exp(show_effect) - 1)) |>
  left_join(shows |> select(show_id, city, event_date, weekday), by = "show_id") |>
  arrange(desc(show_premium_pct))

scored <- d |> select(listing_id, show_id, city, event_date, price_date, days_before, zone, section,
                      row, seat_tier, vip_package, qty, price, expected_price, pct_vs_expected, deal_score, good_deal)
saveRDS(scored, here("data", "processed", "listings_scored.rds"))
write_csv(scored, here("data", "processed", "listings_scored.csv"))

# Diagnostics figure
png(file.path(fig_dir, "diagnostics.png"), width = 1600, height = 800, res = 150)
par(mfrow = c(1, 2))
r <- residuals(fit)
plot(fitted(fit), r, pch = ".", col = rgb(0, 0, 0, 0.2), xlab = "Fitted log price", ylab = "Residual",
     main = "Residuals vs fitted"); abline(h = 0, col = "red")
qqnorm(r, pch = ".", main = "Residual QQ plot"); qqline(r, col = "red")
dev.off()

# 7. Report --------------------------------------------------------------------------

md_table <- function(df, digits = 3) {
  df <- mutate(df, across(where(is.numeric), \(x) as.character(round(x, digits))),
               across(everything(), \(x) replace_na(as.character(x), "")))
  c(paste("|", paste(names(df), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|"),
    apply(df, 1, \(r) paste("|", paste(r, collapse = " | "), "|")))
}
pct <- \(x) sprintf("%+.0f%%", 100 * x)

report <- c(
  "# Model results",
  "",
  sprintf("Generated by `R/02_model.R` on %s. %s sales across %d shows (the 27 `Other`-tier rows are excluded).",
          format(Sys.Date()), format(nrow(d), big.mark = ","), n_distinct(d$show_id)),
  "",
  "## Model comparison: leave-one-city-out cross-validation",
  "",
  "Each city is held out in turn and predicted by a model trained on the other 21.",
  "`mdae_usd` is the median absolute error in dollars per ticket.",
  "",
  md_table(cv_results),
  "",
  "Worst-predicted cities (model 2):",
  "",
  md_table(head(cv_by_city, 6)),
  "",
  "## Model comparison: known shows (10-fold CV, show random effect)",
  "",
  "Timing terms describe price movement *within* a show, so they are judged here.",
  "",
  md_table(known_cv),
  "",
  "## Final model",
  "",
  sprintf("Fitted on all data with a random effect per show. Deviance explained: %.1f%%; residual SD on the log scale: %.3f.",
          100 * s_fit$dev.expl, resid_sd),
  "",
  "Fixed effects (% difference in price, 95% CI). Tier effects are relative to VIP (Club Renaissance); `pkg_*` effects compare other VIP packages with Club Renaissance:",
  "",
  md_table(param_tbl |> transmute(term, effect_pct, lo_pct, hi_pct), 1),
  "",
  "## When to buy",
  "",
  "![Timing curves](figures/timing_curves.png)",
  "",
  "Model-adjusted price vs. 120 days out, for a comparable seat:",
  "",
  md_table(timing_summary |> transmute(seat_tier, n, cheapest_day,
                                       `at cheapest` = pct(vs_120_at_best),
                                       `30 days out` = pct(vs_120_at_30),
                                       `7 days out` = pct(vs_120_at_7),
                                       `show day` = pct(vs_120_at_0),
                                       `show day 95% CI` = paste0(pct(lo_0), " to ", pct(hi_0)))),
  "",
  "**The final month.** Compared with buying 30 days out (bootstrap 95% interval):",
  "",
  md_table(late_window |> transmute(seat_tier,
                                    `wait until 7 days out` = paste0(pct(wait_to_7), " (", pct(wait_to_7_lo), " to ", pct(wait_to_7_hi), ")"),
                                    `wait until show day` = paste0(pct(wait_to_0), " (", pct(wait_to_0_lo), " to ", pct(wait_to_0_hi), ")"))),
  "",
  sprintf("Curves follow a buyer of a mid-tour show (%s): each day closer to the show is also a calendar day later, so tour-wide market swings (like the early-March slump) show up in the curve. Intervals come from %d show-level bootstrap refits.", format(ref_show_date, "%b %d"), n_boot),
  "",
  "Share of sales that were a good deal (deal score 80+, i.e. the best 20% relative to expected price), by tier and days before the show:",
  "",
  md_table(good_deal_by_window |> mutate(pct_good = round(100 * pct_good)) |>
             pivot_wider(id_cols = seat_tier, names_from = window, values_from = pct_good)),
  "",
  "## Show premiums",
  "",
  "Estimated show effect (how much pricier or cheaper a comparable seat was than the model's baseline):",
  "",
  md_table(show_effects |> transmute(show_id, weekday, show_premium_pct), 1),
  "",
  "## Diagnostics",
  "",
  "![Diagnostics](figures/diagnostics.png)",
  "",
  "## How to read this",
  "",
  "- These are observational sales. The timing curve says what comparable seats *sold for* at each point, not what a single buyer would have paid by waiting.",
  "- Late sales are a selected group: tickets still available near show day may be worse seats within a tier.",
  "- Toronto and Vancouver prices may be in CAD, so the Canada effect could partly reflect currency.",
  "- Residuals have a heavy right tail (some sales far above expected). The deal score uses ranks, so it is not thrown off by them, but dollar predictions for the priciest sales are less reliable.",
  "- Fixed-effect intervals in the table above come from the model and assume independent sales, so treat them as optimistic. Show-level features rest on only 29 shows."
)
writeLines(report, here("reports", "model_results.md"))
message("Done. Report: reports/model_results.md")
