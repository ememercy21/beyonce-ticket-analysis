# Export model results for the interactive page ----------------------------------
#
# Input : models/price_gam.rds, data/processed/listings_scored.rds, shows.csv
# Output: web/data.json
#
# For each show and seat tier: the model's expected price for a typical sale
# (2 tickets, row 15; VIP = Club Renaissance) on each day before the show, a
# show-bootstrap 95% range for the change relative to 30 days out, and the actual
# sales. Plus the adjustments the deal checker needs (row, quantity, VIP package)
# and the residual distribution that turns a price into a deal score.
#
# Run from the project root after R/02_model.R:  Rscript R/03_export_web.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(mgcv)
  library(jsonlite)
  library(here)
})

set.seed(2023)
fit    <- readRDS(here("models", "price_gam.rds"))
scored <- readRDS(here("data", "processed", "listings_scored.rds"))
shows  <- read_csv(here("data", "processed", "shows.csv"), show_col_types = FALSE)
d      <- fit$model  # the exact data the model was fitted on
resid_sd <- sqrt(fit$sig2)
onsale <- as.Date("2023-02-01")
first_sale <- min(scored$price_date)

tiers <- levels(d$seat_tier)
fl <- \(var, val) factor(val, levels = levels(d[[var]]))

# A typical sale, varied by show, tier and day below
base_row <- tibble(
  weekend = fl("weekend", "Weeknight"), multi_night = fl("multi_night", "Single night"),
  later_night = fl("later_night", "First night"), canada = fl("canada", "US"),
  qty_group = fl("qty_group", "2"), row_known = fl("row_known", "Row"), row_num = 15,
  pkg_bhive = 0, pkg_risers = 0, pkg_vip_floor = 0)

show_feats <- shows |> transmute(
  show_id,
  weekend = fl("weekend", if_else(weekday %in% c("Friday", "Saturday"), "Fri/Sat", "Weeknight")),
  multi_night = fl("multi_night", if_else(n_shows_in_city > 1, "Multi-night city", "Single night")),
  later_night = fl("later_night", if_else(city_night > 1, "Later night", "First night")),
  canada = fl("canada", if_else(country == "CA", "Canada", "US")),
  show_day = as.numeric(event_date - onsale),
  max_days = as.integer(event_date - first_sale))

grid <- show_feats |>
  mutate(days_before = map(max_days, \(m) 0:min(m, 200))) |>
  unnest(days_before) |>
  cross_join(tibble(seat_tier = factor(tiers, levels = tiers))) |>
  cross_join(base_row |> select(-weekend, -multi_night, -later_night, -canada)) |>
  mutate(sale_day = show_day - days_before, show_f = factor(show_id, levels = levels(d$show_f))) |>
  arrange(show_id, seat_tier, days_before)

grid$lp <- as.numeric(predict(fit, grid))

# Bootstrap: resample whole shows, refit, and record each curve's change vs. 30
# days out. The show effect cancels in that difference, so it is left out.
n_boot <- 200
i30 <- grid |> mutate(i = row_number()) |> group_by(show_id, seat_tier) |>
  mutate(i30 = i[days_before == 30]) |> pull(i30)
show_ids <- levels(d$show_f)
final_formula <- formula(fit)
show_level_terms <- c("weekend", "multi_night", "later_night", "canada")
pkg_terms <- c("pkg_bhive", "pkg_risers", "pkg_vip_floor")

boot <- map(seq_len(n_boot), function(b) {
  repeat {
    pick <- sample(show_ids, replace = TRUE)
    db <- map(seq_along(pick), \(i) d[d$show_f == pick[i], ] |> mutate(show_f = paste0("b", i))) |>
      list_rbind() |> mutate(show_f = factor(show_f))
    if (all(map_int(db[show_level_terms], n_distinct) == 2) && all(map_dbl(db[pkg_terms], max) == 1)) break
  }
  fb <- bam(final_formula, data = db, method = "fREML", discrete = TRUE)
  gb <- mutate(grid, show_f = factor(levels(db$show_f)[1], levels(db$show_f)))
  lp <- as.numeric(predict(fb, gb, exclude = "s(show_f)"))
  lp - lp[i30]
}) |> do.call(what = cbind)

grid$lo <- apply(boot, 1, quantile, 0.025)
grid$hi <- apply(boot, 1, quantile, 0.975)

# Deal-checker adjustments, as log-price differences from the typical sale
ref1 <- grid |> slice(1)
adj <- function(changes) {
  as.numeric(predict(fit, mutate(ref1, !!!changes))) - ref1$lp
}
row_adj  <- map_dbl(1:40, \(r) adj(list(row_num = r)))
norow    <- adj(list(row_known = fl("row_known", "No row"), row_num = median(d$row_num)))
qty_adj  <- map_dbl(levels(d$qty_group), \(q) adj(list(qty_group = fl("qty_group", q))))
pkg_adj  <- c(`Club Renaissance` = 0,
              `B-Hive` = adj(list(pkg_bhive = 1)),
              `Risers` = adj(list(pkg_risers = 1)),
              `VIP floor` = adj(list(pkg_vip_floor = 1)))

# Residual distribution behind the deal score (log of price / expected price)
resid <- log(scored$price / scored$expected_price)
resid_q <- quantile(resid, seq(0, 1, by = 0.001), names = FALSE)

# Assemble ------------------------------------------------------------------------
r3 <- \(x) round(x, 3)
mean_price <- \(lp) round(exp(lp + resid_sd^2 / 2))

curves <- grid |>
  group_by(show_id, seat_tier) |>
  summarise(price = list(mean_price(lp)), lo = list(r3(lo)), hi = list(r3(hi)), .groups = "drop") |>
  group_by(show_id) |>
  summarise(tiers = list(setNames(pmap(list(price, lo, hi), \(p, l, h) list(price = p, lo = l, hi = h)),
                                  as.character(seat_tier))), .groups = "drop")

sales <- scored |>
  group_by(show_id, seat_tier) |>
  summarise(pts = list(unname(as.matrix(tibble(days_before, round(price))))), .groups = "drop") |>
  group_by(show_id) |>
  summarise(tiers = list(setNames(pts, as.character(seat_tier))), .groups = "drop")

premium <- tibble(show_id = levels(d$show_f),
                  premium = coef(fit)[grepl("s\\(show_f\\)", names(coef(fit)))]) |>
  mutate(premium = round(100 * (exp(premium) - 1), 1))

show_out <- shows |>
  left_join(premium, by = "show_id") |>
  left_join(show_feats |> select(show_id, max_days), by = "show_id") |>
  left_join(scored |> count(show_id, name = "n_sales"), by = "show_id") |>
  transmute(id = show_id, city, date = format(event_date), weekday, premium, n_sales,
            max_days = pmin(max_days, 200))

out <- list(
  generated = format(Sys.Date()),
  tiers = tiers,
  vip_packages = names(pkg_adj),
  shows = show_out,
  curves = setNames(curves$tiers, curves$show_id),
  sales = setNames(sales$tiers, sales$show_id),
  adjust = list(row = r3(row_adj), no_row = r3(norow), qty = setNames(as.list(r3(qty_adj)), levels(d$qty_group)),
                package = as.list(r3(pkg_adj))),
  resid_quantiles = r3(resid_q),
  resid_sd = r3(resid_sd),
  n_sales = nrow(scored),
  n_boot = n_boot
)

dir.create(here("web"), showWarnings = FALSE)
write_json(out, here("web", "data.json"), auto_unbox = TRUE, digits = NA)
message(sprintf("Wrote web/data.json (%.0f KB)", file.size(here("web", "data.json")) / 1024))
