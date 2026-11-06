# Build the clean listings dataset ----------------------------------------------
#
# Input : data/raw/listings/*.csv  (one resale price-history export per show)
#         data/raw/show_manifest.csv (city, date, keep/drop decision per file)
# Output: data/processed/listings.rds / .csv  (one row per resale sale)
#         data/processed/shows.csv            (one row per show)
#         reports/data_quality.md             (what was removed and why)
#
# Run from the project root:  Rscript R/01_build_data.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(here)
})

source(here("R", "seat_tiers.R"))

raw_dir  <- here("data", "raw", "listings")
out_dir  <- here("data", "processed")
rep_path <- here("reports", "data_quality.md")

# 1. Manifest and file integrity -------------------------------------------------

manifest <- read_csv(here("data", "raw", "show_manifest.csv"),
                     col_types = cols(.default = "c")) |>
  mutate(event_date = as.Date(event_date))

files <- list.files(raw_dir, pattern = "\\.csv$")
stopifnot(
  "Every raw file must be in the manifest" = all(files %in% manifest$file),
  "Every manifest file must exist"         = all(manifest$file %in% files)
)

# Detect byte-identical files and make sure each one is handled in the manifest:
# within every group of identical files exactly one is kept.
hashes <- tibble(file = files, md5 = unname(tools::md5sum(file.path(raw_dir, files))))
dupe_groups <- hashes |>
  group_by(md5) |> filter(n() > 1) |>
  left_join(manifest, by = "file") |>
  summarise(files = paste(file, collapse = " = "), kept = sum(status == "keep"), .groups = "drop")
if (any(dupe_groups$kept != 1)) {
  print(dupe_groups)
  stop("Identical raw files found that the manifest does not resolve (need exactly one 'keep' per group).")
}
unflagged <- manifest |> filter(status == "drop") |> anti_join(
  hashes |> group_by(md5) |> filter(n() > 1) |> ungroup(), by = "file")
if (nrow(unflagged) > 0) warning("Dropped files that are not exact duplicates: ", paste(unflagged$file, collapse = ", "))

shows <- manifest |>
  filter(status == "keep") |>
  arrange(event_date) |>
  mutate(show_id = str_remove(file, "\\.csv$")) |>
  group_by(city) |>
  mutate(n_shows_in_city = n(), city_night = row_number()) |>
  ungroup() |>
  mutate(weekday = weekdays(event_date)) |>
  select(show_id, city, country, event_date, weekday, n_shows_in_city, city_night, file, note)

# 2. Read and parse --------------------------------------------------------------

read_show <- function(file) {
  read_csv(file.path(raw_dir, file), col_types = cols(.default = "c"),
           na = c("", "NA")) |>
    rename(scraped_at_raw = 1) |>
    rename_with(str_to_lower) |>
    mutate(file = file)
}

raw <- map(shows$file, read_show) |> list_rbind()
n_raw <- nrow(raw)

listings <- raw |>
  left_join(shows |> select(file, show_id, city, country, event_date), by = "file") |>
  mutate(
    # Timestamps are recorded in US Eastern time, e.g. "07-29-23 / 07:05 PM"
    scraped_at  = as.POSIXct(scraped_at_raw, format = "%m-%d-%y / %I:%M %p", tz = "America/New_York"),
    price_date  = as.Date(scraped_at, tz = "America/New_York"),
    days_before = as.integer(event_date - price_date),
    qty         = as.integer(qty),
    price       = parse_number(price),
    zone        = str_squish(zone),
    section     = str_squish(section),
    row         = str_squish(row),
    seat_tier   = seat_tier(zone, section),
    vip_package = vip_package(zone, section, seat_tier),
    section_num = suppressWarnings(as.integer(str_extract(section, "\\d{3}")))
  )

# 3. Clean, counting what each step removes --------------------------------------

steps <- list()
log_step <- function(df, label) { steps[[label]] <<- nrow(df); df }

listings <- listings |>
  log_step("Rows read (kept shows)") |>
  filter(!is.na(scraped_at), !is.na(price), !is.na(qty)) |>
  log_step("After dropping unparseable timestamp/price/qty") |>
  distinct(show_id, scraped_at, zone, section, row, qty, price, .keep_all = TRUE) |>
  log_step("After removing exact duplicate rows within a show") |>
  filter(days_before >= 0) |>
  log_step("After removing listings scraped after the show date") |>
  filter(price > 0, qty > 0) |>
  log_step("After removing zero/negative price or qty")

listings <- listings |>
  arrange(event_date, show_id, scraped_at) |>
  mutate(listing_id = row_number()) |>
  select(listing_id, show_id, city, country, event_date, scraped_at, price_date,
         days_before, zone, section, section_num, row, seat_tier, vip_package, qty, price)

# 4. Validation --------------------------------------------------------------------

stopifnot(
  !anyNA(listings$show_id),
  all(listings$days_before >= 0),
  all(listings$price > 0),
  n_distinct(listings$show_id) == nrow(shows)
)

shows <- shows |>
  left_join(listings |> group_by(show_id) |>
              summarise(n_listings = n(),
                        first_scrape = min(price_date), last_scrape = max(price_date),
                        days_with_data = n_distinct(price_date),
                        median_price = median(price),
                        pct_other_tier = round(100 * mean(seat_tier == "Other"), 1),
                        .groups = "drop"),
            by = "show_id") |>
  mutate(gap_before_show = as.integer(event_date - last_scrape))

# 5. Write outputs -----------------------------------------------------------------

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(listings, file.path(out_dir, "listings.rds"))
write_csv(listings, file.path(out_dir, "listings.csv"))
write_csv(shows |> select(-file), file.path(out_dir, "shows.csv"))

# 6. Data quality report -------------------------------------------------------------

md_table <- function(df) {
  df <- mutate(df, across(everything(), \(x) replace_na(as.character(x), "")))
  c(paste("|", paste(names(df), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|"),
    apply(df, 1, \(r) paste("|", paste(r, collapse = " | "), "|")))
}

dropped <- manifest |> filter(status == "drop")
dropped_rows <- hashes |> filter(file %in% dropped$file) |>
  mutate(rows = map_int(file, \(f) length(read_lines(file.path(raw_dir, f))) - 1L))

step_tbl <- tibble(step = names(steps), rows = unlist(steps)) |>
  mutate(removed = lag(rows) - rows)

other_tbl <- listings |> filter(seat_tier == "Other") |>
  count(show_id, zone, section, sort = TRUE) |> head(15)

report <- c(
  "# Data quality report",
  "",
  sprintf("Generated by `R/01_build_data.R` on %s.", format(Sys.Date())),
  "",
  "## Raw files",
  "",
  sprintf("- %d raw per-show files, %d rows in total.", length(files),
          sum(map_int(files, \(f) length(read_lines(file.path(raw_dir, f))) - 1L))),
  sprintf("- %d files are byte-identical copies of another show and were dropped (%d rows).",
          nrow(dropped), sum(dropped_rows$rows)),
  sprintf("- %d shows remain across %d cities.", nrow(shows), n_distinct(shows$city)),
  "",
  "The copy in each pair was identified by the last scrape date (scrapes stop on show day)",
  "and by venue-specific zone names. The shows below have **no usable data**:",
  "",
  md_table(dropped |> transmute(file, city, event_date, `copy of` = duplicate_of)),
  "",
  "## Cleaning steps",
  "",
  md_table(step_tbl),
  "",
  "## Shows",
  "",
  md_table(shows |> transmute(show_id, city, event_date, weekday, n_listings,
                              first_scrape, last_scrape, gap_before_show,
                              median_price, pct_other_tier)),
  "",
  "## Seat tiers",
  "",
  md_table(listings |> count(seat_tier) |> mutate(pct = round(100 * n / sum(n), 1))),
  "",
  "VIP packages (each gets its own price level in the model):",
  "",
  md_table(listings |> filter(seat_tier == "VIP") |> group_by(vip_package) |>
             summarise(n = n(), shows = n_distinct(show_id), median_price = median(price)) |> droplevels()),
  "",
  "Most common zone/section combinations still classed as `Other`:",
  "",
  if (nrow(other_tbl)) md_table(other_tbl) else "_None._",
  "",
  "## Caveats",
  "",
  "- Rows appear to be **completed resale transactions** (96% of section/row/qty/price combinations occur once, at irregular times), not listing snapshots. Price is assumed to be per ticket.",
  "- Toronto and Vancouver prices may be in CAD; the source export doesn't say. Check before comparing them with US shows.",
  "- Nashville's scrape ends 3 days before the show.",
  "- Sales within a show share venue, date and demand. Validate models by holding out whole shows, not random rows."
)
writeLines(report, rep_path)

message(sprintf("Done: %d raw rows -> %d listings across %d shows. Report: %s",
                n_raw, nrow(listings), nrow(shows), rep_path))
