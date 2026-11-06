# Seat-tier classification ------------------------------------------------------
#
# Zone names are venue-specific (a "200 Level" seat at Rogers Centre is not the
# same as one at SoFi), so this is a first-pass, rule-based mapping into tiers
# that are comparable across stadiums. Rules are checked in order; the first
# match wins. Anything unmatched falls back to the section number, then "Other".
# The data quality report lists the "Other" rows per show so the rules can be
# refined.

library(stringr)

SEAT_TIERS <- c("VIP", "Floor", "Lower", "Club/Middle", "Upper", "Suite", "Other")

seat_tier <- function(zone, section) {
  z <- str_to_lower(str_squish(coalesce(zone, "")))
  s <- str_to_lower(str_squish(coalesce(section, "")))
  z <- if_else(z == "", s, z)  # a few rows have no zone but a descriptive section
  zs <- paste(z, s)
  sec_num <- suppressWarnings(as.integer(str_extract(s, "\\d{3}")))

  tier <- case_when(
    # Tour-specific premium areas and the pit sit next to the stage. SoFi's own
    # "VIP Plaza/Lower Level" zones are numbered stadium sections, not tour VIP,
    # so they fall through to the Lower rule below.
    str_detect(zs, "club renaissance|clubrn|b-?hive|pure honey|riser|\\bvip\\b|\\bpit\\b") &
      !str_detect(z, "^vip (plaza|lower) level") ~ "VIP",
    str_detect(zs, "suite|loft|booth|executive|dream seats|72 club|champions lounge|terrace tables") ~ "Suite",
    # Stadium floor. Atlanta calls its floor exactly "Field Level" (sections A-N);
    # baseball parks use "Field Level Bases/Infield/..." for lower-bowl seats.
    str_detect(z, "^(field|floor|field level)( tbd)?$") |
      (str_detect(z, "^field") & !str_detect(z, "^field level")) |
      str_detect(s, "^(fld|fl-|floor|field)") ~ "Floor",
    str_detect(z, "upper|grandstand|terrace|concourse|outfield|\\b(3|4|5)00") ~ "Upper",
    str_detect(z, "club|middle|mezzanine|loge|\\b200") ~ "Club/Middle",
    str_detect(z, "lower|plaza|main|field level|premium|infield|bases|dugout|home plate|\\b100") ~ "Lower",
    !is.na(sec_num) & sec_num < 200 ~ "Lower",
    !is.na(sec_num) & sec_num < 300 ~ "Club/Middle",
    !is.na(sec_num) ~ "Upper",
    TRUE ~ "Other"
  )
  factor(tier, levels = SEAT_TIERS)
}

# VIP packages are different products (Club Renaissance hospitality vs. floor
# risers vs. front-of-stage VIP sections), priced up to 4x apart. Keep them in
# one VIP tier for the timing curve, but give each package its own price level.
VIP_PACKAGES <- c("Not VIP", "Club Renaissance", "B-Hive", "Risers", "VIP floor")

vip_package <- function(zone, section, tier) {
  zs <- str_to_lower(paste(coalesce(zone, ""), coalesce(section, "")))
  pkg <- case_when(
    tier != "VIP" ~ "Not VIP",
    str_detect(zs, "club renaissance|clubrn") ~ "Club Renaissance",
    str_detect(zs, "b-?hive") ~ "B-Hive",
    str_detect(zs, "pure honey|riser") ~ "Risers",
    TRUE ~ "VIP floor"   # VIP A/B, Field VIP, pit, Silver VIP Floor
  )
  factor(pkg, levels = VIP_PACKAGES)
}
