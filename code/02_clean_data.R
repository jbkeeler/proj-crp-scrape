# Clean the FSA county-level CRP history workbook into a long county x FY panel.
#
# Input: data/raw/CRPHistoryCounty86-25.xlsx, three wide sheets (one column per FY)
#   ACRES   - cumulative acres under contract at the end of each FY, 1986-2025
#   RENT    - rental payments ($), columns 1987-2026. Footnote 1: "Payments
#             made in fiscal year following performance", so the 2026 column
#             pays for acres under contract in FY2025.
#   AVERAGE - average rental payment ($/acre), 1986-2025 (= RENT[t+1] / ACRES[t])
#
# Output: data/processed/crp_county_fy.{rds,csv}, one row per county x FY:
#   state, state_abbr, state_fips, county, fips, fips_current, county_current,
#   statewide_record, fiscal_year, acres, rent_payment, payment_fy,
#   avg_rent_per_acre
#
# Things to know about the data:
#   * fiscal_year is the performance year. rent_payment for FY t comes from
#     the RENT column t + 1 (kept as payment_fy).
#   * Shannon County, SD (46113) became Oglala Lakota (46102) in 2015, but
#     FSA reports the whole series under 46113. `fips_current` and
#     `county_current` hold the post-2015 code and name for merges with
#     current Census or NASS data. (FSA's footnote says "38102", which is a
#     typo.)
#   * Alaska (02999) and Puerto Rico (72999) are reported as one statewide
#     record ("ALL"); `statewide_record` flags them.
#   * 41 Virginia independent cities are NA for 1986-2014 (not reported
#     separately before FY2015). They are left NA, not set to zero.
#   * avg_rent_per_acre is NA where acres = 0.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# ── Read one wide sheet into long form ─────────────────────────────────────
read_crp_sheet <- function(path, sheet, value_name) {
  raw <- suppressMessages(
    read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")
  )

  header_row <- which(raw[[1]] == "STATE")
  stopifnot(length(header_row) == 1)
  header <- unlist(raw[header_row, ], use.names = FALSE)
  stopifnot(identical(header[1:3], c("STATE", "COUNTY", "FIPS")),
            all(grepl("^\\d{4}(\\.0)?$", header[-(1:3)])))
  names(raw) <- c("state", "county", "fips", sub("\\.0$", "", header[-(1:3)]))

  # Every data row has a numeric FIPS. Titles, blank rows, footnotes, and the
  # TOTALS row don't.
  body   <- filter(raw, str_detect(fips, "^\\d+$"))
  totals <- filter(raw, state == "TOTALS:")

  long <- body |>
    pivot_longer(-c(state, county, fips), names_to = "year",
                 values_to = value_name) |>
    mutate(year = as.integer(year),
           "{value_name}" := as.numeric(.data[[value_name]]))

  # Sanity check: county rows sum to the sheet's TOTALS row (ACRES and RENT
  # have one, AVERAGE does not).
  if (nrow(totals) == 1) {
    tot <- totals |>
      select(-c(state, county, fips)) |>
      pivot_longer(everything(), names_to = "year", values_to = "total") |>
      mutate(year = as.integer(year), total = as.numeric(total))
    chk <- long |>
      summarise(sum = sum(.data[[value_name]], na.rm = TRUE), .by = year) |>
      inner_join(tot, by = "year")
    bad <- filter(chk, abs(sum - total) > 1)
    if (nrow(bad) > 0) {
      warning(sheet, ": county sums differ from TOTALS in years ",
              paste(bad$year, collapse = ", "))
    }
  }

  long
}

# ── Read all three sheets ──────────────────────────────────────────────────
acres <- read_crp_sheet(CRP_COUNTY_RAW, "ACRES",   "acres")
rent  <- read_crp_sheet(CRP_COUNTY_RAW, "RENT",    "rent_payment")
avg   <- read_crp_sheet(CRP_COUNTY_RAW, "AVERAGE", "avg_rent_per_acre")

# All three sheets must cover the same set of counties.
ids <- function(d) distinct(d, state, county, fips) |> arrange(fips)
stopifnot(identical(ids(acres), ids(rent)), identical(ids(acres), ids(avg)))

# ── Align to performance year and join ─────────────────────────────────────
rent <- rent |>
  rename(payment_fy = year) |>
  mutate(year = payment_fy - 1L)

crp <- acres |>
  full_join(select(rent, fips, year, rent_payment, payment_fy),
            by = c("fips", "year")) |>
  full_join(select(avg, fips, year, avg_rent_per_acre),
            by = c("fips", "year")) |>
  rename(fiscal_year = year)

stopifnot(!anyNA(crp$state), !anyDuplicated(crp[c("fips", "fiscal_year")]))

# Check the timing: AVERAGE should equal RENT[t+1] / ACRES[t]. Early-year
# averages are unrounded and later ones round to cents, so allow 1 cent.
mismatch <- crp |>
  filter(acres > 0, !is.na(avg_rent_per_acre)) |>
  filter(abs(rent_payment / acres - avg_rent_per_acre) > 0.011)
if (nrow(mismatch) > 0) {
  warning(nrow(mismatch), " county-years where avg_rent_per_acre != ",
          "rent_payment / acres (more than 1 cent apart)")
}

# ── Tidy identifiers ───────────────────────────────────────────────────────
state_lookup <- tibble(
  state      = c(toupper(state.name), "PUERTO RICO"),
  state_abbr = c(state.abb, "PR")
)

crp <- crp |>
  mutate(
    county = str_squish(str_remove(county, "\\s*\\d+/$")),  # drop "2/" footnote markers
    fips   = str_pad(fips, 5, pad = "0"),
    state_fips       = str_sub(fips, 1, 2),
    statewide_record = county == "ALL",
    fips_current   = if_else(fips == "46113", "46102", fips),
    county_current = if_else(fips == "46113", "OGLALA LAKOTA", county)
  ) |>
  left_join(state_lookup, by = "state") |>
  select(state, state_abbr, state_fips, county, fips, fips_current,
         county_current, statewide_record, fiscal_year, acres, rent_payment,
         payment_fy, avg_rent_per_acre) |>
  arrange(fips, fiscal_year)

stopifnot(!anyNA(crp$state_abbr))

# ── Save ───────────────────────────────────────────────────────────────────
dir.create(DIR_PROCESSED, showWarnings = FALSE, recursive = TRUE)
saveRDS(crp, paste0(CRP_COUNTY_CLEAN, ".rds"))
write.csv(crp, paste0(CRP_COUNTY_CLEAN, ".csv"), row.names = FALSE, na = "")

message("Saved ", nrow(crp), " rows (", n_distinct(crp$fips), " counties x ",
        n_distinct(crp$fiscal_year), " FYs) to ", CRP_COUNTY_CLEAN, ".{rds,csv}")
