# Project-wide constants: file paths, parameters, shared settings.

NDSU_COLORS <- c(
ndsu_green  = "#00583D",
ndsu_yellow = "#FFC425",
ndsu_rust   = "#B83E27",
ndsu_night  = "#0F374B",
ndsu_teal   = "#51ABD0",
ndsu_sage   = "#8ABD78"
)


# ── CRP data sources ──────────────────────────────────────
# FSA serves each file through a landing page (/documents/<slug>); the real
# file URL is read off that page in 01_import_data.R.
CRP_STATS_PAGE      <- "https://www.fsa.usda.gov/tools/informational/reports/conservation-statistics/crp"
CRP_COUNTY_LANDING  <- "https://www.fsa.usda.gov/documents/crphistorycounty86-25xlsx"

DIR_RAW       <- "data/raw"
DIR_PROCESSED <- "data/processed"

CRP_COUNTY_RAW   <- file.path(DIR_RAW, "CRPHistoryCounty86-25.xlsx")
CRP_COUNTY_CLEAN <- file.path(DIR_PROCESSED, "crp_county_fy")  # .rds / .csv added on save
