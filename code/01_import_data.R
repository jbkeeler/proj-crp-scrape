# Download the FSA "CRP Enrollment and Rental Payments by County, 1986-2025"
# workbook into data/raw.
#
# FSA links point at a Drupal landing page (/documents/<slug>), not the file.
# The landing page holds the real URL under /sites/default/files/<yyyy-mm>/,
# which changes whenever FSA re-uploads, so it is read fresh each time.
#
# Skips the download if the raw file already exists. Set
# options(crp.force_download = TRUE) to fetch it again.

library(httr2)
library(rvest)

resolve_fsa_file_url <- function(landing_url, ext = "xlsx") {
  page  <- read_html(landing_url)
  links <- html_attr(html_elements(page, "a[href]"), "href")
  hits  <- unique(grep(paste0("/sites/default/files/.*\\.", ext, "$"),
                       links, value = TRUE, ignore.case = TRUE))
  if (length(hits) != 1) {
    stop("Expected one .", ext, " link on ", landing_url, ", found ", length(hits))
  }
  url_absolute(hits, landing_url)
}

download_fsa_file <- function(landing_url, dest, ext = "xlsx",
                              force = getOption("crp.force_download", FALSE)) {
  if (file.exists(dest) && !force) {
    message("Using existing ", dest)
    return(invisible(dest))
  }
  file_url <- resolve_fsa_file_url(landing_url, ext)
  message("Downloading ", file_url)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  request(file_url) |>
    req_user_agent("NDSU ARPC research (R httr2)") |>
    req_retry(max_tries = 3) |>
    req_perform(path = dest)
  invisible(dest)
}

download_fsa_file(CRP_COUNTY_LANDING, CRP_COUNTY_RAW)
