# Download EPA Automotive Trends Data into data/raw/
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

epa_url <- "https://www.epa.gov/system/files/other-files/2026-05/model-year-2024-fuel-economy-and-technology-data.csv"
epa_dest <- "data/raw/epa_automotive_trends.csv"

if (!file.exists(epa_dest)) {
  download.file(epa_url, destfile = epa_dest, mode = "wb")
  message("Downloaded EPA dataset to ", epa_dest)
} else {
  message("EPA dataset already exists at ", epa_dest)
}
