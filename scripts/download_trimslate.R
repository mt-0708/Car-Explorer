# Download Trimslate Vehicle Specifications CSV into data/raw/
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

trimslate_url <- "https://trimslate.com/data/export"
trimslate_dest <- "data/raw/trimslate_specs.csv"

if (!file.exists(trimslate_dest)) {
  download.file(trimslate_url, destfile = trimslate_dest, mode = "wb")
  message("Downloaded Trimslate dataset to ", trimslate_dest)
} else {
  message("Trimslate dataset already exists at ", trimslate_dest)
}
