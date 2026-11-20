# Download Original Cars MSRP Dataset (GitHub Gist) into data/raw/
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

msrp_url <- "https://gist.githubusercontent.com/dvinubius/33486dad2dc86319f0a06ffdb1d4f34f/raw/a5d63cfb037dcc031c28d9ca62e381fa9bd294a0/cars_msrp_kaggle.csv"
msrp_dest <- "data/raw/cars_msrp_kaggle.csv"

if (!file.exists(msrp_dest)) {
  download.file(msrp_url, destfile = msrp_dest, mode = "wb")
  message("Downloaded Cars MSRP dataset to ", msrp_dest)
} else {
  message("Cars MSRP dataset already exists at ", msrp_dest)
}
