# Download matched Creative Commons vehicle photographs and attribution metadata
# from Wikimedia Commons into images/ and data/processed/vehicle_images.csv
library(tidyverse)
library(httr2)
library(jsonlite)

dir.create("images", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

ua <- "CarExplorerStudentProject/1.0 (https://github.com/mt-0708/Car-Explorer)"

# Representative 2024-2027 models across all 5 powertrains and major body styles
targets <- tribble(
  ~make,          ~model_prefix, ~wiki_title,
  "Toyota",       "Camry",       "Toyota Camry (XV80)",
  "Honda",        "Civic",       "Honda Civic (eleventh generation)",
  "Ford",         "F150",        "Ford F-Series (fourteenth generation)",
  "Chevrolet",    "Corvette",    "Chevrolet Corvette (C8)",
  "Tesla",        "Model 3",     "Tesla Model 3",
  "Hyundai",      "Ioniq 5",     "Hyundai Ioniq 5",
  "Rivian",       "R1S",         "Rivian R1S",
  "Lucid",        "Air",         "Lucid Air",
  "Porsche",      "911",         "Porsche 992",
  "BMW",          "M3",          "BMW M3 (G80)",
  "Subaru",       "Outback",     "Subaru Outback",
  "Toyota",       "Prius",       "Toyota Prius (XW60)",
  "Mazda",        "MX-5",        "Mazda MX-5 (ND)",
  "Jeep",         "Wrangler",    "Jeep Wrangler (JL)",
  "Kia",          "EV9",         "Kia EV9",
  "Alfa Romeo",   "Tonale",      "Alfa Romeo Tonale"
)

strip_html <- \(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  str_squish(str_replace_all(x, "<[^>]+>", ""))
}

fetch_commons_photo <- function(make, model_prefix, wiki_title) {
  slug <- str_to_lower(str_replace_all(paste(make, model_prefix), "[^a-zA-Z0-9]+", "_"))

  # Step 1: Get curated lead image filename from English Wikipedia article
  wp_resp <- request("https://en.wikipedia.org/w/api.php") |>
    req_user_agent(ua) |>
    req_url_query(
      action = "query",
      titles = wiki_title,
      prop = "pageimages",
      piprop = "name",
      redirects = 1,
      format = "json"
    ) |>
    req_perform() |>
    resp_body_json()

  pages <- wp_resp$query$pages
  page_img <- pages[[1]]$pageimage
  if (is.null(page_img)) return(NULL)

  # Step 2: Query Wikimedia Commons for 640px thumbnail URL + license/artist metadata
  cm_resp <- request("https://commons.wikimedia.org/w/api.php") |>
    req_user_agent(ua) |>
    req_url_query(
      action = "query",
      titles = paste0("File:", page_img),
      prop = "imageinfo",
      iiprop = "url|extmetadata",
      iiurlwidth = 640,
      format = "json"
    ) |>
    req_perform() |>
    resp_body_json()

  cpage <- cm_resp$query$pages[[1]]
  if (is.null(cpage$imageinfo)) return(NULL)

  ii <- cpage$imageinfo[[1]]
  meta <- ii$extmetadata
  thumb_url <- ii$thumburl %||% ii$url
  ext <- str_to_lower(tools::file_ext(str_remove(thumb_url, "\\?.*$")))
  if (!ext %in% c("jpg", "jpeg", "png", "webp")) ext <- "jpg"

  local_path <- file.path("images", paste0(slug, ".", ext))
  if (!file.exists(local_path)) {
    request(thumb_url) |>
      req_user_agent(ua) |>
      req_perform(path = local_path)
    Sys.sleep(0.25)
  }

  tibble(
    make = make,
    model_prefix = model_prefix,
    local_image = local_path,
    commons_file = page_img,
    source_url = ii$descriptionurl %||% NA_character_,
    photographer = strip_html(meta$Artist$value %||% "Wikimedia Commons contributor"),
    license = strip_html(meta$LicenseShortName$value %||% "Creative Commons"),
    license_url = meta$LicenseUrl$value %||% NA_character_
  )
}

results <- pmap(targets, fetch_commons_photo) |> list_rbind()
write_csv(results, "data/processed/vehicle_images.csv")
cat("Downloaded", nrow(results), "matched vehicle photos with attribution!\n")
print(results |> select(make, model_prefix, local_image, photographer, license))
