# Connect cleaned datasets without duplicating observations
library(tidyverse)

# Helper to normalize make and model strings for matching across sources
norm_key <- \(x) str_to_lower(str_replace_all(coalesce(x, ""), "[^a-zA-Z0-9]", ""))

msrp_clean <- read_csv("data/processed/msrp_clean.csv", show_col_types = FALSE)
trimslate_clean <- read_csv("data/processed/trimslate_clean.csv", show_col_types = FALSE)
epa_clean <- read_csv("data/processed/epa_clean.csv", show_col_types = FALSE)

# 1. Summarize Trimslate to ONE row per (year, make_key, model_key) to prevent join duplication
trimslate_by_model <- trimslate_clean |>
  mutate(make_key = norm_key(make), model_key = norm_key(model)) |>
  summarize(
    ts_annual_fuel_cost = median(annual_fuel_cost, na.rm = TRUE),
    ts_co2_gpm = median(co2_gpm, na.rm = TRUE),
    ts_mpg_combined = median(mpg_combined, na.rm = TRUE),
    ts_powertrain = first(na.omit(powertrain)),
    .by = c(year, make_key, model_key)
  )

# Build deduplicated MSRP analysis dataset (2001-2017 genuine new-vehicle MSRPs)
msrp_merged <- msrp_clean |>
  distinct() |>
  # Pre-2001 rows record $2,000 trade-in estimates rather than new-car sticker prices
  filter(year >= 2001, !is.na(horsepower), !is.na(msrp)) |>
  mutate(
    make_key = norm_key(make),
    model_key = norm_key(model),
    mpg_combined = round(1 / (0.55 / mpg_city + 0.45 / mpg_highway), 1),
    body_group = case_when(
      str_detect(body_style, "SUV") ~ "SUV",
      str_detect(body_style, "Pickup") ~ "Pickup",
      str_detect(body_style, "Hatchback") ~ "Hatchback",
      str_detect(body_style, "Minivan|Van") ~ "Van / Minivan",
      TRUE ~ body_style
    )
  ) |>
  left_join(
    trimslate_by_model,
    by = c("year", "make_key", "model_key"),
    relationship = "many-to-one"
  ) |>
  select(-make_key, -model_key)

write_csv(msrp_merged, "data/processed/msrp_merged.csv")

# 2. Summarize EPA MY 2024 data to ONE row per (year, make_key, model_key)
epa_by_model <- epa_clean |>
  mutate(make_key = norm_key(make), model_key = norm_key(model)) |>
  summarize(
    epa_horsepower = median(horsepower, na.rm = TRUE),
    epa_inertia_weight_lbs = median(inertia_weight_lbs, na.rm = TRUE),
    epa_displacement_l = median(displacement_l, na.rm = TRUE),
    epa_co2_gpm = median(co2_gpm, na.rm = TRUE),
    .by = c(year, make_key, model_key)
  )

# Build deduplicated 2022-2027 recent vehicles dataset (Trimslate + EPA MY 2024)
recent_vehicles <- trimslate_clean |>
  filter(year >= 2022, year <= 2027) |>
  distinct() |>
  mutate(
    make_key = norm_key(make),
    model_key = norm_key(model),
    body_group = case_when(
      str_detect(body_style, "Sport Utility|SUV") ~ "SUV",
      str_detect(body_style, "Pickup") ~ "Pickup",
      str_detect(body_style, "Minivan|Van") ~ "Van / Minivan",
      str_detect(body_style, "Wagon") ~ "Wagon",
      str_detect(body_style, "Two Seaters") ~ "Two-Seater / Sports",
      str_detect(body_style, "Cars") ~ "Sedan / Hatchback / Coupe",
      TRUE ~ "Other / Special Purpose"
    )
  ) |>
  left_join(
    epa_by_model,
    by = c("year", "make_key", "model_key"),
    relationship = "many-to-one"
  ) |>
  mutate(
    horsepower_combined = coalesce(horsepower, epa_horsepower),
    co2_combined = coalesce(co2_gpm, epa_co2_gpm)
  ) |>
  select(-make_key, -model_key)

write_csv(recent_vehicles, "data/processed/recent_vehicles_2022_2027.csv")

cat("msrp_merged rows:", nrow(msrp_merged), "\n")
cat("recent_vehicles_2022_2027 rows:", nrow(recent_vehicles),
    "| with HP:", sum(!is.na(recent_vehicles$horsepower_combined)),
    "| with MPG:", sum(!is.na(recent_vehicles$mpg_combined)), "\n")
