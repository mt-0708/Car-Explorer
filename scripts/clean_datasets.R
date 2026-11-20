# Clean each of the three raw datasets separately into data/processed/
library(tidyverse)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# 1. Clean EPA Automotive Trends (MY 2024 detailed certification data)
epa_raw <- read_csv(
  "data/raw/epa_automotive_trends.csv",
  na = c("", "NA", "N/A"),
  show_col_types = FALSE,
  guess_max = 5000
)

epa_clean <- epa_raw |>
  transmute(
    year = as.integer(MODEL_YEAR),
    manufacturer_group = str_squish(`CAFE Mfr Name`),
    make = str_squish(MFR_DIVISION_SHORT_NM),
    model = str_squish(CARLINE_NAME),
    vehicle_class = str_squish(CARLINE_CLASS_DESC),
    compliance_category = str_squish(`Compliance Category Desc`),
    drive_system = str_squish(DRIVE_SYSTEM_DESC),
    transmission = str_squish(TRANS_TYPE_DESC),
    fuel_type = str_squish(FUEL_USAGE_DESC),
    cylinders = as.numeric(NUM_CYLINDRS_ROTORS),
    displacement_l = as.numeric(ENG_DISPL),
    horsepower = as.numeric(ENG_RATED_HP),
    mpg_city = as.numeric(MFR_CITY_FE_LABEL),
    mpg_highway = as.numeric(MFR_HWY_FE_LABEL),
    mpg_combined = as.numeric(MFR_COMB_FE_LABEL),
    co2_gpm = as.numeric(`EPA CAFE MT Calc - Comb GHG`),
    inertia_weight_lbs = as.numeric(`CAFE Base Level Inertia Weight`),
    production_volume = as.numeric(CALC_GHG_PROD_VOLUME)
  )

write_csv(epa_clean, "data/processed/epa_clean.csv")

# 1b. Clean EPA Historical Automotive Trends Summary (1975-2025 uploaded export)
if (file.exists("data/raw/table_export.csv")) {
  epa_trends_raw <- read_csv(
    "data/raw/table_export.csv",
    col_types = cols(.default = "c"),
    na = c("", "-", "NA", "N/A")
  )

  epa_trends_clean <- epa_trends_raw |>
    transmute(
      year = as.integer(str_extract(`Model Year`, "\\d{4}")),
      is_preliminary = str_detect(`Model Year`, "Prelim"),
      regulatory_class = str_squish(`Regulatory Class`),
      vehicle_type = str_squish(`Vehicle Type`),
      production_share = as.numeric(`Production Share`),
      mpg_real_world = as.numeric(`Real-World MPG`),
      mpg_city = as.numeric(`Real-World MPG_City`),
      mpg_highway = as.numeric(`Real-World MPG_Hwy`),
      weight_lbs = as.numeric(`Weight (lbs)`),
      horsepower = as.numeric(`Horsepower (HP)`),
      footprint_sqft = as.numeric(`Footprint (sq. ft.)`)
    )

  write_csv(epa_trends_clean, "data/processed/epa_trends_clean.csv")
}

# 2. Clean Original Cars MSRP Dataset (GitHub Gist)
msrp_raw <- read_csv(
  "data/raw/cars_msrp_kaggle.csv",
  na = c("", "NA", "N/A", "UNKNOWN"),
  show_col_types = FALSE
)

msrp_clean <- msrp_raw |>
  transmute(
    year = as.integer(Year),
    make = str_squish(Make),
    model = str_squish(Model),
    fuel_type = str_squish(`Engine Fuel Type`),
    horsepower = as.numeric(`Engine HP`),
    cylinders = as.numeric(`Engine Cylinders`),
    transmission = str_squish(`Transmission Type`),
    drivetrain = str_squish(Driven_Wheels),
    doors = as.integer(`Number of Doors`),
    market_category = str_squish(`Market Category`),
    vehicle_size = str_squish(`Vehicle Size`),
    body_style = str_squish(`Vehicle Style`),
    mpg_city = as.numeric(`city mpg`),
    mpg_highway = as.numeric(`highway MPG`),
    popularity = as.numeric(Popularity),
    msrp = as.numeric(MSRP)
  )

write_csv(msrp_clean, "data/processed/msrp_clean.csv")

# 3. Clean Trimslate Vehicle Specifications (read as text first so sparse columns aren't dropped)
trimslate_raw <- read_csv(
  "data/raw/trimslate_specs.csv",
  col_types = cols(.default = "c"),
  na = c("", "NA", "N/A")
)

trimslate_clean <- trimslate_raw |>
  transmute(
    year = as.integer(year),
    make = str_squish(make),
    model = str_squish(model),
    trim = str_squish(trim),
    body_style = str_squish(body_style),
    powertrain = str_squish(powertrain),
    fuel_type = str_squish(fuel_type),
    drivetrain = str_squish(drivetrain),
    transmission = str_squish(transmission),
    cylinders = as.numeric(cylinders),
    aspiration = str_squish(aspiration),
    horsepower = as.numeric(horsepower),
    torque = as.numeric(torque),
    msrp = as.numeric(msrp),
    mpg_city = as.numeric(mpg_city),
    mpg_highway = as.numeric(mpg_highway),
    mpg_combined = as.numeric(comb_mpg),
    epa_rated = as.logical(epa_rated),
    electric_range = as.numeric(electric_range),
    charge_240v_hours = as.numeric(charge_240v_hours),
    annual_fuel_cost = as.numeric(annual_fuel_cost),
    co2_gpm = as.numeric(co2_gpm),
    eco_score = as.numeric(eco_score),
    start_stop = as.logical(start_stop),
    confirmed = as.logical(confirmed)
  )

write_csv(trimslate_clean, "data/processed/trimslate_clean.csv")

message("Saved cleaned datasets to data/processed/")
