library(fipsharmonizer)
library(readr)
library(dplyr)

if (file.exists("cbp_employment.rds")) {
  cbp_data <- readRDS("cbp_employment.rds")
} else {
  stop("Error: cbp_employment.rds not found. Please ensure it is in the project root.")
}

cbp_data$decade <- as.integer(floor(cbp_data$year / 10) * 10)

crosswalk_1970 <- build_crosswalk(1970, 2020, FALSE)
crosswalk_1980 <- build_crosswalk(1980, 2020, FALSE)
crosswalk_1990 <- build_crosswalk(1990, 2020, FALSE)
crosswalk_2000 <- build_crosswalk(2000, 2020, FALSE)
crosswalk_2010 <- build_crosswalk(2010, 2020, FALSE)

cbp_1970 <- cbp_data %>% filter(decade == 1970)
cbp_1980 <- cbp_data %>% filter(decade == 1980)
cbp_1990 <- cbp_data %>% filter(decade == 1990)
cbp_2000 <- cbp_data %>% filter(decade == 2000)
cbp_2010 <- cbp_data %>% filter(decade == 2010)

data_1970 <- harmonize_fips(
  df = cbp_1970,
  fips_col = "fips",
  year = 1970,
  target_year = 2020,
  weight_vars = "emp_all",
  agg_vars = "fipstate",
  crosswalk = crosswalk_1970 %>% rename(weight = m2_weight)
)

data_1980 <- harmonize_fips(
  df = cbp_1980,
  fips_col = "fips",
  year = 1980,
  target_year = 2020,
  weight_vars = "emp_all",
  agg_vars = "fipstate",
  crosswalk = crosswalk_1980 %>% rename(weight = m2_weight)
)

data_1990 <- harmonize_fips(
  df = cbp_1990,
  fips_col = "fips",
  year = 1990,
  target_year = 2020,
  weight_vars = "emp_all",
  agg_vars = "fipstate",
  crosswalk = crosswalk_1990 %>% rename(weight = m2_weight)
)

data_2000 <- harmonize_fips(
  df = cbp_2000,
  fips_col = "fips",
  year = 2000,
  target_year = 2020,
  weight_vars = "emp_all",
  agg_vars = "fipstate",
  crosswalk = crosswalk_2000 %>% rename(weight = m2_weight)
)

data_2010 <- harmonize_fips(
  df = cbp_2010,
  fips_col = "fips",
  year = 2010,
  target_year = 2020,
  weight_vars = "emp_all",
  agg_vars = "fipstate",
  crosswalk = crosswalk_2010 %>% rename(weight = m2_weight),
  na.rm = T
)
