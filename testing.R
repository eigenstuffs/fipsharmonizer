library(dplyr)
library(readr)
library(stringr)

build_crosswalk <- function(year_from, year_to, endpoints_only = FALSE) {
  if (endpoints_only) {
    years <- c(year_from)
  } else {
    years <- seq(year_from, year_to - 10, by = 10)
  }

  crosswalk_list <- lapply(years, function(year) {
    file_path <- paste0("data/input/crosswalks/crosswalks/CountyToCounty/", year_to, "/", year_to, "_csv/Crosswalk_", year, "_", year_to, ".csv")
    
    df <- read_csv(file_path, show_col_types = FALSE)
    
    df %>%
      mutate(year_from = year, year_to = year_to) %>%
      rename(fips_from = !!rlang::sym(paste0("gisjoin_", year)))
  })

  bind_rows(crosswalk_list) %>%
    select(contains("gis"), contains("fips"), contains("weight"), contains("year")) %>%
    rename(fips_to = !!rlang::sym(paste0("gisjoin_", year_to))) %>%
    mutate(
      fips_from = str_c(str_sub(fips_from, 2, 3), str_sub(fips_from, 5, 7)),
      fips_to = str_pad(as.character(fips_to), 5, pad = "0")
    ) %>%
    mutate(across(contains("fips"), as.character))
}

# examples
crosswalk_1970_2020_full <- build_crosswalk(1970, 2020)
crosswalk_1970_2020_endpoints <- build_crosswalk(1970, 2020, endpoints_only = TRUE)

nhgis_pop <- read_csv("data/input/nhgis/nhgis0001_ts_nominal_county.csv")


# the weights are calculated as follows (from crosswalks/readme.pdf)

# m1: an area-based model, equivalent in construction to existing area-based
# crosswalks.

# m2: a population-based model, with county area divided into urban and rur-
# al areas, based on historical population estimates from Fang and Jawitz
# (2018).

# m3: a population-based model, with county area divided into urban and
# rural areas after excluding non-inhabitable areas, based on historical popu-
# lation estimates from Fang and Jawitz (2018).

# m4: a population-based model, with county area divided into urban and rur-
# al areas after excluding non-inhabitable areas, with additional weighting for
# topographic suitability, based on historical population estimates from Fang
# and Jawitz (2018).

# m5: a population-based model, with built-up settlement areas indicated in
# space (1810–2020 only), based on Leyk et al (2020).

# m6: a population-based model, with built-up property counts indicated in
# space (1810–2020 only), based on Leyk et al (2020).


####


View(harmonize_fips(
  df = nhgis_pop,
  fips_col = "GISJOIN",
  year = 1970,
  target_year = 2020,
  weight_vars = "A00AA1970",
  group_vars = "STATE",
  crosswalk = crosswalk_1970_2020_full %>% rename(weight = m2_weight)
))

View(harmonize_fips(
  df = nhgis_pop,
  fips_col = "GISJOIN",
  year = 1970,
  target_year = 2020,
  weight_vars = "A00AA1970",
  group_vars = "STATE",
  crosswalk = crosswalk_1970_2020_full %>% rename(weight = m2_weight),
  output_format = "standard"
))

####

# data quality checks

# str(crosswalk_1970 %>% select(gisjoin_1970))
# head(crosswalk_1970 %>% select(gisjoin_1970))

# matched_data <- inner_join(
#   crosswalk_1970,
#   nhgis_pop,
#   by = c("gisjoin_1970" = "GISJOIN")
# )

# cor(matched_data$m1_weight, matched_data$m2_weight, use = "complete.obs")

# GISJOIN is the 1970 FIPS code
# gisjoin_2020 is the 2020 FIPS code