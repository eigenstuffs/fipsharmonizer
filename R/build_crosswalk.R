#' Build a Historical FIPS Crosswalk
#'
#' This function constructs a comprehensive FIPS code crosswalk by combining
#' individual crosswalk files from a specified start year to a target year.
#' It assumes a specific directory structure where the source CSV files are located.
#'
#' @param year_from The starting year for the crosswalk (e.g., 1970).
#' @param year_to The target year for the crosswalk (e.g., 2020).
#' @param endpoints_only A logical value. If TRUE, only the single crosswalk for
#'   the specified `year_from` to `year_to` is loaded. If FALSE (the default),
#'   it will load and combine all decadal crosswalks between the start and
#'   target years.
#'
#' @return A single data frame containing the combined crosswalk data with
#'   harmonized column names ('fips_from', 'fips_to').
#'
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows mutate select rename contains across
#' @importFrom stringr str_c str_sub str_pad
#' @importFrom rlang sym
#'
#' @export
build_crosswalk <- function(year_from, year_to, endpoints_only = FALSE) {
  if (endpoints_only) {
    years <- c(year_from)
  } else {
    years <- seq(year_from, year_to - 10, by = 10)
  }

  crosswalk_list <- lapply(years, function(year) {
    file_path <- paste0("data/input/crosswalks/crosswalks/CountyToCounty/", year_to, "/", year_to, "_csv/Crosswalk_", year, "_", year_to, ".csv")
    
    if (!file.exists(file_path)){
        stop(paste("Required crosswalk file not found:", file_path))
    }

    df <- readr::read_csv(file_path, show_col_types = FALSE)

    df %>%
      dplyr::mutate(year_from = year, year_to = year_to) %>%
      dplyr::rename(fips_from = !!rlang::sym(paste0("gisjoin_", year)))
  })

  dplyr::bind_rows(crosswalk_list) %>%
    dplyr::select(dplyr::contains("gis"), dplyr::contains("fips"), dplyr::contains("weight"), dplyr::contains("year")) %>%
    dplyr::rename(fips_to = !!rlang::sym(paste0("gisjoin_", year_to))) %>%
    dplyr::mutate(
      fips_from = stringr::str_c(stringr::str_sub(fips_from, 2, 3), stringr::str_sub(fips_from, 5, 7)),
      fips_to = stringr::str_pad(as.character(fips_to), 5, pad = "0")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("fips"), as.character))
}
