#' Reformat FIPS codes
#'
#' This function reformats a vector of FIPS codes to a standard 5-digit format
#' or the NHGIS format. It can handle various input formats, including numeric
#' FIPS, character FIPS with or without leading zeros, and NHGIS formatted FIPS codes.
#'
#' @param fips_vector A vector of FIPS codes to reformat.
#' @param output_format The desired output format. Can be "standard" (for 5-digit FIPS)
#'   or "nhgis". Defaults to "standard".
#'
#' @return A character vector of reformatted FIPS codes.
#'
#' @importFrom stringr str_c str_sub str_pad str_starts
#' @importFrom dplyr case_when
#'
#' @export
#'
#' @examples
#' reformat_fips(c(1001, "01001", "G010010"))
#' reformat_fips(c(1001, "01001", "G010010"), output_format = "nhgis")
reformat_fips <- function(fips_vector, output_format = "standard") {

  # Ensure input is character
  fips_vector <- as.character(fips_vector)

  # First, standardize to 5-digit FIPS
  standard_fips <- dplyr::case_when(
    stringr::str_starts(fips_vector, "G") ~ stringr::str_c(stringr::str_sub(fips_vector, 2, 3), stringr::str_sub(fips_vector, 5, 7)),
    !stringr::str_starts(fips_vector, "G") ~ stringr::str_pad(fips_vector, 5, pad = "0"),
    TRUE ~ fips_vector
  )

  # Now, format to the desired output format
  if (output_format == "nhgis") {
    output_fips <- stringr::str_c("G", stringr::str_sub(standard_fips, 1, 2), "0", stringr::str_sub(standard_fips, 3, 5), "0")
  } else {
    output_fips <- standard_fips
  }

  return(output_fips)
}
