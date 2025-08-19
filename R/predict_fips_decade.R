#' Predict the decade/year of FIPS codes based on presence patterns
#'
#' @param fips_codes A vector of FIPS codes to analyze
#' @param crosswalk A dataframe containing FIPS code changes with year_from and year_to columns
#' @param decades Optional vector of decades to analyze (default: seq(1970, 2020, 10))
#'
#' @return A list containing:
#'   - decade_percentages: percentage of FIPS codes present in each decade
#'   - most_likely_decade: the decade with highest percentage match
#'   - confidence: the percentage match for the most likely decade
#'   - analysis_summary: detailed breakdown by decade
#'
#' @importFrom dplyr filter pull
#' @importFrom stringr str_pad str_replace
#'
#' @export
predict_fips_decade <- function(fips_codes, crosswalk, decades = seq(1970, 2020, 10)) {
  
  # Input validation
  if (is.null(crosswalk)) {
    stop("A crosswalk dataframe must be provided")
  }
  
  if (length(fips_codes) == 0) {
    stop("At least one FIPS code must be provided")
  }
  
  # Standardize FIPS codes to 5-digit format
  fips_codes <- as.character(fips_codes)
  fips_codes <- stringr::str_pad(fips_codes, 5, pad = "0")
  
  # Remove any NHGIS formatted codes (starting with G)
  fips_codes <- stringr::str_replace(fips_codes, "^G(\\d{2})0(\\d{3})0?$", "\\1\\2")
  
  total_fips <- length(unique(fips_codes))
  
  # Analyze presence in each decade
  decade_analysis <- list()
  
  for (decade in decades) {
    # Find FIPS codes that existed during this decade
    decade_start <- decade
    decade_end <- decade + 9
    
    # Get all FIPS codes that were valid during this decade
    valid_fips <- crosswalk %>%
      dplyr::filter(year_from <= decade_end, year_to >= decade_start) %>%
      dplyr::pull(fips_from) %>%
      unique()
    
    # Also check fips_to for codes that became valid in this period
    valid_fips_to <- crosswalk %>%
      dplyr::filter(year_from <= decade_end, year_to >= decade_start) %>%
      dplyr::pull(fips_to) %>%
      unique()
    
    all_valid_fips <- unique(c(valid_fips, valid_fips_to))
    
    # Count matches
    matches <- sum(fips_codes %in% all_valid_fips)
    percentage <- round((matches / total_fips) * 100, 2)
    
    decade_analysis[[as.character(decade)]] <- list(
      decade = decade,
      matches = matches,
      total = total_fips,
      percentage = percentage,
      valid_fips_count = length(all_valid_fips)
    )
  }
  
  # Convert to data frame for easier handling
  results_df <- do.call(rbind, lapply(decade_analysis, function(x) {
    data.frame(
      decade = x$decade,
      matches = x$matches,
      total = x$total,
      percentage = x$percentage,
      valid_fips_count = x$valid_fips_count
    )
  }))
  
  # Find the most likely decade
  best_match <- results_df[which.max(results_df$percentage), ]
  
  # Create percentage summary
  decade_percentages <- setNames(results_df$percentage, paste0(results_df$decade, "s"))
  
  return(list(
    decade_percentages = decade_percentages,
    most_likely_decade = best_match$decade,
    confidence = best_match$percentage,
    analysis_summary = results_df,
    input_fips_count = total_fips
  ))
}