#' Harmonize FIPS codes across time periods
#'
#' @param df A dataframe containing FIPS codes
#' @param fips_col The name of the column containing FIPS codes
#' @param year The year the FIPS codes in the data are from
#' @param target_year The year to convert FIPS codes to (defaults to most recent)
#' @param weight_vars Optional vector of column names containing values to be weighted during aggregation
#' @param agg_vars Optional vector of column names to be aggregated (using sum for numeric, first for non-numeric)
#' @param group_vars Optional vector of column names to group by (besides FIPS) when aggregating
#' @param crosswalk A dataframe containing FIPS code changes (must be provided)
#' @param weight_type How to handle weights: "provided" (use weights in crosswalk) or "equal" (split equally)
#' @param handle_special_cases Logical; whether to apply special handling for cases like CT COGs (default TRUE)
#' @param output_format The format for the output FIPS column, either "standard" or "nhgis".
#' @param na.rm Whether to remove NAs in weighting, by default "TRUE"
#'
#' @return A dataframe with harmonized FIPS codes and appropriately weighted/aggregated values
#'
#' @importFrom dplyr mutate across all_of case_when left_join coalesce group_by summarise rename select first distinct
#' @importFrom rlang sym
#' @importFrom stringr str_c str_sub str_pad str_starts
#'
#' @export
harmonize_fips <- function(df, 
                          fips_col,
                          year,
                          target_year = NULL,
                          weight_vars = NULL,
                          agg_vars = NULL,
                          group_vars = NULL,
                          crosswalk = NULL,
                          weight_type = "provided",
                          handle_special_cases = TRUE,
                          output_format = "standard",
                          na.rm = T) {
  
  # Input validation
  if (is.null(crosswalk)) {
    stop("A crosswalk dataframe must be provided")
  }
  
  if (!fips_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in input dataframe", fips_col))
  }
  
  if (!is.null(weight_vars)) {
    missing_weights <- weight_vars[!weight_vars %in% names(df)]
    if (length(missing_weights) > 0) {
      stop(sprintf("Weight columns not found: %s", paste(missing_weights, collapse = ", ")))
    }
  }
  
  if (!is.null(agg_vars)) {
    missing_aggs <- agg_vars[!agg_vars %in% names(df)]
    if (length(missing_aggs) > 0) {
      stop(sprintf("Aggregation columns not found: %s", paste(missing_aggs, collapse = ", ")))
    }
  }
  
  if (!is.null(group_vars)) {
    missing_groups <- group_vars[!group_vars %in% names(df)]
    if (length(missing_groups) > 0) {
      stop(sprintf("Grouping columns not found: %s", paste(missing_groups, collapse = ", ")))
    }
  }
  
  # If target_year is NULL, use the most recent year in the crosswalk
  if (is.null(target_year)) {
    target_year <- max(crosswalk$year_to)
  }
  
  # Ensure FIPS codes are properly formatted character strings
  df <- df %>% 
    mutate(
      across(all_of(fips_col), function(x) {
        x <- as.character(x)
        case_when(
          str_starts(x, "G") ~ str_c(str_sub(x, 2, 3), str_sub(x, 5, 7)),
          !str_starts(x, "G") ~ str_pad(x, 5, pad = "0"),
          TRUE ~ x
        )
      })
    )
  
  # Special handling for Connecticut COGs if needed
  if (handle_special_cases) {
    # Flag CT counties
    df <- df %>%
      mutate(
        is_ct = substr(!!sym(fips_col), 1, 2) == "09"
      )
    
    # Add warning for CT counties in transition period
    if (any(df$is_ct) && year < 2020 && target_year >= 2020) {
      warning("Converting Connecticut counties to COGs. Consider handling this transition separately.")
    }
  }
  
  # Merge with crosswalk to get the new FIPS codes
  df_harmonized <- df %>%
    left_join(
      crosswalk %>%
        filter(year_from <= year,
               year_to >= target_year),
      by = setNames("fips_from", fips_col)
    )
  
  # For FIPS codes that didn't change, keep the original
  df_harmonized <- df_harmonized %>%
    mutate(
      fips_to = coalesce(fips_to, !!sym(fips_col)),
      weight = coalesce(weight, 1)
    )
  
  print("survived weights")
  
  # If weight_type is "equal", override weights for splits
  # if (weight_type == "equal") {
  #   df_harmonized <- df_harmonized %>%
  #     group_by(!!sym(fips_col)) %>%
  #     mutate(
  #       weight = 1 / n()
  #     ) %>%
  #     ungroup()
  # }
  
  # Handle weighted variables and aggregation
  if (!is.null(weight_vars) || !is.null(agg_vars)) {
    # Prepare grouping variables
    group_cols <- c("fips_to", group_vars)
    
    # Weight numeric variables
    if (!is.null(weight_vars)) {
      df_harmonized <- df_harmonized %>%
        mutate(
          across(all_of(weight_vars), ~ .x * weight)
        )
    }

    print("Survived weighting")
    
    # Aggregate by new FIPS code
    df_harmonized <- df_harmonized %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(
        # Sum weighted variables
        across(all_of(weight_vars), ~ sum(.x, na.rm = na.rm)),
        # Handle other aggregation variables
        across(all_of(setdiff(agg_vars, weight_vars)), 
               ~ if(is.numeric(.x)) sum(.x, na.rm = na.rm) else first(.x)),
        .groups = "drop"
      )
    
    print("Survived aggregation")
  }
  
  # Rename the FIPS column back to original name
  df_harmonized <- df_harmonized %>%
    rename(!!fips_col := fips_to)

  print("Survived renaming")
  
  # Add change type information if available
  if ("change_type" %in% names(crosswalk)) {
    df_harmonized <- df_harmonized %>%
      left_join(
        crosswalk %>%
          select(fips_from, fips_to, change_type) %>%
          distinct(),
        by = setNames("fips_to", fips_col)
      )
  }

  print("Survived change type")
  
  # Clean up special case flags
  if (handle_special_cases) {
    df_harmonized <- df_harmonized %>%
      select(-any_of(c("is_ct")))
  }
  
  # Format output FIPS based on user request
  if (output_format == "nhgis") {
    df_harmonized <- df_harmonized %>% 
      mutate(
        across(all_of(fips_col), ~ str_c("G", str_sub(., 1, 2), "0", str_sub(., 3, 5), "0"))
      )
  }
  
  return(df_harmonized)
}

# Example usage:
#' @examples
#' # Example with numeric FIPS codes
#' df <- data.frame(
#'   fips = c(9001, 9003, 9005),  # Numeric FIPS without leading zeros
#'   population = c(1000, 2000, 3000),
#'   county_name = c("Fairfield", "Hartford", "Litchfield"),
#'   state = c("CT", "CT", "CT")
#' )
#' 
#' result <- harmonize_fips(
#'   df = df,
#'   fips_col = "fips",
#'   year = 2019,
#'   target_year = 2023,
#'   weight_vars = "population",
#'   agg_vars = c("population", "county_name"),
#'   group_vars = "state",
#'   crosswalk = crosswalk
#' ) 