library(dplyr)

source("R/predict_fips_decade.R")

library(fipsharmonizer)
crosswalk <- build_crosswalk(1920, 2020, FALSE)

test_fips_1 <- c("01001", "01003", "06001", "36001", "48001")
cat("Testing with FIPS codes:", paste(test_fips_1, collapse = ", "), "\n")

result1 <- predict_fips_decade(test_fips_1, crosswalk)
cat("Decade percentages:\n")
print(result1$decade_percentages)
cat("Most likely decade:", result1$most_likely_decade, "with", result1$confidence, "% confidence\n\n")

# Example 2: Test with Connecticut FIPS codes (which changed in 2020)
ct_fips <- c("09001", "09003", "09005", "09007", "09009")
cat("Testing with Connecticut FIPS codes:", paste(ct_fips, collapse = ", "), "\n")

result2 <- predict_fips_decade(ct_fips, crosswalk)
cat("Decade percentages:\n")
print(result2$decade_percentages)
cat("Most likely decade:", result2$most_likely_decade, "with", result2$confidence, "% confidence\n\n")

# Example 3: Test with Alaska FIPS codes (which have many changes)
ak_fips <- c("02105", "02195", "02201", "02232")
cat("Testing with Alaska FIPS codes:", paste(ak_fips, collapse = ", "), "\n")

result3 <- predict_fips_decade(ak_fips, crosswalk)
cat("Decade percentages:\n")
print(result3$decade_percentages)
cat("Most likely decade:", result3$most_likely_decade, "with", result3$confidence, "% confidence\n\n")

# Display detailed analysis for the first example
cat("Detailed analysis for first example:\n")
print(result3$analysis_summary)
