# ========================================================
# R Script: Collapse multiple gharama columns into one (skip column 4)
# ========================================================

library(dplyr)
library(readxl)

# 1. Load data
data <- read_excel("Documents/WORKS/MERGIE/BAGAMOYO/data_community.xlsx")

# 2. Identify gharama multiple-choice columns (1–4 exist, we drop 4)
gharama_cols <- grep("^nani_anahusika_na_gharama___", colnames(data), value = TRUE)
gharama_cols <- setdiff(gharama_cols, "nani_anahusika_na_gharama___4")  # skip col 4

# 3. Collapse numeric + free-text into single column
data$gharama <- apply(data[, c(gharama_cols, "mwingineyo")], 1, function(x) {
  x <- x[!is.na(x) & x != 0 & x != ""]
  if (length(x) == 0) {
    return(NA)
  } else {
    return(paste(x, collapse = ","))
  }
})

# 4. Drop old columns (including skipped col 4) and keep gharama in same position
first_pos <- which(names(data) == gharama_cols[1])
data <- data %>%
  select(-all_of(c(gharama_cols, "nani_anahusika_na_gharama___4", "mwingineyo"))) %>%
  relocate(gharama, .before = first_pos)

# 5. Save updated dataset
write.csv(data, "Documents/WORKS/MERGIE/BAGAMOYO/cleaned_community_data_2025_09_25.csv", row.names = FALSE)

