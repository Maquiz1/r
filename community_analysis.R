# ========================================================
# R Script: Data Cleaning and Multiple-choice Analysis
# ========================================================

# ----------------------------
# 0. Load required libraries
# ----------------------------
library(readxl)   # To read Excel files
library(dplyr)    # For data manipulation (filter, select, mutate, summarise)
library(tidyr)    # For pivoting and handling missing values
library(ggplot2)  # For plotting charts

# ----------------------------
# 1. Import data from Excel
# ----------------------------
data <- read_excel("Documents/WORKS/MERGIE/BAGAMOYO/data.xlsx")

# ----------------------------
# 2. Explore dataset structure
# ----------------------------
nrow(data)          # Number of rows
ncol(data)          # Number of columns
colnames(data)      # Column names
head(data)          # First few rows
str(data)           # Structure of dataset
summary(data)       # Summary statistics

# ----------------------------
# 3. Check and handle duplicate records
# ----------------------------
any(duplicated(data$record_id))       # Are there duplicates?
sum(duplicated(data$record_id))       # Number of duplicates

dup_ids <- data$record_id[duplicated(data$record_id)]
dup_records <- data[data$record_id %in% dup_ids, ]
View(dup_records)  # View duplicated records

# ----------------------------
# 4. Check frequency and missing values for key variables
# ----------------------------
table(data$mkoa);
sum(is.na(data$mkoa))

table(data$wilaya); 
sum(is.na(data$wilaya))

table(data$kijiji); 
sum(is.na(data$kijiji))

table(data$kitongoji); 
sum(is.na(data$kitongoji))

table(data$jinsia); 
sum(is.na(data$jinsia))

table(data$kiwango_cha_elimu);
sum(is.na(data$kiwango_cha_elimu))

sum(is.na(data$umri))  # Missing values for age

# View rows with missing age
missing_umri <- data[is.na(data$umri), ]
nrow(missing_umri)   
head(missing_umri)   
View(missing_umri)   

# Remove rows with missing age
data <- data %>% drop_na(umri)

# ----------------------------
# 5. Create derived variables
# ----------------------------
# Categorize age into groups
data$age_group <- cut(
  data$umri,
  breaks = c(-Inf, 18, 35, 60, Inf),
  labels = c("0-18", "19-35", "36-60", "61+")
)
age_group <- table(data$age_group)
View(age_group)

# ----------------------------
# 6. Single-answer question analysis
# ----------------------------
# Convert codes to labeled factors
data$yapi_ni_makadirio_sahihi_y <- factor(
  data$yapi_ni_makadirio_sahihi_y,
  levels = c(1, 2, 3),  
  labels = c("Option A", "Option B", "Option C")  
)

# Calculate counts and percentages
single_table <- table(data$yapi_ni_makadirio_sahihi_y)
single_percent <- prop.table(single_table) * 100
single_summary <- data.frame(
  Answer = names(single_table),
  Count = as.numeric(single_table),
  Percent = round(single_percent, 2)
)
View(single_summary)

# ----------------------------
# 7. Multiple-answer question analysis (Block 1)
# ----------------------------
# Select multiple-choice columns
mc_cols1 <- data %>% select(nini_chanzo_chako_cha_mapa___1:nini_chanzo_chako_cha_mapa___3)

# Map column names to readable labels
labels_map1 <- c(
  "nini_chanzo_chako_cha_mapa___1" = "Mshahara wa sekta maalum/ Muajiriwa wa sekta maalum",
  "nini_chanzo_chako_cha_mapa___2" = "Nimejiajiri",
  "nini_chanzo_chako_cha_mapa___3" = "Tegemezi"
)

# Reshape data to long format and handle missing/zero values
mc_long1 <- mc_cols1 %>%
  mutate(id = row_number()) %>%     
  pivot_longer(cols = -id, names_to = "Choice", values_to = "Selected") %>%
  mutate(Choice = labels_map1[Choice]) %>%
  mutate(Selected = ifelse(is.na(Selected), 0, Selected))  

# Summarize multiple-choice responses
mc_summary1 <- mc_long1 %>%
  group_by(Choice) %>%
  summarise(
    Count = sum(Selected != 0),                 
    Percent = round(Count / nrow(data) * 100, 2),
    .groups = "drop"
  )
View(mc_summary1)

# ----------------------------
# 8. Multiple-answer question analysis (Block 2)
# ----------------------------
mc_cols2 <- data %>% select(
  nimejiajiri___1,
  nimejiajiri___2,
  nimejiajiri___3,
  nimejiajiri___4,
  nimejiajiri___5
)

labels_map2 <- c(
  "nimejiajiri___1" = "Biashara",
  "nimejiajiri___2" = "Kilimo",
  "nimejiajiri___3" = "Kibarua",
  "nimejiajiri___4" = "Mvuvi",
  "nimejiajiri___5" = "Ufugaji"
)

mc_long2 <- mc_cols2 %>%
  mutate(id = row_number()) %>%     
  pivot_longer(cols = -id, names_to = "Choice", values_to = "Selected") %>%
  mutate(Choice = labels_map2[Choice]) %>%
  mutate(Selected = ifelse(is.na(Selected), 0, Selected))  

mc_summary2 <- mc_long2 %>%
  group_by(Choice) %>%
  summarise(
    Count = sum(Selected != 0),                 
    Percent = round(Count / nrow(data) * 100, 2),
    .groups = "drop"
  )
View(mc_summary2)

# ----------------------------
# 9. Plot results for Block 2 multiple-choice
# ----------------------------
ggplot(mc_summary2, aes(x = Choice, y = Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5) +
  labs(title = "Self-employment Sources", x = "Type of Work", y = "Count") +
  theme_minimal()

# ----------------------------
# 10. Save cleaned dataset
# ----------------------------
write.csv(data, "Documents/WORKS/MERGIE/BAGAMOYO/cleaned_data.csv", row.names = FALSE)

# ----------------------------
# 11. Export summaries for reporting
# ----------------------------
write.csv(single_summary, "Documents/WORKS/MERGIE/BAGAMOYO/single_summary.csv", row.names = FALSE)
write.csv(mc_summary1, "Documents/WORKS/MERGIE/BAGAMOYO/mc_summary1.csv", row.names = FALSE)
write.csv(mc_summary2, "Documents/WORKS/MERGIE/BAGAMOYO/mc_summary2.csv", row.names = FALSE)
write.csv(missing_summary, "Documents/WORKS/MERGIE/BAGAMOYO/missing_summary.csv", row.names = FALSE)
