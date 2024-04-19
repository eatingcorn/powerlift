#### Preamble ####
# Purpose: Cleans the raw_powerlifting_data dataset obtained from
# "OpenPowerlifing"
# Author: Ricky Fung
# Date: 14 April 2023
# Contact: ricky.fung@mail.utoronto.ca
# Pre-requisites: Run script "00-install_packages.R".

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(janitor)
library(arrow)


# Function to map weight classes to broader categories
map_weightclass_to_broader_category <- function(weight_class_kg) {
  weight_classes <- c(20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 130, 150, Inf)
  categories <- c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", 
                  "90-99", "100-109", "110-129", "130-145", "150+")
  range_index <- findInterval(as.numeric(weight_class_kg), weight_classes)
  ifelse(range_index == 0, "Other", categories[range_index])
}


# Read raw data
raw_data <- read_csv("data/raw_data/raw_powerlifting_data.csv")

# Clean data
cleaned_data <- raw_data %>%
  clean_names() %>%
  # Remove non-tested and disqualified individuals
  filter(!is.na(tested) & tested != "" & place != "DQ" & age_class != "12-May" & sex != "Mx") %>%
  
  # Mutate total_kg to binary variable
  mutate(
    sex = case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male"
    ),
    weight_class_kg = map_weightclass_to_broader_category(weight_class_kg),
    competitive = ifelse(total_kg >= 600, 1, 0)
  ) %>%
  
  # Select columns for analysis
  select(competitive, sex, equipment, age_class, weight_class_kg, total_kg) %>% 
  # Remove rows with missing information
  na.omit()

# Save data as CSV
write_csv(cleaned_data, "data/analysis_data/powerlifting_analysis_data.csv")

# Save data as Parquet
write_parquet(cleaned_data, "data/analysis_data/powerlifting_analysis_data.parquet")


