#### Preamble ####
# Purpose: Tests the cleaned dataset obtained from
# "OpenPowerlifing"
# Author: Ricky Fung
# Date: 14 April 2023
# Contact: ricky.fung@mail.utoronto.ca
# Pre-requisites: Run script "02-data_cleaning.R" and "00-install_packages.R"


#### Workspace setup ####
library(tidyverse)
library(arrow)


#### Test data ####

pl_analysis_data <- read_parquet("data/analysis_data/powerlifting_analysis_data.parquet")


# Test to check the column names
colnames(pl_analysis_data) == c("sex", "equipment", "age", "age_class", "bodyweight_kg", "weight_class_kg",
                                 "best3squat_kg", "best3bench_kg", "best3deadlift_kg", "total_kg",
                                 "wilks")

# Test to check the AgeClass values
pl_analysis_data$age_class %>% 
  unique() %>% sort()== c("13-15", "16-17", "18-19", "20-23",
                          "24-34", "35-39", "40-44", "45-49", "5-12" ,
                          "50-54", "55-59", "60-64", "65-69",
                          "70-74", "75-79", "80-999")

# Test to check the WeightClassKg values
pl_analysis_data$weight_class_kg %>% 
  unique() %>% sort() == c("100-109",   "110-129",   "130-145", "150+"  ,"30-39",
                           "40-49", "50-59",   "60-69",   "70-79",
                           "80-89",  "90-99")

# Test to check the Equipment values
pl_analysis_data$equipment %>% 
  unique() %>% sort() == c("Multi-ply",  "Raw",  "Single-ply",
                           "Unlimited",  "Wraps" )


# Test to check the Sex
pl_analysis_data$sex %>% 
  unique() %>% sort() == c("Female", "Male")

# Test to check for NA values in the data set
any(is.na(pl_analysis_data)) == FALSE





 
