#### Preamble ####
# Purpose: Simulates data related to the openpowerlifting dataset
# Author: Ricky Fung
# Date: 14 April 2023
# Contact: ricky.fung@mail.utoronto.ca
# Pre-requisites: Run script "00-install_packages.R".

#### Workspace setup ####
library(tidyverse)


#### Functions to help simulate data ####

# Function to calculate Wilks Score.
calculate_wilks <- function(sex, bodyweight_kg, best3squat_kg, best3bench_kg, best3deadlift_kg) {
  if(sex == "Male") {
    a <- -216.0475144
    b <- 16.2606339
    c <- -0.002388645
    d <- -0.00113732
    e <- 7.01863E-06
    f <- -1.291E-08
  } else {
    a <- 594.31747775582
    b <- -27.23842536447
    c <- 0.82112226871
    d <- -0.00930733913
    e <- 4.731582E-05
    f <- -9.054E-08
  }
  
  wilks_score <- (best3squat_kg + best3bench_kg + best3deadlift_kg) *
    (500 /
       (a + b * bodyweight_kg + c * bodyweight_kg^2 + d * bodyweight_kg^3 + e * bodyweight_kg^4 + f * bodyweight_kg^5))
  
  return(wilks_score)
}

# Function to map Age to an AgeClasses
map_age_to_ageclass <- function(age) {
  if (age >= 24 & age <= 34) {
    return("24-34")
  } else if (age >= 40 & age <= 44) {
    return("40-44")
  } else if (age >= 16 & age <= 17) {
    return("16-17")
  } else if (age >= 35 & age <= 39) {
    return("35-39")
  } else if (age >= 55 & age <= 59) {
    return("55-59")
  } else if (age >= 45 & age <= 49) {
    return("45-49")
  } else if (age >= 18 & age <= 19) {
    return("18-19")
  } else if (age >= 20 & age <= 23) {
    return("20-23")
  } else if (age >= 50 & age <= 54) {
    return("50-54")
  } else if (age >= 60 & age <= 64) {
    return("60-64")
  } else if (age >= 13 & age <= 15) {
    return("13-15")
  } else if (age >= 70 & age <= 74) {
    return("70-74")
  } else if (age >= 65 & age <= 69) {
    return("65-69")
  } else if (age >= 75 & age <= 79) {
    return("75-79")
  } else {
    return("80-999")
  }
}

# Function to map Bodyweight to an WeightClasses
map_bodyweight_to_weightclass <- function(weight_class_kg) {
  if (weight_class_kg <= 30) {
    return("30-39")
  } else if (weight_class_kg <= 40) {
    return("40-49")
  } else if (weight_class_kg <= 50) {
    return("50-59")
  } else if (weight_class_kg <= 60) {
    return("60-69")
  } else if (weight_class_kg <= 70) {
    return("70-79")
  } else if (weight_class_kg <= 80) {
    return("80-89")
  } else if (weight_class_kg <= 90) {
    return("90-99")
  } else if (weight_class_kg <= 100) {
    return("100-109")
  } else if (weight_class_kg <= 110) {
    return("110-129")
  } else {
    return("130-145")
  }
}


#### Simulate data ####

set.seed(302)

simulated_pl_data <- tibble (
  
  sex = sample(x = c("Male", "Female", "Mixed"),
               size = 10000,
               replace = TRUE),
  
  equipment = sample (x= c("Raw", "Wraps", 
                           "Single-ply", "Multi-ply", "Unlimited"), 
                      size = 10000, replace = TRUE),
  
  age = sample(x = 14:85, size = 10000, replace = TRUE),
  
  
  bodyweight_kg = round(runif(10000, min = 30, max = 145), 1),
  
  best3squat_kg = round(runif(10000, min = 50, max = 400), 1), # Sample Best 3 Squat between 50 and 400 kg
  best3bench_kg = round(runif(10000, min = 20, max = 300), 1), # Sample Best 3 Bench between 20 and 300 kg
  best3deadlift_kg = round(runif(10000, min = 60, max = 500), 1)) # Sample Best 3 Deadlift between 60 and 500 kg
  
simulated_pl_data$wilks <- mapply(calculate_wilks, 
                                          simulated_pl_data$sex, 
                                          simulated_pl_data$bodyweight_kg, 
                                          simulated_pl_data$best3squat_kg, 
                                          simulated_pl_data$best3bench_kg, 
                                          simulated_pl_data$best3deadlift_kg)

simulated_pl_data$age_class <- sapply(simulated_pl_data$age, map_age_to_ageclass)

simulated_pl_data$weight_class_kg <- sapply(simulated_pl_data$bodyweight_kg, map_bodyweight_to_weightclass)

  
  
  
  
  
#### Tests ####

# Test to check the number of observations is 1000
nrow(simulated_pl_data) == 10000

# Test to check the column names
colnames(simulated_pl_data) == c("sex", "equipment", "age", "bodyweight_kg", 
                                 "best3squat_kg", "best3bench_kg", "best3deadlift_kg",
                                 "wilks", "age_class", "weight_class_kg")
# Test to check the age_class values
simulated_pl_data$age_class %>% 
  unique() %>% sort()== c("13-15", "16-17", "18-19", "20-23",
                          "24-34", "35-39", "40-44", "45-49",
                          "50-54", "55-59", "60-64", "65-69",
                          "70-74", "75-79", "80-999")

# Test to check the weight_class_kg values
simulated_pl_data$weight_class_kg %>% 
  unique() %>% sort() == c("100-109",   "110-129",   "130-145",   "30-39",
                           "40-49", "50-59",   "60-69",   "70-79",
                           "80-89",  "90-99")

# Test to check the equipment values
simulated_pl_data$equipment %>% 
  unique() %>% sort() == c("Multi-ply",  "Raw",  "Single-ply",
                           "Unlimited",  "Wraps" )


# Test to check the sex
simulated_pl_data$sex %>% 
  unique() %>% sort() == c("Female", "Male", "Mixed")


# Test to check for NA values in the data set
any(is.na(pl_analysis_data)) == FALSE

