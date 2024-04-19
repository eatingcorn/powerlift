#### Preamble ####
# Purpose: Models for the openpowerlifting dataset.
# Author: Ricky Fung
# Date: 14 April 2023
# Contact: ricky.fung@mail.utoronto.ca
# Pre-requisites: Run script "00-install_packages.R".


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(rstanarm)
library(modelsummary)
#### Read data ####
analysis_data <- read_parquet("data/analysis_data/powerlifting_analysis_data.parquet")

### Model data ####

set.seed(302)
# Randomly sample observations to reduce the dataset size
reduced_data <- analysis_data %>% 
  sample_n(size = 15000)


model <-
    stan_glm(
      formula = competitive ~ sex + age_class + weight_class_kg + equipment,
      data = reduced_data,
      family = binomial(link = "logit"),
      prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
      prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
      seed = 302
    )

#### Save model ####
saveRDS(
  model,
  file = "models/model.rds"
)



