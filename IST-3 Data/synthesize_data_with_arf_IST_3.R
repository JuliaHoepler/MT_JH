
# Load the necessary libraries ####

library(arf)
library(dplyr)


# Source an external script containing custom functions ####
source("Medical Insurance Cost Data/functions.R")


# Import dataset ####

data_small <- readRDS("IST-3 Data/Raw Data/data_small.Rds")
real_train <- read.csv("IST-3 Data/Raw Data/real_train.csv")


# Some data processing
data_small <- data_small %>%
  mutate(outcome = as.factor(outcome),
         itt_treat = as.factor(itt_treat),
         vis_infarct = as.factor(vis_infarct))


# set seed
set.seed(3105)


# Generate 50 datasets
generate_synthetic_datasets(data = data_small, repetitions = 50, output_folder = "IST-3 Data/Data/arf/")

# Generate 50 train data sets
generate_synthetic_datasets(data = real_train, repetitions = 50, output_folder = "IST-3 Data/Data/train_data/arf/")





