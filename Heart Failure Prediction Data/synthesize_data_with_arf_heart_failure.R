
# Load needed packages ####
library(data.table)
library(arf)
library(dplyr)
library(haven)

# Load functions ####
source("Heart Failure Prediction Data/functions.R")





# Load the necessary libraries ####
library(arf)


# Import dataset ####
heart_failure <- readRDS("Heart Failure Prediction Data/Data/heart_failure_data.Rds")
real_train <- read.csv("Heart Failure Prediction Data/Raw Data/real_train.csv")



# Set seed
set.seed(3105)

# Generate 50 datasets
generate_synthetic_datasets(data = heart_failure, repetitions = 50, output_folder = "Heart Failure Prediction Data/Data/arf/")



# Generate 50 train data sets
generate_synthetic_datasets(data = real_train, repetitions = 50, output_folder = "Heart Failure Prediction Data/Data/train_data/arf/")




