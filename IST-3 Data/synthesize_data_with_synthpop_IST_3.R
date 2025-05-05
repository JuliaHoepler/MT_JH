
# Load the necessary library for synthetic data generation ####
library(synthpop)
library(dplyr)
library(data.table)


# Source an external script containing custom functions ####
source("Medical Insurance Cost Data/functions.R")


# Import dataset ####
data_small <- readRDS("IST-3 Data/Raw Data/data_small.Rds")
real_train <- read.csv("IST-3 Data/Raw Data/real_train.csv")



# Some data processing
# data_small <- data_small %>%
#   mutate(outcome = as.factor(outcome),
#          itt_treat = as.factor(itt_treat),
#          vis_infarct = as.factor(vis_infarct))



# Create synthetic data ####

# Generate 50 synthetic datasets
generate_synthetic_data_synthpop(data = data_small, 
                                 reps = 50, 
                                 seed = 3105, 
                                 output_folder = "IST-3 Data/Data/synthpop/")



# Generate 50 synthetic train datasets
generate_synthetic_data_synthpop(data = real_train, 
                                 reps = 50, 
                                 seed = 3105, 
                                 output_folder = "IST-3 Data/Data/train_data/synthpop/")
