
# Load the necessary library for synthetic data generation ####
library(synthpop)



# Source an external script containing custom functions ####
source("Heart Failure Prediction Data/functions.R")



# Import the dataset containing heart failure data ####

heart_failure <- readRDS("Heart Failure Prediction Data/Data/heart_failure_data.Rds")
real_train <- read.csv("Heart Failure Prediction Data/Raw Data/real_train.csv")




# Generate synthetic datasets  ####

# Generate 50 synthetic datasets
generate_synthetic_data_synthpop(data = heart_failure, 
                                 reps = 50, 
                                 seed = 3105, 
                                 output_folder = "Heart Failure Prediction Data/Data/synthpop/")



# Generate 50 synthetic train datasets
generate_synthetic_data_synthpop(data = real_train, 
                                 reps = 50, 
                                 seed = 3105, 
                                 output_folder = "Heart Failure Prediction Data/Data/train_data/synthpop/")

