
# Load the necessary library for synthetic data generation ####
library(synthpop)



# Source an external script containing custom functions ####
source("Medical Insurance Cost Data/functions.R")



# Import the dataset containing medical insurance data ####

# The dataset is assumed to be in CSV format, located at "Medical Insurance Cost Daten/Raw Data/medical_insurance.csv"
medical_insurance <- read.csv("Medical Insurance Cost Data/Raw Data/medical_insurance.csv")
real_train <- read.csv("Medical Insurance Cost Data/Raw Data/real_train.csv")




# Generate synthetic datasets  ####

# Generate 50 synthetic datasets
generate_synthetic_data_synthpop(data = medical_insurance, 
                                 reps = 50, 
                                 seed = 3105, 
                                 output_folder = "Medical Insurance Cost Data/Data/synthpop/")



# Generate 50 synthetic train datasets
generate_synthetic_data_synthpop(data = real_train, 
                                 reps = 50, 
                                 seed = 3105, 
                                 output_folder = "Medical Insurance Cost Data/Data/train_data/synthpop/")

