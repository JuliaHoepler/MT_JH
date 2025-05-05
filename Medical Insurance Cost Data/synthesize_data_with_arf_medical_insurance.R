
# Load the necessary libraries ####
library(arf)


# Import dataset ####
medical_insurance <- read.csv("Medical Insurance Cost Data/Data/medical_insurance.csv")
real_train <- read.csv("Medical Insurance Cost Data/Raw Data/real_train.csv")



# Set seed
set.seed(3105)

# Generate 50 datasets
generate_synthetic_datasets(data = medical_insurance, repetitions = 50, output_folder = "Medical Insurance Cost Data/Data/arf/")



# Generate 50 train data sets
generate_synthetic_datasets(data = real_train, repetitions = 50, output_folder = "Medical Insurance Cost Data/Data/train_data/arf/")
