# Load packages ####
library(dplyr)
library(naniar)


# Import data set ####

medical_insurance <- read.csv("Medical Insurance Cost Data/Raw Data/medical_insurance.csv")


# Are there any missing values?
miss_var_summary(medical_insurance)
# -> no!




# Data partitioning
set.seed(3105)
n <- nrow(medical_insurance)
train_indices <- sample(1:n, size = round(0.8 * n))  
medical_insurance_train <- medical_insurance[train_indices, ]
medical_insurance_holdout <- medical_insurance[-train_indices, ]

# Datensatz als CSV speichern
write.csv(medical_insurance_train, "Medical Insurance Cost Data/Raw Data/real_train.csv", row.names = FALSE)
write.csv(medical_insurance_holdout, "Medical Insurance Cost Data/Raw Data/real_holdout.csv", row.names = FALSE)


# For alpha precision, beta recall and authentcity
medical_insurance <- medical_insurance %>%
  mutate(sex = as.character(sex),
         children = as.numeric(children),
         smoker = as.character(smoker),
         region = as.character(region))
write.csv(medical_insurance, "Medical Insurance Cost Data/Data/medical_insurance_2.csv", row.names = FALSE)
