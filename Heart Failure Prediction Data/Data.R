# Load packages ####
library(dplyr)
library(naniar)

# https://www.kaggle.com/datasets/fedesoriano/heart-failure-prediction?resource=download
# last visited on: 01.02.2025


# Import data set ####

heart_failure_data <- read.csv("Heart Failure Prediction Data/Raw Data/heart.csv")


# Are there any missing values?
miss_var_summary(heart_failure_data)
# -> no!


# Some data processing
heart_failure_data <- heart_failure_data %>%
  mutate(HeartDisease = as.factor(HeartDisease),
         Sex = as.factor(Sex),
         ChestPainType = as.factor(ChestPainType),
         FastingBS = as.factor(FastingBS),
         RestingECG = as.factor(RestingECG),
         ExerciseAngina = as.factor(ExerciseAngina),
         ST_Slope = as.factor(ST_Slope))

write.csv(heart_failure_data, "Heart Failure Prediction Data/Raw Data/heart_failure_data.csv", row.names = FALSE)




# Data partitioning
set.seed(3105)
n <- nrow(heart_failure_data)
train_indices <- sample(1:n, size = round(0.8 * n))  
heart_failure_train <- heart_failure_data[train_indices, ]
heart_failure_holdout <- heart_failure_data[-train_indices, ]

# Datensatz als CSV speichern
write.csv(heart_failure_train, "Heart Failure Prediction Data/Raw Data/real_train.csv", row.names = FALSE)
write.csv(heart_failure_holdout, "Heart Failure Prediction Data/Raw Data/real_holdout.csv", row.names = FALSE)


