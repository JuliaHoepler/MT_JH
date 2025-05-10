# Load packages
library(synthpop)
library(caret)
library(stringdist)
library(dplyr)
library(arf)
library(ranger)
library(purrr) 
library(caret)      
library(randomForest) 
library(knitr)


source("IST-3 Data/functions.R")

# Load the real dataset
data_small <- readRDS("IST-3 Data/Raw Data/data_small.Rds")


real_data <- data_small


# Data partitioning
set.seed(3105)
n <- nrow(data_small)
train_indices <- sample(1:n, size = round(0.8 * n))  
data_small_train <- data_small[train_indices, ]
data_small_holdout <- data_small[-train_indices, ]

# Datensatz als CSV speichern
write.csv(data_small_train, "IST-3 Data/Raw Data/real_train.csv", row.names = FALSE)
write.csv(data_small_holdout, "IST-3 Data/Raw Data/real_holdout.csv", row.names = FALSE)




real_train <- read.csv("IST-3 Data/Raw Data/real_train.csv")
real_holdout <- read.csv("IST-3 Data/Raw Data/real_holdout.csv")






# 1. Membership disclosure ####

# Constructing the Attack Dataset

set.seed(3105)

t <- 0.05  
n <- nrow(real_data) # 2772
N <- n/t # (N = n/t)

attack_size <- round(0.1 * n)  
num_train_samples <- min(nrow(real_train), max(1, round(t * attack_size)))
num_holdout_samples <- min(nrow(real_holdout), attack_size - num_train_samples)

attack_train <- real_train[sample(1:nrow(real_train), size = num_train_samples, replace = FALSE), ]
attack_holdout <- real_holdout[sample(1:nrow(real_holdout), size = num_holdout_samples, replace = FALSE), ]
attack_data <- rbind(attack_train, attack_holdout)





# List of synthetic data generation methods
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")

# List of dataset sizes to test
m_values <- c(5)


# Compute F1 scores and store results in a single data frame
results_df <- do.call(rbind, lapply(m_values, function(m) {
  do.call(rbind, lapply(methods, evaluate_method, m = m, n = n, N = N))
}))

# Return the final results data frame
results_df

# Compute mean NNDR per method and dataset size, plus difference
mean_results <- results_df %>%
  group_by(Method, m) %>%
  summarise(
    Mean_M = round(mean(M, na.rm = TRUE), 3),
    .groups = "drop"
  )

print(mean_results)


# Save results to CSV
write.csv(results_df, "IST-3 Data/evaluation_disclosure_risk/membership_disclosure.csv", row.names = FALSE)
write.csv(mean_results, "IST-3 Data/evaluation_disclosure_risk/mean_membership_disclosure.csv", row.names = FALSE)






# 2. Attribute disclosure risk ####


## 2.1 Calculate TCAP ####

#m_values <- c(5, 10, 50)
m_values <- c(5)

methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")

# Compute TCAP scores for all methods
tcap_results <- data.frame(
  Methode = methods,
  do.call(rbind, lapply(methods, compute_tcap_for_method, m_values = m_values))
)

# Rename columns for better readability
#colnames(tcap_results) <- c("Methode", "m = 5", "m = 10", "m = 50")
colnames(tcap_results) <- c("Methode", "m = 5")

print(tcap_results)


# Save the TCAP results to a CSV file
write.csv(tcap_results, "IST-3 Data/evaluation_disclosure_risk/TCAP_scores.csv", row.names = FALSE)







