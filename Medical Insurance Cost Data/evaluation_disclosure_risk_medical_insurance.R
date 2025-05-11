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


source("Medical Insurance Cost Data/functions.R")

# Load the real dataset
medical_insurance_data <- read.csv("Medical Insurance Cost Data/Raw Data/medical_insurance.csv")
real_data <- medical_insurance_data


real_train <- read.csv("Medical Insurance Cost Data/Raw Data/real_train.csv")
real_holdout <- read.csv("Medical Insurance Cost Data/Raw Data/real_holdout.csv")





# 1. Membership disclosure ####

# Constructing the Attack Dataset

set.seed(3105)

t <- 0.05 
n <- nrow(real_data)
N <- n/t # (N = n/t)

attack_size <- round(0.1 * n)  
num_train_samples <- min(nrow(real_train), max(1, round(t * attack_size)))
num_holdout_samples <- min(nrow(real_holdout), attack_size - num_train_samples)

attack_train <- real_train[sample(1:nrow(real_train), size = num_train_samples, replace = FALSE), ]
attack_holdout <- real_holdout[sample(1:nrow(real_holdout), size = num_holdout_samples, replace = FALSE), ]
attack_data <- rbind(attack_train, attack_holdout)



# Data processing
real_train <- real_train %>%
  mutate(across(c(sex, smoker, region, children), as.factor))

# Evaluate for different values of m
m_values <- c(5)
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")


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
write.csv(results_df, "Medical Insurance Cost Data/evaluation_disclosure_risk/membership_disclosure.csv", row.names = FALSE)
write.csv(mean_results, "Medical Insurance Cost Data/evaluation_disclosure_risk/mean_membership_disclosure.csv", row.names = FALSE)





# 2. Attribute disclosure risk ####

# Convert categorical variables
medical_insurance_data <- medical_insurance_data %>% 
  mutate(sex = as.factor(sex),
         smoker = as.factor(smoker),
         region = as.factor(region))


# Define methods and sample sizes
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")
sample_sizes <- c(5)

# Compute results for all methods and sample sizes
results <- expand.grid(Method = methods, SampleSize = sample_sizes)
results$TCAP<- mapply(compute_attribute_disclosure, results$Method, results$SampleSize)

print(results)

write.csv(results, "Medical Insurance Cost Data/evaluation_disclosure_risk/tcap_synthpop.csv", row.names = FALSE)




