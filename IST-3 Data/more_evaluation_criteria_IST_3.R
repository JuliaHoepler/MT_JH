
#...............................................................................
# LOAD NECESSARY LIBRARIES ####
#...............................................................................

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
library(FNN) 
library(ggplot2)
library(tidyr)
library(data.table)
library(cluster)
library(keras)
library(class)
library(pROC)
library(RANN)  # For nearest neighbor search
library(dplyr) # For data manipulation
library(FNN)   # Alternative nearest neighbor library if needed
library(stats) # For PCA
library(e1071)



#...............................................................................
# SOURCE AN EXTERNAL FUNCTION SCRIPT ####
#...............................................................................

source("IST-3 Data/functions.R")




#...............................................................................
# IMPORT DATA SETS ####
#...............................................................................

data_small <- readRDS("IST-3 Data/Raw Data/data_small.Rds")
real_data <- data_small

real_train <- read.csv("IST-3 Data/Raw Data/real_train.csv")
real_test <- read.csv("IST-3 Data/Raw Data/real_holdout.csv")
real_holdout <- real_test



# data_small$outcome <- ifelse(data_small$outcome == 1, 0, 
#                              ifelse(data_small$outcome == 0, 1, data_small$outcome))



#...............................................................................
# Alpha-precision, beta-recall & authenticity ####
#...............................................................................

# Type: resemblance, privacy


# Note: Die drei Metriken müssen nochmal genauer angeschaut werden und
# wahrscheinlich noch angepasst werden.


set.seed(3105)

# Initialize empty dataframe
results_df <- data.frame(Method = character(), 
                         M = integer(), 
                         AlphaPrecision = numeric(), 
                         BetaRecall = numeric(), 
                         Authenticity = numeric())

# List of methods and dataset sizes
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")
m_values <- c(5)

# Iterate through methods and dataset sizes
for (method in methods) {
  
  for (m in m_values) {
    
    path <- paste0("IST-3 Data/Data/", method)
    pattern <- paste0("^syn_data_small_", method, "_.*\\.csv$")
    files <- list.files(path = path, pattern = pattern, full.names = TRUE)
    selected_files <- files[grepl(paste0("syn_data_small_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
    syn_data_list <- lapply(selected_files, read.csv)
    
    # Compute evaluation metrics
    alpha_precision <- compute_alpha_precision_multi(real_data, syn_data_list)
    beta_recall <- compute_beta_recall_multi(real_data, syn_data_list)
    authenticity <- compute_authenticity_multi(real_data, syn_data_list)
    
    # Store results
    temp_df <- data.frame(Method = method, 
                          M = m, 
                          AlphaPrecision = mean(alpha_precision), 
                          BetaRecall = mean(beta_recall), 
                          Authenticity = mean(authenticity))
    results_df <- bind_rows(results_df, temp_df)
    
  }
  
}

# Print results
print(results_df)

# Save results
write.csv(results_df, "IST-3 Data/evaluation_more_criteria/synthetic_data_evaluation_precision_recall_authenticity.csv", row.names = FALSE)




library(forcats)

results_df <- read.csv("IST-3 Data/alpha_precision_results_means.csv")


results_df <- results_df %>%
  mutate(method = fct_recode(method,
                             ARF = "arf",
                             CTGAN = "ctgan",
                             PrivBayes = "privbayes",
                             Synthpop = "synthpop",
                             TABSYN = "tabsyn",
                             TVAE = "tvae"))


plot_precision_recall <- ggplot(results_df, aes(x = beta_recall, y = alpha_precision, color = method)) +
  geom_jitter(size = 4, width = 0.005) +  # Added jitter
  xlim(0.0, 1.0) +
  ylim(0.0, 1.0) +
  labs(x = expression(beta * "-Recall (Diversity)"),
    y = expression(alpha * "-Precision (Fidelity)")
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2")

plot_precision_recall

ggsave(
  filename = "plot_precision_recall_m5.pdf",
  plot = plot_precision_recall,
  device = "pdf",
  path = "IST-3 Data/evaluation_more_criteria/",
  width = 7,
  height = 7
)





#...............................................................................
# Classifier two-sample test (C2ST) ####
#...............................................................................

# Type: resemblance

# Example of running the function
results_df <- data.frame(Method = character(), 
                         M = integer(), 
                         C2ST_Accuracy = numeric(),
                         AUC = numeric(),
                         P_Value = numeric(),
                         Classifier = character())


# List of synthetic data generation methods
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")

# List of dataset sizes to test
m_values <- c(5, 10, 50)

# # Test parameters:
# methods <- c("synthpop", "arf")
# m_values <- c(5, 10)

set.seed(3105)

for (method in methods) {
  for (m in m_values) {
    
    path <- paste0("IST-3 Data/Data/train_data/", method)
    pattern <- paste0("^syn_real_train_", method, "_.*\\.csv$")
    files <- list.files(path = path, pattern = pattern, full.names = TRUE)
    selected_files <- files[grepl(paste0("syn_real_train_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
    syn_data_list <- lapply(selected_files, read.csv)
    
    # Evaluating using KNN
    results_df <- bind_rows(results_df, evaluate_c2st_per_dataset_cv(method, m, real_test, syn_data_list, classifier_type = "KNN"))
    
    # Evaluate using Random Forest
    results_df <- bind_rows(results_df, evaluate_c2st_per_dataset_cv(method, m, real_test, syn_data_list, classifier_type = "RF"))
    
    # Evaluate using Neural Network
    results_df <- bind_rows(results_df, evaluate_c2st_per_dataset_cv(method, m, real_test, syn_data_list, classifier_type = "NN"))
  }
}

# Reset the row names
rownames(results_df) <- NULL


# Compute mean identical share per method and dataset size
mean_results <- results_df %>%
  group_by(Method, M, Classifier) %>%
  summarise(Mean_C2ST_Accuracy = round(mean(C2ST_Accuracy, na.rm = TRUE), 3),
            Mean_AUC = round(mean(AUC, na.rm = TRUE), 3), .groups = "drop")

# Print results
print(results_df)
print(mean_results)


# Save results
write.csv(results_df, "IST-3 Data/evaluation_more_criteria/synthetic_data_evaluation_c2st_avg.csv", row.names = FALSE)
write.csv(mean_results, "IST-3 Data/evaluation_more_criteria/synthetic_data_evaluation_mean_c2st_avg.csv", row.names = FALSE)


synthetic_data_evaluation_c2st_avg <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_evaluation_c2st_avg.csv")
synthetic_data_evaluation_mean_c2st_avg <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_evaluation_mean_c2st_avg.csv")









#...............................................................................
# ML utility ####
#...............................................................................

# Type: utility

# Real dataset is split into a training set and a test set.
# Training set is used to train a generative model.
# Predictive performance for a downstream task on real training data and synthetic
# data is evaluated on the real test dataset and compared.

# Initialize empty results dataframe
results_df <- data.frame(Method = character(), 
                         M = integer(), 
                         Accuracy = numeric())

# List of synthetic data generation methods
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")

# List of dataset sizes to test
m_values <- c(5, 10, 50)

# Define target variable
target_var <- "outcome"


# Test parameters:
# methods <- c("synthpop", "arf")
# m_values <- c(5, 10)


# Iterate through methods and dataset sizes
for (method in methods) {
  
  for (m in m_values) {
    
    path <- paste0("IST-3 Data/Data/train_data/", method)
    pattern <- paste0("^syn_real_train_", method, "_.*\\.csv$")
    files <- list.files(path = path, pattern = pattern, full.names = TRUE)
    selected_files <- files[grepl(paste0("syn_real_train_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
    syn_data_list <- lapply(selected_files, read.csv)
    
    results_df <- bind_rows(results_df, evaluate_regression_per_dataset(method, m, real_test, syn_data_list))
    
  }
  
}



# Compute mean identical share per method and dataset size
mean_results <- results_df %>%
  group_by(Method, M) %>%
  summarise(Mean_Accuracy = round(mean(Accuracy, na.rm = TRUE), 3), .groups = "drop")

# Print results
print(results_df)
print(mean_results)


# Save results
write.csv(results_df, "IST-3 Data/evaluation_more_criteria/synthetic_data_ML_utility.csv", row.names = FALSE)
write.csv(mean_results, "IST-3 Data/evaluation_more_criteria/synthetic_data_mean_ML_utility.csv", row.names = FALSE)




#...............................................................................
# Share of identical records ####
#...............................................................................


# for (i in 1:50) {
#   old_name <- paste0("IST-3 Data/Data/train_data/tabsyn/syn_IST-3_train_TabSyn_", i)
#   new_name <- paste0("IST-3 Data/Data/train_data/tabsyn/syn_real_train_tabsyn_", i)
# 
#   file.rename(old_name, new_name)
# }
# 
# folder <- "IST-3 Data/Data/train_data/tabsyn"
# 
# for (i in 1:50) {
#   old_name <- file.path(folder, paste0("syn_real_train_tabsyn_", i))
#   new_name <- paste0(old_name, ".csv")
# 
#   if (file.exists(old_name)) {
#     file.rename(old_name, new_name)
#   }
# }

# Type: privacy

# Initialize results dataframe
results_df <- data.frame(Method = character(), M = integer(), 
                         IMS_Train_Syn = numeric(), 
                         IMS_Train_Test = numeric(), 
                         stringsAsFactors = FALSE)

# List of synthetic data generation methods
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")

# List of dataset sizes to test
m_values <- c(5, 10, 50)


# Iterate over methods and dataset sizes
for (method in methods) {
  for (m in m_values) {
    
    # Define file path
    path <- paste0("IST-3 Data/Data/train_data/", method)
    
    # Read synthetic dataset files
    files <- list.files(path, pattern = paste0("^syn_real_train_", method, ".*\\.csv$"), full.names = TRUE)
    
    csv_files <- files[grepl(paste0("syn_real_train_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
    
    for (file in csv_files) {
      
      synthetic_data <- read.csv(file)
      
      num_identical_train_syn <- nrow(semi_join(synthetic_data, real_train, by = colnames(real_train)))
      share_identical_train_syn <- num_identical_train_syn / nrow(synthetic_data)
      
      num_identical_train_test <- nrow(semi_join(real_test, real_train, by = colnames(real_train)))
      share_identical_train_test <- num_identical_train_test / nrow(real_test)
      
      # Store results
      results_df <- bind_rows(results_df, data.frame(Method = method, M = m, 
                                                     IMS_Train_Syn = share_identical_train_syn,
                                                     IMS_Train_Test = share_identical_train_test))
    }
    
  }
}

# Print results
print(results_df)

# Compute mean identical share per method and dataset size
mean_results <- results_df %>%
  group_by(Method, M) %>%
  summarise(Mean_IMS_Train_Syn = round(mean(IMS_Train_Syn, na.rm = TRUE), 3),
            Mean_IMS_Train_Test = round(mean(IMS_Train_Test, na.rm = TRUE), 3), 
            .groups = "drop")

print(mean_results)


# Save results
write.csv(results_df, "IST-3 Data/evaluation_more_criteria/synthetic_data_identical_records.csv", row.names = FALSE)
write.csv(mean_results, "IST-3 Data/evaluation_more_criteria/synthetic_data_mean_identical_records.csv", row.names = FALSE)




#...............................................................................
# Distance to closest records (DCR) ####
#...............................................................................

# Type: privacy

# Comparison of the nearest neighbor distances between training, synthetic data 
# and holdout data.

# The distances between synthetic data and training data should not be systematically 
# smaller than between training and holdout data.


# Note:
# Macht Gower-Distanz bei gemischten Variablentypen mehr Sinn
# oder one-hot encoding für die kategorialen und dann in numerics umwandeln
# und Manhattan-Distanz berechnen?
# -> Gower-Distanz




# Initialize results dataframe
results_df <- data.frame(Method = character(), M = integer(), 
                         DCR_Synth_Train = numeric(), DCR_Holdout_Train = numeric(), 
                         stringsAsFactors = FALSE)

# List of synthetic data generation methods
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")

# List of dataset sizes to test
m_values <- c(5, 10, 50)



# Convert character columns to factors in real datasets
real_train <- convert_to_factors(real_train)
real_holdout <- convert_to_factors(real_holdout)

# Iterate over methods and dataset sizes
for (method in methods) {
  for (m in m_values) {
    
    # Define file path
    path <- paste0("IST-3 Data/Data/train_data/", method)
    
    # Read synthetic dataset files
    files <- list.files(path, pattern = paste0("^syn_real_train_", method, ".*\\.csv$"), full.names = TRUE)
    csv_files <- files[grepl(paste0("syn_real_train_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
    
    for (file in csv_files) {
      
      synthetic_data <- read.csv(file)
      synthetic_data <- convert_to_factors(synthetic_data)
      
      
      # Calculate DCR
      dcr_value <- calculate_dcr(synthetic_data, real_train, real_holdout)
      
      # Store results
      results_df <- bind_rows(results_df, data.frame(Method = method, 
                                                     M = m,
                                                     DCR_Synth_Train = dcr_value$dcr_synth_train,
                                                     DCR_Holdout_Train = dcr_value$dcr_holdout_train))
    }
  }
}

# Compute mean DCR per method and dataset size, rounded to 3 decimal places
mean_results <- results_df %>%
  group_by(Method, M) %>%
  summarise(across(starts_with("DCR"), ~ round(mean(.x, na.rm = TRUE), 3)), .groups = "drop")

# Print results
print(results_df)
print(mean_results)

# Save results
write.csv(results_df, "IST-3 Data/evaluation_more_criteria/synthetic_data_dcr.csv", row.names = FALSE)
write.csv(mean_results, "IST-3 Data/evaluation_more_criteria/synthetic_data_dcr_mean.csv", row.names = FALSE)



# Interpretation (m=5):
# The Mean_DCR values are small negative numbers.
# Since DCR is calculated as the difference between the minimum distances to the 
# training and holdout sets, a negative value suggests that, on average, synthetic 
# records are slightly closer to the holdout set than to the training set.
# This could indicate that the synthetic data captures the distribution well and 
# does not overfit to the training data.

# -> When the difference is negative, then the distances between synthetic data 
# and training data are not be systematically smaller than between training and 
# holdout data
# -> good







#...............................................................................
# Nearest-neighbor distance ratio (NNDR) ####
#...............................................................................

# 1. Find nearest neighbors:
#   - For each synthetic data point, identify its two nearest neighbors in the real dataset
#   - Compute the Euclidean (or another chosen metric) distance between the synthetic 
#     point and these two nearest real points: 
#                                             d_1 (distance to closest neighbor)
#                                             d_2 (distance to second-closest neighbor)


# 2. Compute the NNDR:
#    d_1 (distance to closest neighbor) / d_2 (distance to second-closest neighbor)


# 3. Interpretation:
#    - NNDR close to 1: The nearest and second-nearest neighbors are almost equally
#      distant, meaning the synthetic data does not strongly resemble a specific 
#      real point. Suggest good privacy.

#    - NNDR close to 0: The nearest neighbor is much closer than the second-nearest,
#      meaning the synthetic data point is highly to one real data point. This
#      suggests higher privacy risk.

#    - Moderate NNDR: More balanced privacy and utility.


# Type: privacy

# Initialize results dataframe
results_nndr <- data.frame(Method = character(), M = integer(),
                           Mean_NNDR_Synth = numeric(), Mean_NNDR_Holdout = numeric(),
                           stringsAsFactors = FALSE)

# Synthetic data generation methods and dataset sizes to test
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")
m_values <- c(5, 10, 50)

# Convert real datasets to factor format where appropriate
real_train <- convert_to_factors(real_train)
real_holdout <- convert_to_factors(real_holdout)



# --- Main Loop ---

for (method in methods) {
  for (m in m_values) {
    
    path <- paste0("IST-3 Data/Data/train_data/", method, "/")
    files <- list.files(path, pattern = paste0("^syn_real_train_", method, ".*\\.csv$"), full.names = TRUE)
    csv_files <- files[grepl(paste0("syn_real_train_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
    
    for (file in csv_files) {
      
      synthetic_data <- read.csv(file) %>% convert_to_factors()
      
      nndr_result <- compute_nndr_tabular(synthetic_data, real_train, real_holdout)
      
      mean_synth <- mean(nndr_result$nndr_synth, na.rm = TRUE)
      mean_holdout <- mean(nndr_result$nndr_holdout, na.rm = TRUE)
      
      results_nndr <- bind_rows(results_nndr, data.frame(
        Method = method,
        M = m,
        Mean_NNDR_Synth = mean_synth,
        Mean_NNDR_Holdout = mean_holdout
      ))
    }
  }
}


# --- Aggregation ---

# Compute mean NNDR per method and dataset size, plus difference
mean_results <- results_nndr %>%
  group_by(Method, M) %>%
  summarise(
    Mean_NNDR_Synth = round(mean(Mean_NNDR_Synth, na.rm = TRUE), 3),
    Mean_NNDR_Holdout = round(mean(Mean_NNDR_Holdout, na.rm = TRUE), 3),
    NNDR_Diff = round(mean(Mean_NNDR_Synth - Mean_NNDR_Holdout, na.rm = TRUE), 3),
    .groups = "drop"
  )

# Display results
print(results_nndr)
print(mean_results)

# Save results
write.csv(results_nndr, "IST-3 Data/evaluation_more_criteria/synthetic_data_nndr.csv", row.names = FALSE)
write.csv(mean_results, "IST-3 Data/evaluation_more_criteria/synthetic_data_mean_nndr.csv", row.names = FALSE)



# Interpretation:

# Key Observations (m=5)

# arf (Mean: 0.884, SD: 0.008):
# Has the lowest mean NNDR, meaning its synthetic samples are more distinct from real data.
# The higher SD (0.00812) indicates more variability in NNDR values.

# ctgan (Mean: 0.921, SD: 0.002):
# Generates synthetic data very similar to real data (high mean NNDR).
# The lowest SD (0.00186) suggests its NNDR values are very consistent.

# privbayes (Mean: 0.942, SD: 0.003):
# Produces the most similar synthetic data to real data (highest mean NNDR).
# Slightly more variation than ctgan, but still quite stable.

# synthpop (Mean: 0.891, SD: 0.008):
# Slightly more diversity than ctgan and privbayes, but still relatively high NNDR.
# Moderate SD (0.00766) suggests some variation in the matches.



#...............................................................................
# Correlation plots ####
#...............................................................................

library(ggcorrplot)
library(readr)
library(patchwork)
library(fastDummies)

# Color schemes
colors_corr <- c("#2166AC", "#F7F7F7", "#B2182B")  # For correlation plot
colors_diff <- c("#762A83", "#F7F7F7", "#1B7837")  # For difference plot


# --- Process original data ---
real_clean <- process_dataset(real_data)
cor_real <- cor(real_clean, use = "pairwise.complete.obs", method = "pearson")






# --- Original correlation matrix plot ---
cor_real <- cor(real_clean, use = "pairwise.complete.obs", method = "pearson")
cor_plot_original <- ggcorrplot(cor_real, 
                                type = "upper", 
                                lab = FALSE,
                                colors = colors_corr,
                                show.legend = TRUE,
                                lab_size = 2,
                                digits = 2,
                                lab_col = "black",
                                legend.title = "Correlation")


# Save original correlation matrix as PDF
ggsave(filename = file.path("IST-3 Data/evaluation_more_criteria/corrplot_Original_Data.pdf"),
       plot = cor_plot_original,
       width = 6, height = 6,
       device = "pdf")



# --- Store plots here ---
cor_plots <- list()
cor_diff_plots <- list()

# --- Add original data plot ---
cor_plots[[1]] <- ggcorrplot(cor_real, type = "upper", lab = TRUE, 
                             title = "Original Data", show.legend = TRUE)

# --- Methods and number of files to use ---
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")
m <- 5 # number of synthetic datasets per method

# Mapping of method names to pretty display names
method_pretty_names <- c(
  synthpop = "Synthpop",
  arf = "ARF",
  privbayes = "PrivBayes",
  ctgan = "CTGAN",
  tvae = "TVAE",
  tabsyn = "TABSYN"
)


for (method in methods) {
  
  pretty_method_name <- method_pretty_names[[method]]
  
  message("Processing method: ", method)
  
  path <- paste0("IST-3 Data/Data/", method, "/")
  files <- list.files(path, pattern = paste0("^syn_data_small_", method, ".*\\.csv$"), full.names = TRUE)
  csv_files <- files[grepl(paste0("syn_data_small_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
  
  cor_list <- list()
  
  for (file in csv_files) {
    df <- read.csv(file)
    df_clean <- process_dataset(df)
    
    if (all(colnames(df_clean) == colnames(real_clean))) {
      cor_synth <- cor(df_clean, use = "pairwise.complete.obs", method = "pearson")
      cor_list[[length(cor_list) + 1]] <- cor_synth
    }
  }
  
  if (length(cor_list) == 0) {
    message("No valid synthetic datasets for method: ", method)
    next
  }
  
  cor_avg <- Reduce("+", cor_list) / length(cor_list)
  
  # --- Plot correlation matrix ---
  p_cor <- ggcorrplot(cor_avg, 
                      type = "upper", 
                      lab = FALSE,
                      #title = paste0(method), 
                      colors = colors_corr,
                      show.legend = TRUE,
                      lab_size = 2, 
                      digits = 2,
                      lab_col = "black",
                      legend.title = "Correlation")
  
  
  ggsave(filename = paste0("IST-3 Data/evaluation_more_criteria/corrplot_", method, ".pdf"),
         plot = p_cor,
         width = 5, height = 5,
         device = "pdf")
  
  # --- Plot difference matrix ---
  cor_diff <- cor_avg - cor_real
  
  p_diff <- ggcorrplot(cor_diff, 
                       type = "upper", 
                       lab = FALSE,
                       #title = paste0(method),
                       colors = colors_diff,
                       show.legend = TRUE,
                       lab_size = 2,
                       digits = 2,
                       lab_col = "black",
                       legend.title = "Correlation\nDifference")
  
  
  
  ggsave(filename = paste0("IST-3 Data/evaluation_more_criteria/corrdiff_", method, ".pdf"),
         plot = p_diff,
         width = 5, height = 5,
         device = "pdf")
}


















#...............................................................................
# Outlier ####
#...............................................................................

## m = 1 ####

data_small <- readRDS("IST-3 Data/Raw Data/data_small.Rds")
IST_3_synthpop_1 <- read.csv("IST-3 Data/Data/synthpop/syn_data_small_synthpop_1.csv")
IST_3_arf_1 <- read.csv("IST-3 Data/Data/arf/syn_data_small_arf_1.csv")
IST_3_privbayes_1 <- read.csv("IST-3 Data/Data/privbayes/syn_data_small_privbayes_1.csv")
IST_3_ctgan_1 <- read.csv("IST-3 Data/Data/ctgan/syn_data_small_ctgan_1.csv")


# Funktion für Scatterplots
scatter_plot <- function(data, title, color) {
  ggplot() +
    geom_point(data = data_small, aes(x = nihss, y = randdelay), shape = 1, color = "black", alpha = 0.5) +  
    geom_point(data = data, aes(x = nihss, y = randdelay), shape = 4, color = color, alpha = 0.7) +
    labs(title = title, x = "nihss", y = "randdelay") +
    theme_minimal()
}

# Plots für alle Methoden erstellen
p1 <- scatter_plot(IST_3_arf_1, "ARF", "blue")
p2 <- scatter_plot(IST_3_synthpop_1, "Synthpop", "red")
p3 <- scatter_plot(IST_3_privbayes_1, "PrivBayes", "green")
p4 <- scatter_plot(IST_3_ctgan_1, "CTGAN", "purple")

# Plots nebeneinander anzeigen
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)



# How to handle more than one synthetic dataset per method?

# 1. Overlaying

# Funktion für Scatterplots mit mehreren synthetischen Datensätzen
scatter_plot_multiple <- function(data_list, title, color) {
  ggplot() +
    geom_point(data = data_small, aes(x = nihss, y = randdelay), shape = 1, color = "black", alpha = 0.5) +  # Originaldaten
    lapply(data_list, function(df) geom_point(data = df, aes(x = nihss, y = randdelay), shape = 4, color = color, alpha = 0.1)) +  # Alle synthetischen Sätze überlagern
    labs(title = title, x = "nihss", y = "randdelay") +
    theme_minimal()
}

# Listen mit synthetischen Datensätzen erstellen (Beispiel für das Laden der 50 Dateien)
IST_3_synthpop_list <- lapply(1:5, function(i) read.csv(paste0("IST-3 Data/Data/synthpop/syn_data_small_synthpop_", i, ".csv")))
IST_3_arf_list <- lapply(1:5, function(i) read.csv(paste0("IST-3 Data/Data/arf/syn_data_small_arf_", i, ".csv")))
IST_3_privbayes_list <- lapply(1:5, function(i) read.csv(paste0("IST-3 Data/Data/privbayes/syn_data_small_privbayes_", i, ".csv")))
IST_3_ctgan_list <- lapply(1:5, function(i) read.csv(paste0("IST-3 Data/Data/ctgan/syn_data_small_ctgan_", i, ".csv")))

# Plots erstellen
p1 <- scatter_plot_multiple(IST_3_arf_list, "ARF", "blue")
p2 <- scatter_plot_multiple(IST_3_synthpop_list, "Synthpop", "red")
p3 <- scatter_plot_multiple(IST_3_privbayes_list, "PrivBayes", "green")
p4 <- scatter_plot_multiple(IST_3_ctgan_list, "CTGAN", "purple")

# Plots nebeneinander anzeigen
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)





# 2. Plot each synthetic dataset in a grid

all_plots <- lapply(1:10, function(i) scatter_plot(IST_3_synthpop_list[[i]], paste("Synthpop - Set", i), "red"))
gridExtra::grid.arrange(grobs = all_plots, ncol = 5)








# Function to create scatter plot for individual synthetic datasets
scatter_plot <- function(data, title, color) {
  ggplot() +
    geom_point(data = data_small, aes(x = nihss, y = randdelay), shape = 1, color = "black", alpha = 0.5) +  # Original data as circles
    geom_point(data = data, aes(x = nihss, y = randdelay), shape = 4, color = color, alpha = 0.7) +  # Synthetic data as crosses
    labs(title = title, x = "nihss", y = "randdelay") +
    theme_minimal()
}

# Function to generate multiple plots for a method
generate_plots <- function(data_list, method_name, color, num_plots = 10) {
  lapply(1:num_plots, function(i) scatter_plot(data_list[[i]], paste(method_name, "- Set", i), color))
}

# Load synthetic datasets into lists
IST_3_synthpop_list <- lapply(1:10, function(i) read.csv(paste0("IST-3 Data/Data/synthpop/syn_data_small_synthpop_", i, ".csv")))
IST_3_arf_list <- lapply(1:10, function(i) read.csv(paste0("IST-3 Data/Data/arf/syn_data_small_arf_", i, ".csv")))
IST_3_privbayes_list <- lapply(1:10, function(i) read.csv(paste0("IST-3 Data/Data/privbayes/syn_data_small_privbayes_", i, ".csv")))
IST_3_ctgan_list <- lapply(1:10, function(i) read.csv(paste0("IST-3 Data/Data/ctgan/syn_data_small_ctgan_", i, ".csv")))

# Generate 10 plots per method
synthpop_plots <- generate_plots(IST_3_synthpop_list, "Synthpop", "red", num_plots = 10)
arf_plots <- generate_plots(IST_3_arf_list, "ARF", "blue", num_plots = 10)
privbayes_plots <- generate_plots(IST_3_privbayes_list, "PrivBayes", "green", num_plots = 10)
ctgan_plots <- generate_plots(IST_3_ctgan_list, "CTGAN", "purple", num_plots = 10)

# Arrange all plots in a 4x10 grid (4 methods, 10 datasets each)
gridExtra::grid.arrange(grobs = c(synthpop_plots, arf_plots, privbayes_plots, ctgan_plots), ncol = 10)




# 3. Aggregating - but I do not like that option