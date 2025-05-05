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


# C2ST 
more_criteria_IST_3_c2st <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_evaluation_c2st_avg.csv")
more_criteria_IST_3_mean_c2st <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_evaluation_mean_c2st_avg.csv")


# ML utility
more_criteria_IST_3_ml_utility <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_ML_utility.csv")
more_criteria_IST_3_mean_ml_utility <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_mean_ML_utility.csv")


# SIR
more_criteria_IST_3_sir <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_identical_records.csv")
more_criteria_IST_3_mean_sir <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_mean_identical_records.csv")


# DCR
more_criteria_IST_3_dcr <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_dcr.csv")
more_criteria_IST_3_mean_dcr <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_dcr_mean.csv")


# NNDR
more_criteria_IST_3_nndr <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_nndr.csv")
more_criteria_IST_3_mean_nndr <- read.csv("IST-3 Data/evaluation_more_criteria/synthetic_data_mean_nndr.csv")



#...............................................................................
# Classifier two-sample test (C2ST) ####
#...............................................................................

# Boxplot erstellen
plot_IST_3_c2st <-ggplot(more_criteria_IST_3_c2st, aes(x = factor(M), y = C2ST_Accuracy, fill = Method)) +
  geom_boxplot() +
  labs(title = "Vergleich der C2ST-Accuracy nach Methode und Anzahl synthetischer Datensätze",
       x = "M",
       y = "Mean Accuracy") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 1))  # Setze die y-Achse von 0 bis 1
plot_IST_3_c2st


ggsave(filename = paste0("IST-3 Data/evaluation_more_criteria/plot_IST_3_c2st.pdf"), 
       plot = plot_IST_3_c2st, height = 15, width = 10)




#...............................................................................
# ML utility ####
#...............................................................................

# Boxplot erstellen - R^2
plot_IST_3_ml_utility_r_squared <- ggplot(more_criteria_IST_3_ml_utility, aes(x = factor(M), y = R2, fill = Method)) +
  geom_boxplot() +
  labs(title = "Vergleich der R^2-Werte nach Methode und Anzahl synthetischer Datensätze",
       x = "M",
       y = "Mean ML utility") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") 
plot_IST_3_ml_utility_r_squared

ggsave(filename = paste0("IST-3 Data/evaluation_more_criteria/plot_IST_3_ml_utility_r_squared.pdf"), 
       plot = plot_IST_3_ml_utility_r_squared, height = 15, width = 10)

 


# Boxplot erstellen - RMSE 
plot_IST_3_ml_utility_rmse <- ggplot(more_criteria_IST_3_ml_utility, aes(x = factor(M), y = RMSE, fill = Method)) +
  geom_boxplot() +
  labs(title = "Vergleich der RMSE-Werte nach Methode und Anzahl synthetischer Datensätze",
       x = "M",
       y = "Mean ML utility") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") 
plot_IST_3_ml_utility_rmse



# Boxplot erstellen - MAE
plot_IST_3_ml_utility_mae <- ggplot(more_criteria_IST_3_ml_utility, aes(x = factor(M), y = MAE, fill = Method)) +
  geom_boxplot() +
  labs(title = "Vergleich der MAE-Werte nach Methode und Anzahl synthetischer Datensätze",
       x = "M",
       y = "Mean ML utility") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") 
plot_IST_3_ml_utility_mae


#...............................................................................
# SIR ####
#...............................................................................

ggplot(more_criteria_IST_3_mean_sir, aes(x = factor(M), y = Mean_Identical_Share, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Mean Share of Identical Records (SIR) Across Methods",
    x = "Number of Synthetic Datasets (M)",
    y = "Mean SIR Value"
  ) +
  scale_fill_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")) + # Pastellfarben
  #scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00")) + # Kontrastreiche Palette
  theme(legend.position = "right")


# KEY OBSERVATIONS

# ARF & CTGAN have no identical records at all (SIR = 0)

# PrivBayes: Very low SIR (0.4% - 0.5%)

# Synthpop has extremely high SIR (~12%)



#...............................................................................
# DCR ####
#...............................................................................

plot_IST_3_dcr <- ggplot(more_criteria_IST_3_dcr, aes(x = Method, y = DCR, fill = as.factor(M))) +
  geom_boxplot() +
  labs(title = "DCR Distribution Across Methods and Synthetic Dataset Sizes",
       x = "Method",
       y = "DCR Value",
       fill = "Number of Synthetic Datasets (M)") +
  theme_minimal()

ggsave(filename = paste0("IST-3 Data/evaluation_more_criteria/plot_IST_3_dcr.pdf"), 
       plot = plot_IST_3_dcr, height = 15, width = 10)


# KEY OBSERVATIONS

# DCR interpretation:

# - Negative DCR values suggest that, on average, synthetic records are closer to 
#   the real training set than to the holdout set.

# - More negative values may indicate potential overfitting to the training data.

# - Ideally, DCR should be close to zero or slightly positive to ensure the
#   synthetic data generalized well to unseen records.


# Comparison of methods:

# - ARF & Synthpop tend to have higher (less negative) DCR values, suggesting
#   they generalize better than CTGAN.

# - CTGAN shows the lowest (most negative) DCR values, indicating higher overfitting,
#   as synthetic records are closer to real training data than holdout data.

# - PrivBayes has the leats variance in DCR, indicating a better generalization.


# Effect of m (number of Synthetic Datasets):
  
# - In general, larger m values (more synthetic datasets) tend to reduce the spread 
#   of DCR values.

# - CTGAN and Synthpop show the strongest dependency on m (large changes in DCR as m increases).

# - PrivBayes is the most stable across different m values.



#  What Does This Mean?

# CTGAN shows the highest overfitting, especially at lower M values.
# PrivBayes performs consistently well, meaning it produces synthetic data that generalizes better.
# Increasing m (more synthetic datasets) generally improves generalization, but the effect varies across methods.
# ARF and Synthpop are intermediate, but Synthpop shows higher variability, suggesting it might be less stable.




#...............................................................................
# NNDR ####
#...............................................................................

# Boxplot erstellen
plot_IST_3_nndr <- ggplot(more_criteria_IST_3_nndr, aes(x = factor(M), y = Mean_NNDR_Filtered, fill = Method)) +
  geom_boxplot() +
  labs(title = "NNDR-Verteilung nach Methode und M",
       x = "M",
       y = "Mean NNDR Filtered") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") 

ggsave(filename = paste0("IST-3 Data/evaluation_more_criteria/plot_IST_3_nndr.pdf"), 
       plot = plot_IST_3_nndr, height = 15, width = 10)


# KEY OBSERVATIONS

# Interpretation in general:
#    - NNDR close to 1: The nearest and second-nearest neighbors are almost equally
#      distant, meaning the synthetic data does not strongly resemble a specific 
#      real point. Suggest good privacy.

#    - NNDR close to 0: The nearest neighbor is much closer than the second-nearest,
#      meaning the synthetic data point is highly similar to one real data point. This
#      suggests higher privacy risk.

#    - Moderate NNDR: More balanced privacy and resemblance


# Here: 

# CTGAN has the highest NNDR (~0.75) → Suggests better utility/resemblance but worse privacy, 
# as synthetic data closely resemble real data.

# Synthpop has the lowest NNDR (~0.55-0.58) → Suggests better privacy but worse utility, 
# as synthetic data are more distinct.

# ARF and PrivBayes are in the middle (~0.68-0.70) → Offer a balance between privacy and utility.


