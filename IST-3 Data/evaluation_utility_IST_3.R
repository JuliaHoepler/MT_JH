#...............................................................................
# LOAD NECESSARY LIBRARIES ####
#...............................................................................

library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(caret)  
library(Metrics) 
library(synthpop)




#...............................................................................
# SOURCE AN EXTERNAL FUNCTION SCRIPT ####
#...............................................................................

source("IST-3 Data/functions.R")




#...............................................................................
# IMPORT DATA SETS ####
#...............................................................................

## Original data ####
data_small <- readRDS("IST-3 Data/Raw Data/data_small.Rds")



## Synthpop ####

# List all CSV files in the directory
csv_files <- list.files(path = "IST-3 Data/Data/synthpop/", pattern = "*.csv", full.names = TRUE)

# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_data_small_synthpop_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



## ARF ####
# List all CSV files in the directory
csv_files <- list.files(path = "IST-3 Data/Data/arf/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_data_small_arf_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



## PrivBayes ####

# List all CSV files in the directory
csv_files <- list.files(path = "IST-3 Data/Data/privbayes/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_data_small_privbayes_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



## CTGAN ####

# List all CSV files in the directory
csv_files <- list.files(path = "IST-3 Data/Data/ctgan/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_data_small_ctgan_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}


## TVAE ####

# List all CSV files in the directory
csv_files <- list.files(path = "IST-3 Data/Data/tvae/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_data_small_tvae_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}




## TABSYN ####

# List all CSV files in the directory
csv_files <- list.files(path = "IST-3 Data/Data/tabsyn/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_data_small_tabsyn_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



#...............................................................................
# DATA PROCSESSING ####
#...............................................................................



## PrivBayes ####

# Create lists of datasets
privbayes_datasets <- list.files(path = "IST-3 Data/Data/privbayes/", 
                                 pattern = "^syn_data_small_privbayes_", 
                                 full.names = F)

# Remove the ".csv" extension
privbayes_datasets <- sub("\\.csv$", "", privbayes_datasets)


# Filter datasets from 1 to 5
privbayes_datasets_m5 <- privbayes_datasets[grepl("^syn_data_small_privbayes_([1-5])$", privbayes_datasets)]

# Filter datasets from 1 to 10
privbayes_datasets_m10 <- privbayes_datasets[grepl("^syn_data_small_privbayes_(1|2|3|4|5|6|7|8|9|10)$", privbayes_datasets)]

# Filter datasets from 1 to 50
privbayes_datasets_m50 <- privbayes_datasets[grepl("^syn_data_small_privbayes_([1-9]|[1-4][0-9]|50)$", privbayes_datasets)]





## CTGAN ####

# Create lists of datasets
ctgan_datasets <- list.files(path = "IST-3 Data/Data/ctgan/", 
                             pattern = "^syn_data_small_ctgan_", 
                             full.names = F)

# Remove the ".csv" extension
ctgan_datasets <- sub("\\.csv$", "", ctgan_datasets)


# Filter datasets from 1 to 5
ctgan_datasets_m5 <- ctgan_datasets[grepl("^syn_data_small_ctgan_([1-5])$", ctgan_datasets)]

# Filter datasets from 1 to 10
ctgan_datasets_m10 <- ctgan_datasets[grepl("^syn_data_small_ctgan_(1|2|3|4|5|6|7|8|9|10)$", ctgan_datasets)]

# Filter datasets from 1 to 50
ctgan_datasets_m50 <- ctgan_datasets[grepl("^syn_data_small_ctgan_([1-9]|[1-4][0-9]|50)$", ctgan_datasets)]





## ARF ####

# Create lists of datasets
arf_datasets <- list.files(path = "IST-3 Data/Data/arf/", 
                           pattern = "^syn_data_small_arf_", 
                           full.names = F)

# Remove the ".csv" extension
arf_datasets <- sub("\\.csv$", "", arf_datasets)


# Filter datasets from 1 to 5
arf_datasets_m5 <- arf_datasets[grepl("^syn_data_small_arf_([1-5])$", arf_datasets)]

# Filter datasets from 1 to 10
arf_datasets_m10 <- arf_datasets[grepl("^syn_data_small_arf_(1|2|3|4|5|6|7|8|9|10)$", arf_datasets)]

# Filter datasets from 1 to 50
arf_datasets_m50 <- arf_datasets[grepl("^syn_data_small_arf_([1-9]|[1-4][0-9]|50)$", arf_datasets)]




## Synthpop ####

# Create lists of datasets
synthpop_datasets <- list.files(path = "IST-3 Data/Data/synthpop/", 
                                pattern = "^syn_data_small_synthpop_", 
                                full.names = F)


# Remove the ".csv" extension
synthpop_datasets <- sub("\\.csv$", "", synthpop_datasets)


# Filter datasets from 1 to 5
synthpop_datasets_m5 <- synthpop_datasets[grepl("^syn_data_small_synthpop_([1-5])$", synthpop_datasets)]

# Filter datasets from 1 to 10
synthpop_datasets_m10 <- synthpop_datasets[grepl("^syn_data_small_synthpop_(1|2|3|4|5|6|7|8|9|10)$", synthpop_datasets)]

# Filter datasets from 1 to 50
synthpop_datasets_m50 <- synthpop_datasets[grepl("^syn_data_small_synthpop_([1-9]|[1-4][0-9]|50)$", synthpop_datasets)]



## TVAE ####

# Create lists of datasets
tvae_datasets <- list.files(path = "IST-3 Data/Data/tvae/", 
                              pattern = "^syn_data_small_tvae_", 
                              full.names = F)


# Remove the ".csv" extension
tvae_datasets <- sub("\\.csv$", "", tvae_datasets)


# Filter datasets from 1 to 5
tvae_datasets_m5 <- tvae_datasets[grepl("^syn_data_small_tvae_([1-5])$", tvae_datasets)]

# Filter datasets from 1 to 10
tvae_datasets_m10 <- tvae_datasets[grepl("^syn_data_small_tvae_(1|2|3|4|5|6|7|8|9|10)$", tvae_datasets)]

# Filter datasets from 1 to 50
tvae_datasets_m50 <- tvae_datasets[grepl("^syn_data_small_tvae_([1-9]|[1-4][0-9]|50)$", tvae_datasets)]







## TABSYN ####


# for (i in 1:50) {
#   old_name <- paste0("IST-3 Data/Data/tabsyn/syn_IST-3_full_TabSyn_", i)
#   new_name <- paste0("IST-3 Data/Data/tabsyn/syn_data_small_tabsyn_", i)
# 
#   file.rename(old_name, new_name)
# }
# 
# folder <- "IST-3 Data/Data/tabsyn"
# 
# for (i in 1:50) {
#   old_name <- file.path(folder, paste0("syn_data_small_tabsyn_", i))
#   new_name <- paste0(old_name, ".csv")
# 
#   if (file.exists(old_name)) {
#     file.rename(old_name, new_name)
#   }
# }


# Create lists of datasets
tabsyn_datasets <- list.files(path = "IST-3 Data/Data/tabsyn/", 
                                pattern = "^syn_data_small_tabsyn_", 
                                full.names = F)


# Remove the ".csv" extension
tabsyn_datasets <- sub("\\.csv$", "", tabsyn_datasets)


# Filter datasets from 1 to 5
tabsyn_datasets_m5 <- tabsyn_datasets[grepl("^syn_data_small_tabsyn_([1-5])$", tabsyn_datasets)]

# Filter datasets from 1 to 10
tabsyn_datasets_m10 <- tabsyn_datasets[grepl("^syn_data_small_tabsyn_(1|2|3|4|5|6|7|8|9|10)$", tabsyn_datasets)]

# Filter datasets from 1 to 50
tabsyn_datasets_m50 <- tabsyn_datasets[grepl("^syn_data_small_tabsyn_([1-9]|[1-4][0-9]|50)$", tabsyn_datasets)]



#...............................................................................
# COMBINATION OF ALL DATASETS TO LISTS ####
#...............................................................................


## m = 5 ####

all_datasets_m5 <- c("data_small", privbayes_datasets_m5, arf_datasets_m5, synthpop_datasets_m5, ctgan_datasets_m5, tvae_datasets_m5, tabsyn_datasets_m5)
all_datasets_m5_wo_original <- c(privbayes_datasets_m5, arf_datasets_m5, synthpop_datasets_m5, ctgan_datasets_m5, tvae_datasets_m5, tabsyn_datasets_m5)

# Create an empty list to store the datasets
datasets_m5 <- list()
datasets_m5_wo_original <- list()


# Loop through the dataset names in 'all_datasets'
for (dataset_name in all_datasets_m5) {
  # Assuming that the datasets are loaded in the environment with the same name as the string in 'all_datasets'
  datasets_m5[[dataset_name]] <- get(dataset_name)
}

for (dataset_name in all_datasets_m5_wo_original) {
  # Assuming that the datasets are loaded in the environment with the same name as the string in 'all_datasets'
  datasets_m5_wo_original[[dataset_name]] <- get(dataset_name)
}


# Wandle die folgenden Variablen um
datasets_m5 <- lapply(datasets_m5, function(df) {
  df$outcome <- as.factor(df$outcome)
  df$itt_treat <- as.factor(df$itt_treat)
  df$vis_infarct <- as.factor(df$vis_infarct)
  return(df)
})


datasets_m5_wo_original <- lapply(datasets_m5_wo_original, function(df) {
  df$outcome <- as.factor(df$outcome)
  df$itt_treat <- as.factor(df$itt_treat)
  df$vis_infarct <- as.factor(df$vis_infarct)
  return(df)
})





## m = 10 ####

all_datasets_m10 <- c("data_small", privbayes_datasets_m10, arf_datasets_m10, synthpop_datasets_m10, ctgan_datasets_m10, tvae_datasets_m10, tabsyn_datasets_m10)
all_datasets_m10_wo_original <- c(privbayes_datasets_m10, arf_datasets_m10, synthpop_datasets_m10, ctgan_datasets_m10, tvae_datasets_m10, tabsyn_datasets_m10)

# Create an empty list to store the datasets
datasets_m10 <- list()
datasets_m10_wo_original <- list()


# Loop through the dataset names in 'all_datasets'
for (dataset_name in all_datasets_m10) {
  # Assuming that the datasets are loaded in the environment with the same name as the string in 'all_datasets'
  datasets_m10[[dataset_name]] <- get(dataset_name)
}

for (dataset_name in all_datasets_m10_wo_original) {
  # Assuming that the datasets are loaded in the environment with the same name as the string in 'all_datasets'
  datasets_m10_wo_original[[dataset_name]] <- get(dataset_name)
}


# Wandle die folgenden Variablen um
datasets_m10 <- lapply(datasets_m10, function(df) {
  df$outcome <- as.factor(df$outcome)
  df$itt_treat <- as.factor(df$itt_treat)
  df$vis_infarct <- as.factor(df$vis_infarct)
  return(df)
})


datasets_m10_wo_original <- lapply(datasets_m10_wo_original, function(df) {
  df$outcome <- as.factor(df$outcome)
  df$itt_treat <- as.factor(df$itt_treat)
  df$vis_infarct <- as.factor(df$vis_infarct)
  return(df)
})




## m = 50 ####


all_datasets_m50 <- c("data_small", privbayes_datasets_m50, arf_datasets_m50, synthpop_datasets_m50, ctgan_datasets_m50, tvae_datasets_m50, tabsyn_datasets_m50)
all_datasets_m50_wo_original <- c(privbayes_datasets_m50, arf_datasets_m50, synthpop_datasets_m50, ctgan_datasets_m50, tvae_datasets_m50, tabsyn_datasets_m50)

# Create an empty list to store the datasets
datasets_m50 <- list()
datasets_m50_wo_original <- list()


# Loop through the dataset names in 'all_datasets'
for (dataset_name in all_datasets_m50) {
  # Assuming that the datasets are loaded in the environment with the same name as the string in 'all_datasets'
  datasets_m50[[dataset_name]] <- get(dataset_name)
}

for (dataset_name in all_datasets_m50_wo_original) {
  # Assuming that the datasets are loaded in the environment with the same name as the string in 'all_datasets'
  datasets_m50_wo_original[[dataset_name]] <- get(dataset_name)
}


# Wandle die folgenden Variablen um
datasets_m50 <- lapply(datasets_m50, function(df) {
  df$outcome <- as.factor(df$outcome)
  df$itt_treat <- as.factor(df$itt_treat)
  df$vis_infarct <- as.factor(df$vis_infarct)
  return(df)
})


datasets_m50_wo_original <- lapply(datasets_m50_wo_original, function(df) {
  df$outcome <- as.factor(df$outcome)
  df$itt_treat <- as.factor(df$itt_treat)
  df$vis_infarct <- as.factor(df$vis_infarct)
  return(df)
})







#...............................................................................
# EVALUATION OF UTILITY ####
#...............................................................................

## 1. Fit-for-purpose measures ####

### All variables plotted at once per m-value and per method ####

my_seed <- 3105

# Define methods and corresponding folder names
methods <- list(
  "synthpop" = "synthpop",
  "arf" = "arf",
  "privbayes" = "privbayes",
  "ctgan" = "ctgan",
  "tvae" = "tvae",
  "tabsyn" = "tabsyn"
)

# Define m values
m_values <- c(5, 10, 50)

# Process each method
for (method in names(methods)) {
  process_and_plot(method, m_values, methods[[method]])
}



#...............................................................................

### Side-by-side comparison of each variale per method and m-value ####

output_directory <- "IST-3 Data/evaluation_utility/fit_for_purpose_utility/"
variables <- c("outcome", "itt_treat", "nihss", "randdelay", "vis_infarct", "age")
m_values <- c(5, 10, 50)

for (var in variables) {
  
  generate_comparison_plot(var, 
                           datasets_m5_wo_original, 
                           datasets_m10_wo_original, 
                           datasets_m50_wo_original, 
                           data_small, 
                           output_directory, 
                           m_values)
  
}







## 2. Outcome-specific utility ####

# Aim: For every synthesized data set, a glm should be fitted and then
# the regression coefficients of the model on the original data and the 
# model on the synthesized data sets should be plotted for comparison


### Regression coefficients ####

# Run the function for different values of m
evaluate_model(datasets = datasets_m5, 
               m = 5, 
               output_path = "IST-3 Data/evaluation_utility/outcome_specific_utility/regression_coeff_plot_combined_m5.pdf")
evaluate_model(datasets = datasets_m10, 
               m = 10, 
               output_path = "IST-3 Data/evaluation_utility/outcome_specific_utility/regression_coeff_plot_combined_m10.pdf")
evaluate_model(datasets = datasets_m50, 
               m = 50, 
               output_path = "IST-3 Data/evaluation_utility/outcome_specific_utility/regression_coeff_plot_combined_m50.pdf")






## 3. Global utility ####

# Look at the standardized pMSE and the pMSE ratio

# Define methods and sample sizes
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")
sample_sizes <- c(5, 10, 50)

# Compute results for all methods and sample sizes
results <- expand.grid(Method = methods, SampleSize = sample_sizes)
results$S_pMSE <- mapply(compute_s_pMSE, results$Method, results$SampleSize)

print(results)

write.csv(results, "IST-3 Data/evaluation_utility/global_utility/global_utility_s_pMSE.csv", row.names = FALSE)



# Radar plots ####

library(fmsb)

# Raw data
raw_data <- data.frame(
  row.names = c("ARF", "CTGAN", "PrivBayes", "Synthpop", "TABSYN", "TVAE"),
  C2ST_AUC = c(0.723, 0.904, 0.696, 0.550, 0.605, 0.905),
  S_pMSE = c(243.277, 797.856, 335.188, 240.991, 250.205, 332.434),
  Alpha_Precision = c(0.898, 0.951, 0.561, 0.901, 0.899, 0.960),
  Beta_Recall = c(0.902, 0.836, 0.976, 0.897, 0.900, 0.830),
  CIO = c(0.879, 0.550, 0.285, 0.893, 0.856, 0.413),
  MASD = c(0.205, 4.060, 16.300, 0.601, 0.072, 10.600),
  R2 = c(0.750, 0.719, 0.519, 0.747, 0.753, 0.710)
)


# Function to rank (reverse scale: best rank 1 outward)
rank_reversed <- function(x, invert = FALSE) {
  if (invert) {
    return(rank(x))  # smaller error is better
  } else {
    return(rank(-x)) # larger better
  }
}

# Create the ranking
ranked_data <- data.frame(
  C2ST_AUC = rank_reversed(raw_data$C2ST_AUC, invert = TRUE),
  alpha_Precision = rank_reversed(raw_data$Alpha_Precision),
  beta_Recall = rank_reversed(raw_data$Beta_Recall),
  S_pMSE = rank_reversed(raw_data$S_pMSE, invert = TRUE),
  CIO = rank_reversed(raw_data$CIO),
  MASD = rank_reversed(raw_data$MASD, invert = TRUE),
  R2 = rank_reversed(raw_data$R2)
)

#ranked_data_inverted <- 7 - ranked_data

# Prepare radar data
radar_data_full <- rbind(
  rep(1, ncol(ranked_data)),  # max = 1
  rep(6, ncol(ranked_data)),  # min = 6
  ranked_data
)

# Set row names
row.names(radar_data_full) <- c("Max", "Min", row.names(raw_data))

# Custom colors
method_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")

# List of methods
methods <- row.names(raw_data)


labels <- c(
  "C2ST AUC",
  expression(alpha*"-Precision"),
  expression(beta*"-Recall"),
  "S_pMSE",
  "CIO",
  "MASD",
  expression(R^2)
)


# Open a PDF device to save all the plots in one file
pdf(file = "IST-3 Data/evaluation_utility/all_radar_plots.pdf", width = 9, height = 6)

# Set up the plotting area with 2 rows and 3 columns
par(mfrow = c(2, 3), mar = c(3, 3, 2, 2), oma = c(0, 0, 2, 0))

# Loop through each method and create a radar plot
for (i in 1:length(methods)) {
  method <- methods[i]
  
  # Only take max, min, and this method
  radar_data_one <- radar_data_full[c("Max", "Min", method), ]
  
  # Generate the radar plot for this method
  radarchart(
    radar_data_one,
    axistype = 1,
    vlabels = labels,
    pcol = method_colors[i],
    pfcol = scales::alpha(method_colors[i], 0.4),
    plwd = 3,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "black",
    caxislabels = 5:1,
    vlcex = 0.8,
    title = method
  )
}

# Close the PDF device (this saves all plots)
dev.off()





################################################################################

library(ggplot2)
library(patchwork)
library(dplyr)

# Your color scheme
method_colors <- c(
  "ARF" = "#66C2A5",
  "CTGAN" = "#FC8D62",
  "PrivBayes" = "#8DA0CB",
  "Synthpop" = "#E78AC3",
  "TABSYN" = "#A6D854",
  "TVAE" = "#FFD92F"
)

# Function to calculate S_pMSE
calculate_spmse <- function(observed, synthetic) {
  data_combined <- rbind(
    data.frame(observed, source = "observed"),
    data.frame(synthetic, source = "synthetic")
  )
  
  data_combined$source <- factor(data_combined$source, levels = c("observed", "synthetic"))
  
  # Logistic regression
  model <- glm(source ~ ., family = binomial, data = data_combined)
  
  preds <- predict(model, type = "response")
  
  spmse <- mean((preds - 0.5)^2)
  return(spmse)
}

# Main plotting function
generate_manual_plots <- function(variable, synthetic_datasets_list, observed_data, output_path) {
  
  plots_list <- list()
  
  # Loop over each synthetic method
  for (method_name in names(synthetic_datasets_list)) {
    
    synthetic_data <- synthetic_datasets_list[[method_name]]
    
    # Calculate S_pMSE
    observed_subset <- observed_data[, ..variable]
    synthetic_subset <- synthetic_data[, ..variable]
    
    
    colnames(observed_subset) <- "var"
    colnames(synthetic_subset) <- "var"
    
    spmse_value <- calculate_spmse(observed_subset, synthetic_subset)
    
    # Check if variable is continuous or categorical
    if (is.numeric(observed_subset$var)) {
      # Density plot for continuous
      p <- ggplot() +
        geom_density(data = observed_subset, aes(x = var, fill = "Observed"), alpha = 0.5) +
        geom_density(data = synthetic_subset, aes(x = var, fill = "Synthetic"), alpha = 0.5) +
        scale_fill_manual(values = c("Observed" = "black", "Synthetic" = method_colors[method_name])) +
        labs(title = paste0(method_name, " | S_pMSE = ", round(spmse_value, 2)),
             x = variable, y = "Density") +
        theme_minimal()
      
    } else {
      # Bar plot for categorical
      observed_freq <- observed_subset %>% count(var) %>% mutate(source = "Observed")
      synthetic_freq <- synthetic_subset %>% count(var) %>% mutate(source = "Synthetic")
      
      combined_freq <- rbind(observed_freq, synthetic_freq)
      
      p <- ggplot(combined_freq, aes(x = var, y = n, fill = source)) +
        geom_col(position = "dodge", alpha = 0.7) +
        scale_fill_manual(values = c("Observed" = "black", "Synthetic" = method_colors[method_name])) +
        labs(title = paste0(method_name, " | S_pMSE = ", round(spmse_value, 2)),
             x = variable, y = "Count") +
        theme_minimal()
    }
    
    plots_list[[method_name]] <- p
  }
  
  # Combine all method plots into one
  final_plot <- wrap_plots(plots_list, ncol = 2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  # Save the plot
  ggsave(filename = paste0(output_path, "manual_plot_", variable, ".pdf"),
         plot = final_plot, height = 8, width = 12)
}

generate_manual_plots("vis_infarct", datasets_m5_wo_original, data_small, "IST-3 Data/evaluation_utility/fit_for_purpose_utility/")
