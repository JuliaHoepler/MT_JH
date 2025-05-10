#...............................................................................
# LOAD NECESSARY LIBRARIES ####
#...............................................................................

library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(caret)  
library(Metrics) 
library(synthpop)
library(patchwork)



#...............................................................................
# SOURCE AN EXTERNAL FUNCTION SCRIPT ####
#...............................................................................

source("Medical Insurance Cost Data/functions.R")




#...............................................................................
# IMPORT DATA SETS ####
#...............................................................................

## Original data ####
medical_insurance <- read.csv("Medical Insurance Cost Data/Raw Data/medical_insurance.csv")


# Some data processing
medical_insurance <- medical_insurance %>%
  mutate(sex = as.factor(sex),
         smoker = as.factor(smoker),
         region = as.factor(region),
         children = as.factor(children))



## Synthpop ####

# List all CSV files in the directory
csv_files <- list.files(path = "Medical Insurance Cost Data/Data/synthpop/", pattern = "*.csv", full.names = TRUE)

# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_medical_insurance_synthpop_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



## ARF ####
# List all CSV files in the directory
csv_files <- list.files(path = "Medical Insurance Cost Data/Data/arf/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_medical_insurance_arf_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



## PrivBayes ####

# List all CSV files in the directory
csv_files <- list.files(path = "Medical Insurance Cost Data/Data/privbayes/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_medical_insurance_privbayes_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



## CTGAN ####

# List all CSV files in the directory
csv_files <- list.files(path = "Medical Insurance Cost Data/Data/ctgan/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_medical_insurance_ctgan_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}




## TVAE ####

# List all CSV files in the directory
csv_files <- list.files(path = "Medical Insurance Cost Data/Data/tvae/", pattern = "*.csv", full.names = TRUE)


# Loop over each file and dynamically create variable names and import data
for (i in seq_along(csv_files)) {
  # Create dynamic variable name based on file index
  var_name <- paste0("syn_medical_insurance_tvae_", i)
  
  # Assign the imported data to the dynamic variable name
  assign(var_name, read.csv(csv_files[i]))
}



## TABSYN ####

# for (i in 1:50) {
#   old_name <- paste0("Medical Insurance Cost Data/Data/tabsyn/syn_medical_insurance_full_TabSyn_", i)
#   new_name <- paste0("Medical Insurance Cost Data/Data/tabsyn/syn_medical_insurance_tabsyn_", i)
# 
#   file.rename(old_name, new_name)
# }
# 
# folder <- "Medical Insurance Cost Data/Data/tabsyn"
# 
# for (i in 1:50) {
#   old_name <- file.path(folder, paste0("syn_medical_insurance_tabsyn_", i))
#   new_name <- paste0(old_name, ".csv")
# 
#   if (file.exists(old_name)) {
#     file.rename(old_name, new_name)
#   }
# }



# List all CSV files in the directory
csv_files <- list.files(path = "Medical Insurance Cost Data/Data/tabsyn/", pattern = "*.csv", full.names = TRUE)

for (file_path in csv_files) {
  # Extract the number from the filename
  num <- sub(".*_tabsyn_(\\d+)\\.csv", "\\1", file_path)
  
  # Build variable name using that number
  var_name <- paste0("syn_medical_insurance_tabsyn_", num)
  
  # Read the CSV and assign it to the dynamically created variable
  assign(var_name, read.csv(file_path))
}


# Loop over each dataset (from 1 to 50)
for (i in 1:50) {
  # Create the variable name dynamically
  var_name <- paste0("syn_medical_insurance_tabsyn_", i)
  
  # Get the dataset by name
  df <- get(var_name)
  
  # Convert the 'children' column to a factor
  df$children <- as.integer(df$children)
  
  # Reassign the modified dataset to the original variable name
  assign(var_name, df)
}




#...............................................................................
# DATA PROCSESSING ####
#...............................................................................



## PrivBayes ####

# Create lists of datasets
privbayes_datasets <- list.files(path = "Medical Insurance Cost Data/Data/privbayes/", 
                                    pattern = "^syn_medical_insurance_privbayes_", 
                                    full.names = F)

# Remove the ".csv" extension
privbayes_datasets <- sub("\\.csv$", "", privbayes_datasets)


# Filter datasets from 1 to 5
privbayes_datasets_m5 <- privbayes_datasets[grepl("^syn_medical_insurance_privbayes_([1-5])$", privbayes_datasets)]

# Filter datasets from 1 to 10
privbayes_datasets_m10 <- privbayes_datasets[grepl("^syn_medical_insurance_privbayes_(1|2|3|4|5|6|7|8|9|10)$", privbayes_datasets)]

# Filter datasets from 1 to 50
privbayes_datasets_m50 <- privbayes_datasets[grepl("^syn_medical_insurance_privbayes_([1-9]|[1-4][0-9]|50)$", privbayes_datasets)]





## CTGAN ####

# Create lists of datasets
ctgan_datasets <- list.files(path = "Medical Insurance Cost Data/Data/ctgan/", 
                                 pattern = "^syn_medical_insurance_ctgan_", 
                                 full.names = F)

# Remove the ".csv" extension
ctgan_datasets <- sub("\\.csv$", "", ctgan_datasets)


# Filter datasets from 1 to 5
ctgan_datasets_m5 <- ctgan_datasets[grepl("^syn_medical_insurance_ctgan_([1-5])$", ctgan_datasets)]

# Filter datasets from 1 to 10
ctgan_datasets_m10 <- ctgan_datasets[grepl("^syn_medical_insurance_ctgan_(1|2|3|4|5|6|7|8|9|10)$", ctgan_datasets)]

# Filter datasets from 1 to 50
ctgan_datasets_m50 <- ctgan_datasets[grepl("^syn_medical_insurance_ctgan_([1-9]|[1-4][0-9]|50)$", ctgan_datasets)]





## ARF ####

# Create lists of datasets
arf_datasets <- list.files(path = "Medical Insurance Cost Data/Data/arf/", 
                             pattern = "^syn_medical_insurance_arf_", 
                             full.names = F)

# Remove the ".csv" extension
arf_datasets <- sub("\\.csv$", "", arf_datasets)


# Filter datasets from 1 to 5
arf_datasets_m5 <- arf_datasets[grepl("^syn_medical_insurance_arf_([1-5])$", arf_datasets)]

# Filter datasets from 1 to 10
arf_datasets_m10 <- arf_datasets[grepl("^syn_medical_insurance_arf_(1|2|3|4|5|6|7|8|9|10)$", arf_datasets)]

# Filter datasets from 1 to 50
arf_datasets_m50 <- arf_datasets[grepl("^syn_medical_insurance_arf_([1-9]|[1-4][0-9]|50)$", arf_datasets)]




## Synthpop ####

# Create lists of datasets
synthpop_datasets <- list.files(path = "Medical Insurance Cost Data/Data/synthpop/", 
                             pattern = "^syn_medical_insurance_synthpop_", 
                             full.names = F)


# Remove the ".csv" extension
synthpop_datasets <- sub("\\.csv$", "", synthpop_datasets)


# Filter datasets from 1 to 5
synthpop_datasets_m5 <- synthpop_datasets[grepl("^syn_medical_insurance_synthpop_([1-5])$", synthpop_datasets)]

# Filter datasets from 1 to 10
synthpop_datasets_m10 <- synthpop_datasets[grepl("^syn_medical_insurance_synthpop_(1|2|3|4|5|6|7|8|9|10)$", synthpop_datasets)]

# Filter datasets from 1 to 50
synthpop_datasets_m50 <- synthpop_datasets[grepl("^syn_medical_insurance_synthpop_([1-9]|[1-4][0-9]|50)$", synthpop_datasets)]





## TVAE ####

# Create lists of datasets
tvae_datasets <- list.files(path = "Medical Insurance Cost Data/Data/tvae/", 
                                pattern = "^syn_medical_insurance_tvae_", 
                                full.names = F)


# Remove the ".csv" extension
tvae_datasets <- sub("\\.csv$", "", tvae_datasets)


# Filter datasets from 1 to 5
tvae_datasets_m5 <- tvae_datasets[grepl("^syn_medical_insurance_tvae_([1-5])$", tvae_datasets)]

# Filter datasets from 1 to 10
tvae_datasets_m10 <- tvae_datasets[grepl("^syn_medical_insurance_tvae_(1|2|3|4|5|6|7|8|9|10)$", tvae_datasets)]

# Filter datasets from 1 to 50
tvae_datasets_m50 <- tvae_datasets[grepl("^syn_medical_insurance_tvae_([1-9]|[1-4][0-9]|50)$", tvae_datasets)]



## TabSyn ####

# Create lists of datasets
tabsyn_datasets <- list.files(path = "Medical Insurance Cost Data/Data/tabsyn/", 
                            pattern = "^syn_medical_insurance_tabsyn_", 
                            full.names = F)


# Remove the ".csv" extension
tabsyn_datasets <- sub("\\.csv$", "", tabsyn_datasets)


# Filter datasets from 1 to 5
tabsyn_datasets_m5 <- tabsyn_datasets[grepl("^syn_medical_insurance_tabsyn_([1-5])$", tabsyn_datasets)]

# Filter datasets from 1 to 10
tabsyn_datasets_m10 <- tabsyn_datasets[grepl("^syn_medical_insurance_tabsyn_(1|2|3|4|5|6|7|8|9|10)$", tabsyn_datasets)]

# Filter datasets from 1 to 50
tabsyn_datasets_m50 <- tabsyn_datasets[grepl("^syn_medical_insurance_tabsyn_([1-9]|[1-4][0-9]|50)$", tabsyn_datasets)]


#...............................................................................
# COMBINATION OF ALL DATASETS TO LISTS ####
#...............................................................................


## m = 5 ####

all_datasets_m5 <- c("medical_insurance", privbayes_datasets_m5, arf_datasets_m5, synthpop_datasets_m5, ctgan_datasets_m5, tvae_datasets_m5, tabsyn_datasets_m5)
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
  df$sex <- as.factor(df$sex)
  df$children <- as.factor(as.integer(df$children))
  df$smoker <- as.factor(df$smoker)
  df$region <- as.factor(df$region)
  return(df)
})


datasets_m5_wo_original <- lapply(datasets_m5_wo_original, function(df) {
  df$sex <- as.factor(df$sex)
  df$children <- as.factor(as.integer(df$children))
  df$smoker <- as.factor(df$smoker)
  df$region <- as.factor(df$region)
  return(df)
})




## m = 10 ####

all_datasets_m10 <- c("medical_insurance", privbayes_datasets_m10, arf_datasets_m10, synthpop_datasets_m10, ctgan_datasets_m10, tvae_datasets_m10, tabsyn_datasets_m10)
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
  df$sex <- as.factor(df$sex)
  df$children <- as.factor(as.integer(df$children))
  df$smoker <- as.factor(df$smoker)
  df$region <- as.factor(df$region)
  return(df)
})


datasets_m10_wo_original <- lapply(datasets_m10_wo_original, function(df) {
  df$sex <- as.factor(df$sex)
  df$children <- as.factor(as.integer(df$children))
  df$smoker <- as.factor(df$smoker)
  df$region <- as.factor(df$region)
  return(df)
})




## m = 50 ####


all_datasets_m50 <- c("medical_insurance", privbayes_datasets_m50, arf_datasets_m50, synthpop_datasets_m50, ctgan_datasets_m50, tvae_datasets_m50, tabsyn_datasets_m50)
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
  df$sex <- as.factor(df$sex)
  df$children <- as.factor(as.integer(df$children))
  df$smoker <- as.factor(df$smoker)
  df$region <- as.factor(df$region)
  return(df)
})


datasets_m50_wo_original <- lapply(datasets_m50_wo_original, function(df) {
  df$sex <- as.factor(df$sex)
  df$children <- as.factor(as.integer(df$children))
  df$smoker <- as.factor(df$smoker)
  df$region <- as.factor(df$region)
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

output_directory <- "Medical Insurance Cost Data/evaluation_utility/fit_for_purpose_utility/"
variables <- c("age", "sex", "bmi", "children", "smoker", "region", "charges")
m_values <- c(5, 10, 50)

for (var in variables) {
  
  generate_comparison_plot(var, 
                           datasets_m5_wo_original, 
                           datasets_m10_wo_original, 
                           datasets_m50_wo_original, 
                           medical_insurance, 
                           output_directory, 
                           m_values)
  
}





## 2. Outcome-specific utility ####

# Aim: For every synthesized data set, a glm should be fitted and then
# the regression coefficients of the model on the original data and the 
# model on the synthesized data sets should be plotted for comparison

### Regression coefficients ####

datasets_m5 <- lapply(datasets_m5, function(df) {
  df$children <- as.integer(df$children)
  return(df)
})

datasets_m10 <- lapply(datasets_m10, function(df) {
  df$children <- as.integer(df$children)
  return(df)
})

datasets_m50 <- lapply(datasets_m50, function(df) {
  df$children <- as.integer(df$children)
  return(df)
})




# Run the function for different values of m
evaluate_model(datasets = datasets_m5, 
               m = 5, 
               output_path = "Medical Insurance Cost Data/evaluation_utility/outcome_specific_utility/regression_coeff_plot_combined_m5.pdf")
evaluate_model(datasets = datasets_m10, 
               m = 10, 
               output_path = "Medical Insurance Cost Data/evaluation_utility/outcome_specific_utility/regression_coeff_plot_combined_m10.pdf")
evaluate_model(datasets = datasets_m50, 
               m = 50, 
               output_path = "Medical Insurance Cost Data/evaluation_utility/outcome_specific_utility/regression_coeff_plot_combined_m50.pdf")







## 3. Global utility ####

# Look at the standardized pMSE

# Define methods and sample sizes
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")
sample_sizes <- c(5)

# Compute results for all methods and sample sizes
results <- expand.grid(Method = methods, SampleSize = sample_sizes)
results$S_pMSE <- mapply(compute_s_pMSE, results$Method, results$SampleSize)

print(results)

write.csv(results, "Medical Insurance Cost Data/evaluation_utility/global_utility/global_utility_s_pMSE.csv", row.names = FALSE)

read_csv("Medical Insurance Cost Data/evaluation_utility/global_utility/global_utility_s_pMSE.csv")






