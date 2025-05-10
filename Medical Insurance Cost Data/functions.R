
# Helpers ####

# ...............................................................................
# HELPER FUNCTION TO LOAD DATASETS
# ...............................................................................

# Function to load datasets from a directory based on a pattern
load_csv_files <- function(directory, pattern) {
  csv_files <- list.files(path = directory, pattern = pattern, full.names = TRUE)
  
  # Use lapply to read the files and store in a list
  datasets <- lapply(csv_files, read.csv)
  
  # Create names for datasets based on the file names
  names(datasets) <- gsub(".csv", "", basename(csv_files))
  
  return(datasets)
}





# Function to load all datasets for a given size (e.g., m5, m10, m50)
load_datasets_for_size <- function(size) {
  # Define the directory paths based on size
  synthpop_dir <- paste0("Medical Insurance Cost Data/Data/synthpop/m", size, "/")
  arf_dir <- paste0("Medical Insurance Cost Data/Data/arf/m", size, "/")
  privbayes_dir <- paste0("Medical Insurance Cost Data/Data/privbayes/m", size, "/")
  ctgan_dir <- paste0("Medical Insurance Cost Data/Data/ctgan/m", size, "/")
  
  # Load datasets based on size
  synthpop_datasets <- load_csv_files(synthpop_dir, paste0("^syn_medical_insurance_synthpop_", size))
  arf_datasets <- load_csv_files(arf_dir, paste0("^syn_medical_insurance_arf_", size))
  privbayes_datasets <- load_csv_files(privbayes_dir, paste0("^syn_medical_insurance_privbayes_", size))
  ctgan_datasets <- load_csv_files(ctgan_dir, paste0("^syn_medical_insurance_ctgan_", size))
  
  return(list(synthpop = synthpop_datasets, arf = arf_datasets, privbayes = privbayes_datasets, ctgan = ctgan_datasets))
}


# Helper function to convert columns to appropriate types
convert_columns <- function(df) {
  df$sex <- as.factor(df$sex)
  df$children <- as.integer(df$children)
  df$smoker <- as.factor(df$smoker)
  df$region <- as.factor(df$region)
  return(df)
}



# Synthetic data generation ####



## Synthpop ####

# Create synthetic datasets based on synthpop for the train datasets
generate_synthetic_data_synthpop <- function(data, reps, seed, folder, output_folder) {
  
  # Extract the name of the dataset dynamically
  data_name <- deparse(substitute(data))
  
  # set seed
  set.seed(seed)
  
  syn_data <- replicate(reps, syn(data)$syn, simplify = FALSE)
  
  lapply(seq_along(syn_data), \(i) {
    fwrite(syn_data[[i]], file = paste0(output_folder, "/syn_", data_name, "_synthpop_", i, ".csv"))
    
  })
}


# Create synthetic datasets based on synthpop for the train datasets
generate_synthetic_data_synthpop_train_data <- function(data, reps, seed, folder, output_folder) {
  
  # Extract the name of the dataset dynamically
  data_name <- deparse(substitute(data))
  
  # set seed
  set.seed(seed)
  
  syn_data <- replicate(reps, syn(data)$syn, simplify = FALSE)
  
  lapply(seq_along(syn_data), \(i) {
    fwrite(syn_data[[i]], file = paste0(output_folder, "/train_data/syn_", data_name, "_synthpop_", i, ".csv"))
    
  })
}





## ARF ####

# Function, to generate synthetic dataset with ARF with varying seed
generate_synthetic_datasets <- function(data, data_name, repetitions, output_folder, base_seed = 3105) {
  
  # Extract the name of the dataset dynamically
  data_name <- deparse(substitute(data))
  
  # Ensure that the output folder exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Loop over the number of repetitions
  for (i in 1:repetitions) {
    # Set the seed
    set.seed(base_seed + i)
    
    # Train ARF and estimate leaf parameters using the provided data
    arf <- adversarial_rf(x = data)
    psi <- forde(arf, data, finite_bounds = "global")
    
    # Generate synthetic samples (same number of synthetic samples as the original dataset)
    n_synth_samples <- nrow(data)  # Generate the same number of synthetic samples as the original dataset
    syn_data <- forge(psi, n_synth = n_synth_samples)
    
    # Save the synthetic dataset to the specified folder, including the actual name of the dataset in the filename
    file_name <- file.path(output_folder, paste0("syn_", data_name, "_arf_", i, ".csv"))
    write.csv(syn_data, file_name, row.names = FALSE)
  }
}




# Utility ####

## Fit-for-purpose utility ####

# Function to process and plot data
process_and_plot <- function(method, m_values, base_path) {
  # Iterate through different values of m
  for (m in m_values) {
    
    # m <- 5
    # method <- list(
    #   "tabsyn" = "tabsyn"
    # )
    # base_path <- method[["tabsyn"]]
    # 
    # Generate regex pattern to match files
    pattern <- paste0("syn_medical_insurance_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$")
    
    # List all relevant files
    dateien <- list.files(
      path = paste0("Medical Insurance Cost Data/Data/", base_path, "/"), 
      pattern = pattern, 
      full.names = TRUE
    )
    
    # Read CSV files
    syn_data <- lapply(dateien, read.csv)
    
    # Convert categorical variables
    syn_data <- lapply(syn_data, function(df) {
      df$sex <- as.factor(df$sex)
      df$smoker <- as.factor(df$smoker)
      df$region <- as.factor(df$region)
      df$children <- as.factor(as.integer(df$children))
      return(df)
    })
    
    #medical_insurance$children <- as.numeric(medical_insurance$children)
    
    # Generate plots
    plot_result <- compare(syn_data, medical_insurance, nrow = 4, ncol = 2)
    plot_output <- plot_result$plots
    
    # Save plot
    ggsave(
      filename = paste0("Medical Insurance Cost Data/evaluation_utility/fit_for_purpose_utility/plot_", method, "_", m, ".png"), 
      plot = plot_output, 
      height = 15, 
      width = 10
    )
  }
}



# Define function to generate and save plots for different m values
generate_comparison_plot <- function(variable, datasets_m5_wo_original, datasets_m10_wo_original, datasets_m50_wo_original, medical_insurance, output_path, m_values) {
  
  for (m in m_values) {
    
    # Select the appropriate dataset group based on m
    if (m == 5) {
      synthpop_group <- datasets_m5_wo_original[grep("synthpop", names(datasets_m5_wo_original))]
      arf_group <- datasets_m5_wo_original[grep("arf", names(datasets_m5_wo_original))]
      privbayes_group <- datasets_m5_wo_original[grep("privbayes", names(datasets_m5_wo_original))]
      ctgan_group <- datasets_m5_wo_original[grep("ctgan", names(datasets_m5_wo_original))]
      tvae_group <- datasets_m5_wo_original[grep("tvae", names(datasets_m5_wo_original))]
      tabsyn_group <- datasets_m5_wo_original[grep("tabsyn", names(datasets_m5_wo_original))]
    } else if (m == 10) {
      synthpop_group <- datasets_m10_wo_original[grep("synthpop", names(datasets_m10_wo_original))]
      arf_group <- datasets_m10_wo_original[grep("arf", names(datasets_m10_wo_original))]
      privbayes_group <- datasets_m10_wo_original[grep("privbayes", names(datasets_m10_wo_original))]
      ctgan_group <- datasets_m10_wo_original[grep("ctgan", names(datasets_m10_wo_original))]
      tvae_group <- datasets_m10_wo_original[grep("tvae", names(datasets_m10_wo_original))]
      tabsyn_group <- datasets_m10_wo_original[grep("tabsyn", names(datasets_m10_wo_original))]
    } else if (m == 50) {
      synthpop_group <- datasets_m50_wo_original[grep("synthpop", names(datasets_m50_wo_original))]
      arf_group <- datasets_m50_wo_original[grep("arf", names(datasets_m50_wo_original))]
      privbayes_group <- datasets_m50_wo_original[grep("privbayes", names(datasets_m50_wo_original))]
      ctgan_group <- datasets_m50_wo_original[grep("ctgan", names(datasets_m50_wo_original))]
      tvae_group <- datasets_m50_wo_original[grep("tvae", names(datasets_m50_wo_original))]
      tabsyn_group <- datasets_m50_wo_original[grep("tabsyn", names(datasets_m50_wo_original))]
    }
    
    
    # Generate comparison plots
    plot_synthpop <- compare(synthpop_group, medical_insurance, vars = variable)$plots
    plot_arf <- compare(arf_group, medical_insurance, vars = variable)$plots
    plot_privbayes <- compare(privbayes_group, medical_insurance, vars = variable)$plots
    plot_ctgan <- compare(ctgan_group, medical_insurance, vars = variable)$plots
    plot_tvae <- compare(tvae_group, medical_insurance, vars = variable)$plots
    plot_tabsyn <- compare(tabsyn_group, medical_insurance, vars = variable)$plots
    
    # Create individual plots with titles
    p1 <- plot(plot_synthpop) + labs(title = paste("Synthpop"))
    p2 <- plot(plot_arf) + labs(title = paste("ARF"))
    p3 <- plot(plot_privbayes) + labs(title = paste("PrivBayes"))
    p4 <- plot(plot_ctgan) + labs(title = paste("CTGAN"))
    p5 <- plot(plot_tvae) + labs(title = paste("TVAE"))
    p6 <- plot(plot_tabsyn) + labs(title = paste("TABSYN"))
    
    # Arrange plots in a 2x2 layout
    combined_plot <- wrap_plots(p1, p2, p3, p4, p5, p6, ncol = 2)
    
    
    # Add common labels and theme
    final_plot <- combined_plot + 
      plot_layout(guides = "collect") & 
      labs(x = "Value", y = "Percent") & 
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      )
    
    # Display plot
    print(final_plot)
    
    # Save the plot
    ggsave(paste0(output_path, "final_plot_", variable, "_", m, ".png"), plot = final_plot, height = 8, width = 12)
  }
}


## Outcome-specific utility ####



#...............................................................................

# Function to fit the glm model and return coefficients
fit_model_medical_insurance <- function(data, dataset_name) {
  
  data$charges_1000 <- data$charges / 1000
  
  model <- lm(charges_1000 ~ age + sex + bmi + children + smoker + region, 
              data = data)
  
  # Extract coefficients and standard errors
  coeffs <- summary(model)$coefficients
  coeff_df <- as.data.frame(coeffs)
  colnames(coeff_df) <- c("Estimate", "Std.Error", "z.value", "Pr(>|z|)")  # Name the columns properly
  coeff_df$Term <- rownames(coeff_df)  # Add the coefficient names
  coeff_df$Dataset <- dataset_name     # Add the dataset name for identification
  rownames(coeff_df) <- NULL           # Remove row names
  
  # Compute confidence intervals (using ± 1.96 * Std. Error)
  coeff_df$ci_lo <- coeff_df$Estimate - 1.96 * coeff_df$Std.Error
  coeff_df$ci_hi <- coeff_df$Estimate + 1.96 * coeff_df$Std.Error
  
  return(coeff_df)
}




# Function to plot regression results for outcome-specific utility
plot_outcomes_medical_insurance <- function(df) {
  
  pd <- position_dodge(width = .5)
  
  ggplot(df[df$Term != "(Intercept)", ], aes(x = Term, y = Estimate, col = Dataset)) + 
    geom_point(position = pd) + 
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = pd, width = .5) + 
    theme_bw() +
    labs(title = "Statistical Utility (Linear Regression)",
         x = "Variable",
         y = "Beta Coefficient",
         color = "Method") +
    scale_color_discrete(drop = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
}



# Funktion zur Berechnung des CIO Measures
calculate_cio <- function(term, data_original, data_syn) {
  # Extrahiere die Zeilen basierend auf dem Term
  row_original <- data_original[data_original$Term == term, ]
  row_syn <- data_syn[data_syn$Term == term, ]
  
  # Berechnung
  L_i <- pmax(row_original$ci_lo_combined, row_syn$ci_lo_combined)
  U_i <- pmin(row_original$ci_hi_combined, row_syn$ci_hi_combined)
  
  cio_measure <- ifelse(
    L_i <= U_i,
    (U_i - L_i) / (2 * (row_original$ci_hi_combined - row_original$ci_lo_combined)) +
      (U_i - L_i) / (2 * (row_syn$ci_hi_combined - row_syn$ci_lo_combined)),
    0
  )
  
  return(cio_measure)
}




# Funktion zur Berechnung des z_j für MASD für einen Term
calculate_z_j_for_masd <- function(term, data_original, data_syn, m) {
  
  # Extrahiere die Zeilen basierend auf dem Term
  row_original <- data_original[data_original$Term == term, ]
  row_syn <- data_syn[data_syn$Term == term, ]
  
  # Berechnung des absoluten Unterschieds der Koeffizienten
  z_j <- (row_syn$combined_estimate - row_original$combined_estimate) / (sqrt(row_original$Variance / m))
  
  return(z_j)
}


evaluate_model <- function(datasets, m, output_path) {
  # Loop over datasets, fit the model, and collect coefficients
  coeff_results <- bind_rows(
    lapply(names(datasets), function(name) {
      fit_model_medical_insurance(datasets[[name]], name)
    })
  )
  
  # Clean up the results for plotting
  coeff_results <- coeff_results %>%
    select(Term, Estimate, Dataset, ci_lo, ci_hi, Std.Error) %>%
    filter(Term != "(Intercept)")  # Exclude intercept for cleaner plot
  
  # Create a new variable "Method" based on the dataset name
  coeff_results <- coeff_results %>%
    mutate(Method = case_when(
      grepl("syn_medical_insurance_privbayes", Dataset) ~ "PrivBayes",
      grepl("syn_medical_insurance_arf", Dataset) ~ "ARF",
      grepl("syn_medical_insurance_synthpop", Dataset) ~ "Synthpop",
      grepl("syn_medical_insurance_ctgan", Dataset) ~ "CTGAN",
      grepl("syn_medical_insurance_tvae", Dataset) ~ "TVAE",
      grepl("syn_medical_insurance_tabsyn", Dataset) ~ "TABSYN",
      Dataset == "medical_insurance" ~ "Real",
      TRUE ~ "Unknown"
    ))
  
  # Combine estimates for each term and method
  combined_results <- coeff_results %>%
    group_by(Term, Method) %>%
    reframe(
      combined_estimate = mean(Estimate), 
      std_error = mean(Std.Error),
      Variance = mean(Std.Error^2),
      B = var(Estimate),                                       
      U = mean(Std.Error^2),                                     
      Var_combined = U + B / m,                                     
      ci_lo_combined = ifelse(Method == "Real", first(ci_lo), combined_estimate - 1.96 * sqrt(Var_combined)),
      ci_hi_combined = ifelse(Method == "Real", first(ci_hi), combined_estimate + 1.96 * sqrt(Var_combined))
    ) %>%
    distinct()
  
  print(combined_results)
  
  # Separate original and synthetic data
  data_original <- combined_results %>% filter(Method == "Real")
  data_syn <- combined_results %>% filter(Method != "Real")
  
  # Compute CIO measures
  results <- bind_rows(lapply(unique(data_syn$Method), function(method) {
    data_syn_method <- data_syn %>% filter(Method == method)
    
    data.frame(
      Term = data_original$Term,
      Method = method,
      cio_measure = sapply(data_original$Term, calculate_cio, data_original = data_original, data_syn = data_syn_method),
      z_j_measures = sapply(data_original$Term, calculate_z_j_for_masd, data_original = data_original, data_syn = data_syn_method, m = m)
    )
  }))
  
  mean_cio_and_masd_per_method <- results %>%
    group_by(Method) %>%
    summarise(mean_cio = round(mean(cio_measure, na.rm = TRUE), digits = 3),
              masd = round(abs(mean(z_j_measures, na.rm = TRUE)), digits = 3))
  
  print(results)
  print(mean_cio_and_masd_per_method)
  
  # Generate labels for methods
  method_labels <- mean_cio_and_masd_per_method %>%
    mutate(label = paste0(Method, " (CIO: ", round(mean_cio, 2), ", MASD: ", round(masd, 2), ")")) %>%
    pull(label)
  
  names(method_labels) <- mean_cio_and_masd_per_method$Method
  
  # Plot results
  pd <- position_dodge(width = 0.5)
  
  regression_coeff_plot_combined <- ggplot(combined_results, aes(y = forcats::fct_rev(Term), x = combined_estimate, col = Method)) + 
    geom_point(position = pd, size = 1.5) + 
    geom_errorbarh(aes(xmin = ci_lo_combined, xmax = ci_hi_combined), position = pd, height = 0.5, linewidth = 0.9) + 
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 1) +  
    theme_bw() +
    labs(title = "",
         x = "Regression Coefficient",
         y = "Variable",
         color = "Method") +
    #scale_color_discrete(drop = FALSE, labels = method_labels) +
    scale_color_manual(
      values = c("#66C2A5", "#FC8D62", "#8DA0CB", "black", "#E78AC3", "#A6D854", "#FFD92F"),
      drop = FALSE#,
      #labels = method_labels
    ) +
    scale_y_discrete(
      labels = c(
        "age" = "Age in years",
        "bmi" = "Body Mass Index",
        "children" = "Number of children",
        "regionnorthwest" = "Region = northwest",
        "regionsoutheast" = "Region = southeast",
        "regionsouthwest" = "Region = southwest",
        "sexmale" = "Gender = male",
        "smokeryes" = "Smoker"
      )) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) 
  
  
  print(regression_coeff_plot_combined)
  
  ggsave(output_path, plot = regression_coeff_plot_combined, width = 8, height = 10)
}
















## Global utility ####

# Function to compute mean S_pMSE for a given method and sample size
compute_s_pMSE <- function(method, m) {
  # Load files dynamically
  path <- "Medical Insurance Cost Data/Data"
  pattern <- paste0("syn_medical_insurance_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$")
  dateien <- list.files(path = file.path(path, method), pattern = pattern, full.names = TRUE)
  
  print(dateien)
  
  # Read data
  syn_data <- lapply(dateien, read.csv)
  
  # Convert categorical variables
  syn_data <- lapply(syn_data, function(df) {
    df$sex <- as.factor(df$sex)
    df$children <- as.factor(as.integer(df$children))
    df$smoker <- as.factor(df$smoker)
    df$region <- as.factor(df$region)
    return(df)
  })
  
  # Compute mean S_pMSE
  compare_results <- lapply(syn_data, compare, medical_insurance)
  s_pMSE_values <- sapply(compare_results, function(res) mean(as.data.frame(res$tab.utility)$S_pMSE))
  
  return(round(mean(s_pMSE_values), 3))
}




# 1. Membership disclosure ####

# Matching Attack Dataset to Synthetic Data
hamming_distance <- function(a, b) sum(a != b)



compute_f1 <- function(synthetic_data) {
  match_attack <- sapply(1:nrow(attack_data), function(i) {
    min_dist <- min(apply(synthetic_data, 1, function(x) hamming_distance(attack_data[i, ], x)))
    return(min_dist)
  })
  
  threshold <- quantile(match_attack, 0.25)
  predicted_membership <- match_attack <= threshold
  actual_membership <- 1:nrow(attack_data) %in% 1:nrow(attack_train)  # True labels
  
  precision <- sum(predicted_membership & actual_membership) / sum(predicted_membership)
  recall <- sum(predicted_membership & actual_membership) / sum(actual_membership)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(f1_score)
}




evaluate_method <- function(method, m, n, N) {
  
  path <- paste0("Medical Insurance Cost Data/Data/train_data/", method, "/")
  pattern <- paste0("^syn_real_train_", method, "_.*\\.csv$")
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  selected_files <- files[grepl(paste0("syn_real_train_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$"), files)]
  data_list <- lapply(selected_files, read.csv)
  
  f1_scores <- sapply(data_list, compute_f1)  # Compute individual F1 scores
  mean_f1 <- mean(f1_scores)  # Compute mean F1 score
  
  f1_max <- 2 * n / (N/(1+n/N))
  
  M <- (mean_f1-f1_max) / (1-f1_max)
  
  # Create a combined data frame with both mean and individual F1 scores
  result_df <- data.frame(Method = method,
                          m = m,
                          File = selected_files,
                          F1_Score = f1_scores,
                          Mean_F1_Score = mean_f1,
                          M = M)
  
  return(result_df)
  
  
}







# 2. Attribute disclosure risk ####


# Function to compute WEAP
compute_WEAP <- function(synthetic_data, key_vars, target_var) {
  synthetic_data %>%
    group_by(across(all_of(key_vars))) %>%
    mutate(WEAP = sum(!!sym(target_var) == first(!!sym(target_var))) / n()) %>%
    ungroup()
}

# Function to filter records with WEAP = 1
filter_high_WEAP <- function(synthetic_data) {
  synthetic_data %>% filter(WEAP == 1)
}

# Function to compute TCAP for a single synthetic dataset
compute_TCAP <- function(original_data, synthetic_data, key_vars, target_var) {
  matched_data <- inner_join(synthetic_data, original_data, by = key_vars, suffix = c("_synth", "_orig"))
  
  matched_data <- matched_data %>%
    mutate(TCAP = ifelse(!!sym(paste0(target_var, "_synth")) == !!sym(paste0(target_var, "_orig")), 1, 0))
  
  mean(matched_data$TCAP, na.rm = TRUE)  # Final TCAP score for the dataset
}

compute_TCAP_multiple <- function(original_data, synthetic_datasets, key_vars, target_var) {
  tcap_scores <- map_dbl(synthetic_datasets, function(synth_data) {
    synth_data <- compute_WEAP(synth_data, key_vars, target_var)
    
    # Debugging: Print unique WEAP values
    print("Unique WEAP values before filtering:")
    print(unique(synth_data$WEAP))
    
    synth_data <- filter_high_WEAP(synth_data)
    
    # Debugging: Check number of records remaining
    print(paste("Records remaining after filtering:", nrow(synth_data)))
    
    if (nrow(synth_data) == 0) {
      return(NA)  # Avoid NaN issues
    }
    
    tcap_value <- compute_TCAP(original_data, synth_data, key_vars, target_var)
    
    # Debugging: Print TCAP for each dataset
    print(paste("TCAP score for this dataset:", tcap_value))
    
    return(tcap_value)
  })
  
  mean_tcap <- mean(tcap_scores, na.rm = TRUE)  # Avoid NaN if all values are NA
  
  # Debugging: Print final TCAP values
  print("Final TCAP scores per dataset:")
  print(tcap_scores)
  
  return(mean_tcap)
}







# Function to compute TCAP scores for a given method and m values
compute_tcap_for_method <- function(method, m_values) {
  path <- paste0("Medical Insurance Cost Data/Data/", tolower(method), "/")
  pattern <- paste0("^syn_medical_insurance_", tolower(method), "_.*\\.csv$")
  
  # List all files
  all_files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Compute TCAP scores for each m value
  tcap_scores <- sapply(m_values, function(m) {
    # Build regex dynamically to match correct files for given m
    regex_m <- if (m == 10) {
      "syn_medical_insurance_.*_(1|2|3|4|5|6|7|8|9|10)\\.csv$"
    } else if (m == 50) {
      "syn_medical_insurance_.*_([1-9]|[1-4][0-9]|50)\\.csv$"
    } else { # Default case for m = 5
      "syn_medical_insurance_.*_[1-5]\\.csv$"
    }
    
    # Select matching files
    selected_files <- all_files[grepl(regex_m, all_files)]
    syn_data_list <- lapply(selected_files, read.csv)
    
    # Compute TCAP score
    compute_TCAP_multiple(real_data, syn_data_list, key_vars = c("age", "sex", "region"), target_var = "charges")
  })
  
  return(tcap_scores)
}








# More evaluation criteria ####

## Helpers ####

preprocess_data <- function(data) {
  data <- as.data.frame(data)  # Ensure it's a data frame
  
  # Convert factors and characters to numeric
  dummy_model <- dummyVars("~ .", data = data, fullRank = TRUE)  
  numeric_data <- predict(dummy_model, newdata = data)
  
  return(as.matrix(numeric_data))  # Convert to numeric matrix
}


# Funktion zur Umwandlung von character-Spalten in Faktoren
convert_to_factors <- function(data) {
  # Wandle alle character-Spalten in Faktoren um
  data[] <- lapply(data, function(x) if (is.character(x)) as.factor(x) else x)
  return(data)
}




## Resemblance ####


embed_data_ocsvm <- function(data, nu_value = 0.05, kernel_type = "radial") {
  
  data_numeric <- as.data.frame(preprocess_data(data))
  
  # Train One-Class SVM (optional for future extension, not used directly)
  oc_svm_model <- svm(data_numeric, 
                      type = "one-classification", 
                      kernel = kernel_type, 
                      nu = nu_value, 
                      scale = FALSE)
  
  emb_center <- colMeans(data_numeric)
  
  return(list(emb_data = data_numeric, 
              emb_center = emb_center, 
              model = oc_svm_model))
}




compute_alpha_precision_multi <- function(real_data, synthetic_list, alpha = 0.9) {
  
  real_emb <- embed_data_ocsvm(real_data)
  real_data_emb <- real_emb$emb_data
  emb_center <- real_emb$emb_center
  
  dist_to_center_real <- sqrt(rowSums((real_data_emb - matrix(emb_center, nrow = nrow(real_data_emb), ncol = length(emb_center), byrow = TRUE))^2))
  radius <- quantile(dist_to_center_real, probs = alpha)
  
  results <- sapply(synthetic_list, function(synthetic_data) {
    synthetic_emb <- embed_data_ocsvm(synthetic_data)
    synthetic_data_emb <- synthetic_emb$emb_data
    
    dist_to_center_synthetic <- sqrt(rowSums((synthetic_data_emb - matrix(emb_center, nrow = nrow(synthetic_data_emb), ncol = length(emb_center), byrow = TRUE))^2))
    inside <- dist_to_center_synthetic <= radius
    
    mean(inside)
  })
  
  return(results)
}




compute_beta_recall_multi <- function(real_data, synthetic_list, beta = 0.9) {
  
  real_emb <- embed_data_ocsvm(real_data)$emb_data
  
  results <- sapply(synthetic_list, function(synthetic_data) {
    synthetic_emb <- embed_data_ocsvm(synthetic_data)
    synthetic_data_emb <- synthetic_emb$emb_data
    emb_center <- colMeans(synthetic_data_emb)
    
    dist_to_center_synthetic <- sqrt(rowSums((synthetic_data_emb - matrix(emb_center, nrow = nrow(synthetic_data_emb), ncol = length(emb_center), byrow = TRUE))^2))
    radius <- quantile(dist_to_center_synthetic, probs = beta)
    
    dist_to_center_real <- sqrt(rowSums((real_emb - matrix(emb_center, nrow = nrow(real_emb), ncol = length(emb_center), byrow = TRUE))^2))
    inside <- dist_to_center_real <= radius
    
    mean(inside)
  })
  
  return(results)
}




compute_authenticity_multi <- function(real_data, synthetic_list) {
  
  real_emb <- embed_data_ocsvm(real_data)$emb_data
  
  results <- sapply(synthetic_list, function(synthetic_data) {
    
    synthetic_emb <- embed_data_ocsvm(synthetic_data)$emb_data
    
    nn_synthetic <- get.knnx(real_emb, synthetic_emb, k = 1)
    min_distances_synthetic <- nn_synthetic$nn.dist[, 1]
    
    real_nn <- get.knnx(real_emb, real_emb, k = 2)
    real_nn_distances <- real_nn$nn.dist[, 2]
    
    authenticity_scores <- sapply(min_distances_synthetic, function(dist) {
      dist > min(real_nn_distances)
    })
    
    mean(authenticity_scores)
  })
  
  return(results)
}







## C2ST ####

# Function to evaluate C2ST
evaluate_c2st_per_dataset_cv <- function(method, m_value, real_test, syn_data_list, classifier_type = "KNN") {
  
  c2st_results <- list()  # Store C2ST results per dataset
  
  for (syn_data in syn_data_list) {
    
    # Label synthetic data as 1
    syn_data$label <- 1 
    
    # Label real data as 0
    real_test$label <- 0  
    
    # Combine real test data with the current synthetic dataset
    combined_data <- rbind(real_test, syn_data)
    
    # Shuffle dataset
    set.seed(123)
    combined_data <- combined_data[sample(nrow(combined_data)), ]
    
    # Convert label into a factor with valid R variable names
    combined_data$label <- factor(combined_data$label, levels = c(0, 1))
    levels(combined_data$label) <- make.names(levels(combined_data$label))  # Make valid R variable names
    
    # Identify numeric columns for scaling
    feature_cols <- setdiff(names(combined_data), "label")
    numeric_cols <- feature_cols[sapply(combined_data[, feature_cols], is.numeric)]
    
    # Scale only numeric features
    combined_data[, numeric_cols] <- scale(combined_data[, numeric_cols])
    
    # Split into train and test set (80-20 split)
    set.seed(123)
    train_index <- createDataPartition(combined_data$label, p = 0.8, list = FALSE)
    train_data <- combined_data[train_index, ]
    test_data <- combined_data[-train_index, ]
    
    # Set up cross-validation
    train_control <- trainControl(method = "cv", number = 5, 
                                  classProbs = TRUE, summaryFunction = twoClassSummary)
    
    # Train classifier based on user selection
    if (classifier_type == "KNN") {
      model <- train(label ~ ., data = train_data, method = "knn", 
                     trControl = train_control, tuneLength = 5, metric = "ROC")
      
    } else if (classifier_type == "NN") {
      model <- train(label ~ ., data = train_data, method = "mlp", 
                     trControl = train_control, tuneGrid = expand.grid(size = 20),  # One hidden layer with 20 neurons
                     metric = "ROC")
      
    } else if (classifier_type == "RF") {
      model <- train(label ~ ., data = train_data, method = "rf", 
                     trControl = train_control, tuneGrid = expand.grid(mtry = sqrt(ncol(train_data) - 1)), 
                     metric = "ROC")
      
    } else {
      stop("Invalid classifier type. Choose 'KNN', 'NN', or 'RF'.")
    }
    
    # Predictions from the model
    predictions <- predict(model, test_data)
    probs <- predict(model, test_data, type = "prob")
    
    # Compute accuracy and AUC
    cm <- confusionMatrix(predictions, test_data$label)
    auc <- roc(test_data$label, probs[, 2])$auc
    
    # Compute p-value using binomial test
    accuracy <- cm$overall["Accuracy"]
    n_test <- length(test_data$label)
    p_value <- 2 * pbinom(q = round(accuracy * n_test), size = n_test, prob = 0.5, lower.tail = FALSE)
    
    # Store results in list as a data frame
    new_row <- data.frame(Method = method, 
                          M = m_value, 
                          C2ST_Accuracy = accuracy,
                          AUC = as.numeric(auc),  # Ensure AUC is numeric
                          P_Value = p_value,
                          Classifier = classifier_type)
    
    c2st_results <- append(c2st_results, list(new_row))
  }
  
  # Return all results after processing all synthetic datasets
  return(do.call(rbind, c2st_results))
}




## ML utility ####

# Define evaluation function for regression metrics
evaluate_regression_per_dataset <- function(method, m, real_test, syn_data_list) {
  results <- data.frame(Method = character(), M = integer(), R2 = numeric(), RMSE = numeric(), MAE = numeric())
  
  for (syn_data in syn_data_list) {
    
    # Convert categorical variables to factors
    syn_data[] <- lapply(syn_data, function(x) if(is.character(x)) as.factor(x) else x)
    real_test[] <- lapply(real_test, function(x) if(is.character(x)) as.factor(x) else x)
    
    # Train Random Forest model on synthetic data
    model_synth <- randomForest(as.formula(paste(target_var, "~ .")), data = syn_data)
    
    # Predict on real test data
    pred_synth <- predict(model_synth, real_test)
    
    # Compute regression metrics
    metrics_synth <- postResample(pred_synth, real_test[[target_var]])
    metrics_synth
    
    
    # Store results
    results <- bind_rows(results, data.frame(Method = method, M = m, 
                                             R2 = metrics_synth[2], 
                                             RMSE = metrics_synth[1], 
                                             MAE = metrics_synth[3]))
  }
  
  return(results)
}



## DCR ####

calculate_dcr <- function(synthetic_data, real_train, real_holdout) {
  
  # Compute Gower distance matrices for each pair (synthetic vs. real)
  gower_synth_train <- as.matrix(daisy(rbind(synthetic_data, real_train), metric = "gower"))[
    1:nrow(synthetic_data), (nrow(synthetic_data) + 1):(nrow(synthetic_data) + nrow(real_train))
  ]
  
  gower_holdout_train <- as.matrix(daisy(rbind(real_holdout, real_train), metric = "gower"))[
    1:nrow(real_holdout), (nrow(real_holdout) + 1):(nrow(real_holdout) + nrow(real_train))
  ]
  
  # Find closest real record for each synthetic record
  dcr_synth_train <- apply(gower_synth_train, 1, min, na.rm = TRUE)
  dcr_holdout_train <- apply(gower_holdout_train, 1, min, na.rm = TRUE)
  
  
  return(list(dcr_synth_train = mean(dcr_synth_train, na.rm = TRUE), 
              dcr_holdout_train = mean(dcr_holdout_train, na.rm = TRUE)))
}






## Nearest neighbor distance ratio ####

# Function to compute NNDR with Gower distance
compute_nndr_tabular <- function(synthetic_data, real_train, real_holdhout) {
  
  ## Synthetic data ##
  
  # Compute Gower distances between synthetic and real data
  gower_distances <- daisy(rbind(synthetic_data, real_train), metric = "gower")
  gower_matrix <- as.matrix(gower_distances)
  
  # Extract the relevant portion (synthetic → real)
  num_synth <- nrow(synthetic_data)
  num_train <- nrow(real_train)
  gower_matrix <- gower_matrix[1:num_synth, (num_synth + 1):(num_synth + num_train)]
  
  # Find nearest and second-nearest neighbors
  sorted_dists <- t(apply(gower_matrix, 1, sort))
  d1 <- sorted_dists[, 1]  # Closest distance
  d2 <- sorted_dists[, 2]  # Second closest distance
  
  # Compute NNDR
  nndr_synth <- d1 / d2
  
  
  
  ## Holdout data ##
  
  # Compute Gower distances between holdout and real data
  gower_distances <- daisy(rbind(real_holdhout, real_train), metric = "gower")
  gower_matrix <- as.matrix(gower_distances)
  
  # Extract the relevant portion (synthetic → real)
  num_holdout <- nrow(real_holdhout)
  num_train <- nrow(real_train)
  gower_matrix <- gower_matrix[1:num_holdout, (num_holdout + 1):(num_holdout + num_train)]
  
  # Find nearest and second-nearest neighbors
  sorted_dists <- t(apply(gower_matrix, 1, sort))
  d1 <- sorted_dists[, 1]  # Closest distance
  d2 <- sorted_dists[, 2]  # Second closest distance
  
  # Compute NNDR
  nndr_holdout <- d1 / d2
  
  return(list(nndr_synth = nndr_synth, nndr_holdout = nndr_holdout))
}




## Correlations ####

# Preprocessing function
process_dataset <- function(df) {
  df %>%
    mutate(
      sex = ifelse(sex == "male", 1, 0),
      smoker = ifelse(smoker == "yes", 1, 0),
      children = as.integer(children)
    ) %>%
    dummy_cols(select_columns = c("region"),
               remove_first_dummy = FALSE,
               remove_selected_columns = TRUE)
}
