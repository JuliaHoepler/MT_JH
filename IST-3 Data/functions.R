
# Note:
# The function deals currently with the following cases:
# For numeric variables:
# 1. For numerics:
# - Binary (exactly 2 unique values): Transformation to factor
# - Categorical (more than 2 unique values and until set threshold): Transformation to factor
# - Continuous (more than the threshold value): stays numeric
# 
# 2. For characters:
# - Binary (exactly 2 unique values): Transformation to factor
# - Categorical (more than 2 unique values and until set threshold): Transformation to factor


# Because it is necessary to distinguish between numerical variables and 
# categorical variables, one need to define a threshold value. Unique values
# above this threshold are assumed to be numeric/integer.

# Challenge: Choose appropriate threshold value.

# There are variables (e.g. country) that are originally characters, so non-ordered
# factor variables.
# On the other hand there are variables that are scores, so ordered factor variables. 
# But they can have many different score values so leaving them as numeric/integer
# would be the best option.
# So why not leave them as they are?
# -> I do not know how the class should look like in order for the data to be 
#    useful for the analyses without further preprocessing.
# -> There are also variables like "aspirin_pre", that are originally numeric 
# and have the unique values c("1", "2", "20", "40"), which are in fact non-ordered 
# categories, which stand for c("Yes", "No", "Question not answered", "Question not asked"). 


transform_variables <- function(df, unique_threshold = 15) {
  
  # Replace empty strings with NA for character columns
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col[col == ""] <- NA
    }
    return(col)
  })
  
  for (col_name in names(df)) {
    col <- df[[col_name]]
    unique_vals <- unique(col)
    num_unique <- length(unique_vals)
    
    if (is.numeric(col)) {
      if (num_unique == 2) {
        # Numeric binary variable, transform to factor
        df[[col_name]] <- as.factor(col)
      } else if (num_unique > 2 && num_unique <= unique_threshold) {
        # Numeric categorical variable, transform to factor
        df[[col_name]] <- as.factor(col)
      }
      # Else: Numeric continuous, keep as is (do nothing)
    } else if (is.character(col)) {
      # Exclude NA values from the unique values count
      num_non_na_unique <- length(unique(na.omit(col)))
      
      if (num_non_na_unique > 2 && num_unique <= unique_threshold) {
        # Character variable with more than 2 categories, transform to factor
        df[[col_name]] <- as.factor(col)
      } else if (num_non_na_unique > 2 && num_unique > unique_threshold) {
        df[[col_name]] <- as.integer(as.factor(col))
      } else if (num_non_na_unique == 2) {
        # Character binary variable, transform to factor
        df[[col_name]] <- as.factor(col)
      }
      # Else: We assume there won't be a character variable with only 1 category
    }
  }
  
  return(df)
}





transform_variables_2 <- function(df, unique_threshold = 15, excluded_vars) {
  
  # Replace empty strings with NA for character columns
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col[col == ""] <- NA
    }
    return(col)
  })
  
  for (col_name in names(df)) {
    col <- df[[col_name]]
    unique_vals <- unique(col)
    num_unique <- length(unique_vals)
    
    if (col_name %in% excluded_vars) {
      next  # Skip manual variables
    } else if (is.numeric(col)) {
      if (num_unique == 2) {
        # Numeric binary variable, transform to factor
        df[[col_name]] <- as.factor(col)
      } else if (num_unique > 2 && num_unique <= unique_threshold) {
        # Numeric categorical variable, transform to factor
        df[[col_name]] <- as.factor(col)
      }
      # Else: Numeric continuous, keep as is (do nothing)
    } else if (is.character(col)) {
      # Exclude NA values from the unique values count
      num_non_na_unique <- length(unique(na.omit(col)))
      
      if (num_non_na_unique > 2 && num_unique <= unique_threshold) {
        # Character variable with more than 2 categories, transform to factor
        df[[col_name]] <- as.factor(col)
      } else if (num_non_na_unique > 2 && num_unique > unique_threshold) {
        df[[col_name]] <- as.integer(as.factor(col))
      } else if (num_non_na_unique == 2) {
        # Character binary variable, transform to factor
        df[[col_name]] <- as.factor(col)
      }
      # Else: We assume there won't be a character variable with only 1 category
    }
  }
  
  return(df)
}



# The following function is supposed to give you an overview over the data before
# and after the transformation (i.e. class, unique values/categories etc.)

variable_summary <- function(df, transformed_df) {
  summary_list <- lapply(names(df), function(col_name) {
    original_col <- df[[col_name]]
    transformed_col <- transformed_df[[col_name]]
    
    freq_table_output <- capture.output(print(table(original_col)))
    freq_table <- paste(freq_table_output[-1], collapse = " ")
    length_freq_table <- length(table(original_col))
    categories <- freq_table_output[2]
    
    freq_table_output <- capture.output(print(table(transformed_col)))
    freq_table_transformed <- paste(freq_table_output[-1], collapse = " ")
    length_freq_table_transformed <- length(table(transformed_col))
    categories_transformed <- freq_table_output[2]
    
    data.frame(
      Variable = col_name,
      Original_Class = class(original_col),
      Transformed_Class = class(transformed_col),
      Original_Categories = categories,
      Transformed_Categories = categories_transformed,
      Original_Number_Categories = length_freq_table,
      Transformed_Number_Categories = length_freq_table_transformed,
      Original_Frequency_Table = freq_table,
      Tranformed_Frequency_Table = freq_table_transformed
    )
  })
  
  summary_df <- do.call(rbind, summary_list)
  return(summary_df)
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
    #   "arf" = "arf"
    # )
    # base_path <- method[["arf"]]
    
    # Generate regex pattern to match files
    pattern <- paste0("syn_data_small_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$")
    
    # List all relevant files
    dateien <- list.files(
      path = paste0("IST-3 Data/Data/", base_path, "/"), 
      pattern = pattern, 
      full.names = TRUE
    )
    
    # Read CSV files
    syn_data <- lapply(dateien, read.csv)
    
    # Convert categorical variables
    syn_data <- lapply(syn_data, function(df) {
      df$outcome <- as.factor(df$outcome)
      df$itt_treat <- as.factor(df$itt_treat)
      df$vis_infarct <- as.factor(df$vis_infarct)
      return(df)
    })
    
    
    data_small <- data_small %>%
      mutate(outcome = as.factor(outcome),
             itt_treat = as.factor(itt_treat),
             vis_infarct = as.factor(vis_infarct))
    
    # Generate plots
    plot_result <- compare(syn_data, data_small, nrow = 4, ncol = 2)
    plot_output <- plot_result$plots
    
    # Save plot
    ggsave(
      filename = paste0("IST-3 Data/evaluation_utility/fit_for_purpose_utility/plot_", method, "_", m, ".png"), 
      plot = plot_output, 
      height = 15, 
      width = 10
    )
  }
}



# Define function to generate and save plots for different m values
generate_comparison_plot <- function(variable, datasets_m5_wo_original, datasets_m10_wo_original, datasets_m50_wo_original, medical_insurance, output_path, m_values) {
  
  for (m in m_values) {
    
    data_small$outcome <- as.factor(data_small$outcome)
    #variable <- "outcome"
    
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
    plot_synthpop <- compare(synthpop_group, data_small, vars = variable)$plots
    plot_arf <- compare(arf_group, data_small, vars = variable)$plots
    plot_privbayes <- compare(privbayes_group, data_small, vars = variable)$plots
    plot_ctgan <- compare(ctgan_group, data_small, vars = variable)$plots
    plot_tvae <- compare(tvae_group, data_small, vars = variable)$plots
    plot_tabsyn <- compare(tabsyn_group, data_small, vars = variable)$plots
    
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

# Function to fit the glm model and return coefficients
fit_model <- function(data, dataset_name) {
  
  data$itt_treat <- relevel(data$itt_treat, ref = "Placebo")
  data$vis_infarct <- relevel(data$vis_infarct, ref = "No")
  
  model <- glm(outcome ~ itt_treat + age + nihss + randdelay + vis_infarct, 
               family = "binomial", data = data)
  
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


# Function to fit the glm model and return OR
fit_model_OR <- function(data, dataset_name) {
  
  data$itt_treat <- relevel(data$itt_treat, ref = "Placebo")
  data$vis_infarct <- relevel(data$vis_infarct, ref = "No")
  
  model <- glm(outcome ~ itt_treat + age + nihss + randdelay + vis_infarct, 
               family = "binomial", data = data)
  
  # Extract coefficients and standard errors
  coeffs <- summary(model)$coefficients
  coeff_df <- as.data.frame(coeffs)
  colnames(coeff_df) <- c("Estimate", "Std.Error", "z.value", "Pr(>|z|)")  # Name the columns properly
  coeff_df$Term <- rownames(coeff_df)  # Add the coefficient names
  coeff_df$Dataset <- dataset_name     # Add the dataset name for identification
  rownames(coeff_df) <- NULL           # Remove row names
  
  coeff_df$Estimate <- exp(coeff_df$Estimate) # look at the OR not the regression coefficient
  
  # Compute confidence intervals (using ± 1.96 * Std. Error)
  coeff_df$ci_lo <- coeff_df$Estimate - 1.96 * coeff_df$Std.Error
  coeff_df$ci_hi <- coeff_df$Estimate + 1.96 * coeff_df$Std.Error
  
  return(coeff_df)
}




# Function to plot regression results for outcome-specific utility
plot_outcomes <- function(df) {
  
  pd <- position_dodge(width = .5)
  
  ggplot(df[df$Term != "(Intercept)", ], aes(x = Term, y = Estimate, col = Dataset)) + 
    geom_point(position = pd) + 
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = pd, width = .5) + 
    theme_bw() +
    labs(title = "Statistical Utility (Logistic Regression)",
         x = "Variable",
         y = "Beta Coefficient",
         color = "Method") +
    scale_color_discrete(drop = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
}

# Relevel factor variables function
relevel_factors <- function(data_list, var_name, ref_level) {
  for (dataset in data_list) {
    if (var_name %in% colnames(get(dataset))) {
      assign(dataset, within(get(dataset), {
        eval(parse(text = paste0(var_name, " <- relevel(as.factor(", var_name, "), ref = '", ref_level, "')")))
      }), envir = .GlobalEnv)
    }
  }
}

# Convert outcome to factor function
convert_to_factor <- function(data_list, var_name) {
  for (dataset in data_list) {
    if (var_name %in% colnames(get(dataset))) {
      assign(dataset, within(get(dataset), {
        eval(parse(text = paste0(var_name, " <- as.factor(", var_name, ")")))
      }), envir = .GlobalEnv)
    }
  }
}



# Function to relevel a specified column in each data frame in a list
relevel_factors <- function(datasets, column_name, ref_level) {
  lapply(datasets, function(df) {
    if (column_name %in% names(df)) {
      df[[column_name]] <- relevel(factor(df[[column_name]]), ref = ref_level)
    }
    df  # Return modified data frame
  })
}

# Function to convert a specified column to a factor in each data frame in a list
convert_to_factor <- function(datasets, column_name) {
  lapply(datasets, function(df) {
    if (column_name %in% names(df)) {
      df[[column_name]] <- as.factor(df[[column_name]])
    }
    df  # Return modified data frame
  })
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
      fit_model_OR(datasets[[name]], name)
    })
  )
  
  # Clean up the results for plotting
  coeff_results <- coeff_results %>%
    select(Term, Estimate, Dataset, ci_lo, ci_hi, Std.Error) %>%
    filter(Term != "(Intercept)")  # Exclude intercept for cleaner plot
  
  # Create a new variable "Method" based on the dataset name
  coeff_results <- coeff_results %>%
    mutate(Method = case_when(
      grepl("syn_data_small_privbayes", Dataset) ~ "PrivBayes",
      grepl("syn_data_small_arf", Dataset) ~ "ARF",
      grepl("syn_data_small_synthpop", Dataset) ~ "Synthpop",
      grepl("syn_data_small_ctgan", Dataset) ~ "CTGAN",
      grepl("syn_data_small_tvae", Dataset) ~ "TVAE",
      grepl("syn_data_small_tabsyn", Dataset) ~ "TABSYN",
      Dataset == "data_small" ~ "Real",
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
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey", linewidth = 1) +  
    theme_bw() +
    labs(title = "",
         x = "Odds Ratio",
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
        "itt_treatrt-PA" = "ITT treatment: rt-PA",
        "nihss" = "NIH Stroke scale",
        "randdelay" = "Delay from stroke to randomisation in hours",
        "vis_infarctYes" = "Visible signs of infarct"
      )) +
    theme(axis.text.y = element_text(angle = 0, hjust = 1),
          legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) 
  
  print(regression_coeff_plot_combined)
  
  ggsave(output_path, plot = regression_coeff_plot_combined, width = 8, height = 8)
}















## Global utility ####

# Function to compute mean S_pMSE for a given method and sample size
compute_s_pMSE <- function(method, m) {
  
  # Load files dynamically
  path <- "IST-3 Data/Data"
  pattern <- paste0("syn_data_small_", method, "_(", paste(1:m, collapse = "|"), ")\\.csv$")
  dateien <- list.files(path = file.path(path, method), pattern = pattern, full.names = TRUE)
  
  print(dateien)
  
  # Read data
  syn_data <- lapply(dateien, read.csv)
  
  # Convert categorical variables
  syn_data <- lapply(syn_data, function(df) {
    df$outcome <- as.factor(df$outcome)
    df$itt_treat <- as.factor(df$itt_treat)
    df$vis_infarct <- as.factor(df$vis_infarct)
    return(df)
  })
  
  # Compute mean S_pMSE
  compare_results <- lapply(syn_data, compare, data_small)
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
  
  #threshold <- 5  # Predefined threshold
  threshold <- quantile(match_attack, 0.25) # 2
  predicted_membership <- match_attack <= threshold
  actual_membership <- 1:nrow(attack_data) %in% 1:nrow(attack_train)  # True labels
  
  precision <- sum(predicted_membership & actual_membership) / sum(predicted_membership)
  recall <- sum(predicted_membership & actual_membership) / sum(actual_membership)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(f1_score)
}





evaluate_method <- function(method, m, n, N) {
  
  path <- paste0("IST-3 Data/Data/train_data/", method, "/")
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
  path <- paste0("IST-3 Data/Data/", tolower(method), "/")
  pattern <- paste0("^syn_data_small_", tolower(method), "_.*\\.csv$")
  
  # List all files
  all_files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  
  # Compute TCAP scores for each m value
  tcap_scores <- sapply(m_values, function(m) {
    # Build regex dynamically to match correct files for given m
    regex_m <- if (m == 10) {
      "syn_data_small_.*_(1|2|3|4|5|6|7|8|9|10)\\.csv$"
    } else if (m == 50) {
      "syn_data_small_.*_([1-9]|[1-4][0-9]|50)\\.csv$"
    } else { # Default case for m = 5
      "syn_data_small_.*_[1-5]\\.csv$"
    }
    
    # Select matching files
    selected_files <- all_files[grepl(regex_m, all_files)]
    syn_data_list <- lapply(selected_files, read.csv)
    
    syn_data_list <- lapply(syn_data_list, function(df) {
      df$outcome <- as.factor(df$outcome)
      return(df)
    })
    
    # Compute TCAP score
    compute_TCAP_multiple(real_data, syn_data_list, key_vars = c("age"), target_var = "outcome")
  })
  
  return(tcap_scores)
}











# More evaluation criteria ####


## Resemblance ####

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

# Define evaluation function for classification metrics
evaluate_regression_per_dataset <- function(method, m, real_test, syn_data_list) {
  
  results <- data.frame(Method = character(), M = integer(), R2 = numeric(), RMSE = numeric(), MAE = numeric())
  
  for (syn_data in syn_data_list) {
    
    # Convert categorical variables to factors
    syn_data[] <- lapply(syn_data, function(x) if(is.character(x)) as.factor(x) else x)
    real_test[] <- lapply(real_test, function(x) if(is.character(x)) as.factor(x) else x)
    
    syn_data[[target_var]] <- as.factor(syn_data[[target_var]])
    real_test[[target_var]] <- as.factor(real_test[[target_var]])
    
    # Train Random Forest model on synthetic data
    model_synth <- randomForest(as.formula(paste(target_var, "~ .")), data = syn_data)
    
    # Predict on real test data
    pred_synth <- predict(model_synth, real_test)
    
    # Compute regression metrics
    metrics_synth <- postResample(pred_synth, real_test[[target_var]])
    
    # Store results
    results <- bind_rows(results, data.frame(Method = method, M = m, 
                                             Accuracy = metrics_synth[1]))
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
      outcome = as.integer(outcome),
      itt_treat = ifelse(itt_treat == "rt-PA", 1, 0),
      vis_infarct = ifelse(vis_infarct == "Yes", 1, 0)
    ) 
}


