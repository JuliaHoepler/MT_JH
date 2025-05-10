
# Load required packages
library(fmsb)
library(scales)


# Radar plots ####


#...............................................................................
## Utility ####
#...............................................................................

# Raw data 
raw_data <- data.frame(
  row.names = c("ARF", "CTGAN", "PrivBayes", "Synthpop", "TABSYN", "TVAE"),
  C2ST_AUC = c(0.606, 0.968, 0.893, 0.622, 0.696, 0.911),
  Alpha_Precision = c(0.961, 0.942, 0.725, 0.985, 0.981, 0.895),
  Beta_Recall = c(0.221, 0.197, 0.154, 0.318, 0.235, 0.166),
  S_pMSE = c(4.868, 177.928, 69.356, 1.013, 3.358, 52.204),
  CIO = c(0.655, 0.337, 0.323, 0.777, 0.842, 0.467),
  MASD = c(0.868, 7.310, 32.100, 1.680, 0.022, 18.100),
  R2 = c(0.802, 0.658, 0.342, 0.858, 0.824, 0.641)
)


### Normalized ####

# ---------------------------
# Normalization
# ---------------------------

# Normalization using fixed or natural bounds where applicable
normalize_c2st_auc <- function(x) {
  1 - abs(x - 0.5) / 0.5
}

normalize_0_1 <- function(x) {
  x  # Already between 0 and 1
}

normalize_lower_better <- function(x) {
  (max(x) - x) / (max(x) - min(x))
}

# Apply updated normalization
normalized_data <- data.frame(
  C2ST_AUC = normalize_c2st_auc(raw_data$C2ST_AUC),
  alpha_Precision = normalize_0_1(raw_data$Alpha_Precision),
  beta_Recall = normalize_0_1(raw_data$Beta_Recall),
  S_pMSE = normalize_lower_better(raw_data$S_pMSE),
  CIO = normalize_0_1(raw_data$CIO),
  MASD = normalize_lower_better(raw_data$MASD),
  R2 = normalize_0_1(raw_data$R2)
)

# ---------------------------
# Prepare radar chart data
# ---------------------------

# Add max and min rows for scaling
radar_data_full <- rbind(
  rep(1, ncol(normalized_data)),  # max = 1
  rep(0, ncol(normalized_data)),  # min = 0
  normalized_data
)

# Set row names
row.names(radar_data_full) <- c("Max", "Min", row.names(raw_data))

# Custom colors for each method
method_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")

# Method names
methods <- row.names(raw_data)

# Metric labels
labels <- c(
  "C2ST AUC",
  expression(alpha*"-Precision"),
  expression(beta*"-Recall"),
  "S_pMSE",
  "CIO",
  "MASD",
  expression(R^2)
)

# ---------------------------
# Create radar plots
# ---------------------------

# Open a PDF device to save all the plots in one file
pdf(file = "Medical Insurance Cost Data/evaluation_utility/all_radar_plots_normalized.pdf", width = 9, height = 6)

# Set up plotting area (2 rows x 3 cols)
par(mfrow = c(2, 3), mar = c(1, 1, 2, 1), oma = c(1.5, 0, 1.5, 0))


# Loop through each method to plot
for (i in 1:length(methods)) {
  method <- methods[i]
  
  # Extract only max, min, and current method
  radar_data_one <- radar_data_full[c("Max", "Min", method), ]
  
  # Draw the radar chart
  radarchart(
    radar_data_one,
    axistype = 1,
    vlabels = labels,
    pcol = method_colors[i],
    pfcol = alpha(method_colors[i], 0.4),
    plwd = 3,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "black",
    caxislabels = seq(0, 1, by = 0.2),
    vlcex = 1,
    title = method
  )
}

# Close the PDF device to save the plots
dev.off()








### Ranked #####


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
pdf(file = "Medical Insurance Cost Data/evaluation_utility/all_radar_plots.pdf", width = 9, height = 6)

# Set up the plotting area with 2 rows and 3 columns
par(mfrow = c(2, 3), mar = c(1, 1, 2, 1), oma = c(1.5, 0, 1.5, 0))

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
    vlcex = 1,
    title = method
  )
}

# Close the PDF device (this saves all plots)
dev.off()



#...............................................................................
## Disclosure risk ####
#...............................................................................

# Raw data 
disclosure_risk_data <- data.frame(
  row.names = c("ARF", "CTGAN", "PrivBayes", "Synthpop", "TABSYN", "TVAE"),
  IMS = c(0.000, 0.000, 0.000, 0.058, 0.000, 0.000),
  DCR = c(0.022, 0.021, 0.059, 0.017, 0.028, 0.027),
  NNDR = c(0.913, 0.927, 0.933, 0.889, 0.906, 0.920),
  M = c(0.001, 0.036, -0.006, 0.019, -0.010, 0.001),
  TCAP = c(0.034, 0.000, 0.000, 0.000, 0.000, 0.000),
  Authenticity = c(0.783, 0.813, 0.837, 0.698, 0.773, 0.831)
)


# Normalize the data
normalize_metric <- function(x, direction = "high", fixed_min = NULL, fixed_max = NULL) {
  if (!is.null(fixed_min) && !is.null(fixed_max)) {
    if (direction == "high") {
      return((x - fixed_min) / (fixed_max - fixed_min))
    } else {
      return((fixed_max - x) / (fixed_max - fixed_min))
    }
  } else {
    if (direction == "high") {
      return((x - min(x)) / (max(x) - min(x)))
    } else {
      return((max(x) - x) / (max(x) - min(x)))
    }
  }
}

# Apply normalization
normalized_disclosure <- data.frame(
  IMS = normalize_metric(disclosure_risk_data$IMS, direction = "high", fixed_min = 0, fixed_max = 1),
  DCR = normalize_metric(disclosure_risk_data$DCR, direction = "high", fixed_min = 0, fixed_max = 1),
  NNDR = normalize_metric(disclosure_risk_data$NNDR, direction = "low", fixed_min = 0, fixed_max = 1),  # higher NNDR is better
  M = normalize_metric(disclosure_risk_data$M, direction = "high", fixed_min = -0.05, fixed_max = 0.2),  # larger absolute error = worse
  TCAP = normalize_metric(disclosure_risk_data$TCAP, direction = "high", fixed_min = 0, fixed_max = 1),
  Authenticity = normalize_metric(disclosure_risk_data$Authenticity, direction = "low", fixed_min = 0, fixed_max = 1)
)

# Add max and min rows for scaling
radar_data_full <- rbind(
  rep(1, ncol(normalized_disclosure)),  # max = 1
  rep(0, ncol(normalized_disclosure)),  # min = 0
  normalized_disclosure
)

# Set row names
row.names(radar_data_full) <- c("Max", "Min", row.names(raw_data))


# Labels and colors
labels <- c("IMS", "DCR", "NNDR", "M", "TCAP", "Authenticity")
methods <- rownames(disclosure_risk_data)
method_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")

# Open PDF
pdf(file = "Medical Insurance Cost Data/evaluation_disclosure_risk/disclosure_risk_radar.pdf", width = 9, height = 6)

par(mfrow = c(2, 3), mar = c(1, 1, 2, 1), oma = c(1.5, 0, 1.5, 0))

# Plot per method
for (i in 1:length(methods)) {
  
  method <- methods[i]
  
  radar_data_one <- radar_data_full[c("Max", "Min", method), ]
  
  radarchart(
    radar_data_one,
    axistype = 1,
    vlabels = labels,
    pcol = method_colors[i],
    pfcol = alpha(method_colors[i], 0.4),
    plwd = 3,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "black",
    caxislabels = seq(0, 1, by = 0.2),
    vlcex = 1,
    title = method
  )
}

# Save and close
dev.off()


