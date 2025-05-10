
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
  C2ST_AUC = c(0.723, 0.904, 0.696, 0.550, 0.605, 0.905),
  Alpha_Precision = c(0.962, 0.882, 0.673, 0.992, 0.979, 0.965),
  Beta_Recall = c(0.487, 0.401, 0.294, 0.539, 0.482, 0.473),
  S_pMSE = c(243.277, 797.856, 335.188, 240.991, 250.205, 332.434),
  CIO = c(0.879, 0.550, 0.285, 0.893, 0.856, 0.413),
  MASD = c(0.205, 4.060, 16.300, 0.601, 0.072, 10.600),
  Accuracy = c(0.750, 0.719, 0.519, 0.747, 0.753, 0.710)
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
  Accuracy = normalize_0_1(raw_data$Accuracy)
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
  "Accuracy"
)

# ---------------------------
# Create radar plots
# ---------------------------

# Open a PDF device to save all the plots in one file
pdf(file = "IST-3 Data/evaluation_utility/all_radar_plots_normalized.pdf", width = 9, height = 6)

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
  Accuracy = rank_reversed(raw_data$Accuracy)
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
  "Accuracy"
)


# Open a PDF device to save all the plots in one file
pdf(file = "IST-3 Data/evaluation_utility/all_radar_plots.pdf", width = 9, height = 6)

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
  IMS = c(0.000, 0.000, 0.004, 0.123, 0.000, 0.000),
  DCR = c(0.013, 0.009, 0.018, 0.012, 0.014, 0.010),
  NNDR = c(0.723, 0.708, 0.720, 0.715, 0.731, 0.721),
  M = c(-0.039, -0.049, -0.039, -0.036, -0.012, -0.046),
  TCAP = c(0.498, 0.923, NA, 0.945, 0.884, NA),
  Authenticity = c(0.497, 0.596, 0.614, 0.472, 0.504, 0.523)
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
pdf(file = "IST-3 Data/evaluation_disclosure_risk/disclosure_risk_radar.pdf", width = 9, height = 6)

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


