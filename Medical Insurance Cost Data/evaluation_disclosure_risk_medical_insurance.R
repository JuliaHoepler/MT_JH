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
#m_values <- c(5, 10, 50)
m_values <- c(5)
methods <- c("synthpop", "arf", "privbayes", "ctgan", "tvae", "tabsyn")


n <- nrow(real_data)
N <- 55440 #(N = n/t)

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

# Print the results table
kable(tcap_results, caption = "TCAP Scores für verschiedene Methoden und m-Werte")

# Save the TCAP results to a CSV file
write.csv(tcap_results, "Medical Insurance Cost Data/evaluation_disclosure_risk/TCAP_scores.csv", row.names = FALSE)





## 2.2 Use classifier ####




# ### ARF ####
# 
# synthetic_files <- syn_data_list_train_arf_m5
# 
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "charges"  
# 
# # ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Klassifikator trainieren
#   set.seed(42)
#   train_control <- trainControl(method = "cv", number = 10)  # 10-fold Cross Validation
#   
#   model_rf <- train(
#     as.formula(paste(confidential_attr, "~ .")),
#     data = synthetic_data,   
#     method = "rf",           
#     trControl = train_control
#   )
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(model_rf, real_data)
#   
#   # ADR berechnen
#   adr <- mean(predictions == real_data[[confidential_attr]])  # Trefferquote
#   adr_values <- c(adr_values, adr)  # ADR speichern
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# table(predictions)  # Zeigt an, welche Klassen vorhergesagt wurden
# 
# boxplot(real_data$charges, synthetic_data$charges)
# 
# 
# str(real_data)
# str(synthetic_data)
# 
# 
# 
# 
# # 4️⃣ Durchschnitts-ADR berechnen & ausgeben
# average_adr <- mean(adr_values)
# cat("\n🔹 Durchschnittlicher ADR über alle synthetischen Datensätze:", average_adr, "\n")
# 
# # 5️⃣ Ergebnisse speichern
# adr_results <- data.frame(dataset = synthetic_files, ADR = adr_values)
# write.csv(adr_results, "adr_results_all_synthetic.csv", row.names = FALSE)
# write.csv(data.frame(Average_ADR = average_adr), "adr_average.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# 
# ### CTGAN ####
# 
# synthetic_files <- syn_data_list_train_ctgan_m5
# 
# 
# #### Sensitive variable: age ####
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "age"  
# 
# # 3️⃣ ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   file <- "Medical Insurance Cost Data/Data/ctgan/m5/syn_real_train_ctgan_1.csv"
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Random Forest trainieren
#   set.seed(42)
#   rf_model <- ranger(
#     formula = as.formula(paste(confidential_attr, "~ .")), 
#     data = synthetic_data, 
#     num.trees = 500,  # Mehr Bäume für bessere Stabilität
#     importance = "impurity"  # Feature-Importance speichern
#   )
#   
#   # Actual values
#   real_data[[confidential_attr]]
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(rf_model, data = real_data)$predictions
#   
#   # ADR berechnen (Mean Absolute Error, da age numerisch ist)
#   adr <- mean(abs(predictions - real_data[[confidential_attr]]))
#   adr_values <- c(adr_values, adr)
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# # Boxplot für Altersverteilung in Echt- & Synthetik-Daten
# boxplot(real_data$age, synthetic_data$age, col = "gray", names = c("Echt", "Synthetisch"))
# 
# # Durchschnitts-ADR berechnen & ausgeben
# average_adr_ctgan_5_age <- mean(adr_values)
# cat("Durchschnittlicher ADR-Wert:", average_adr_ctgan_5_age, "\n")
# 
# 
# 
# 
# #### Sensitive variable: sex ####
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "sex"  
# 
# # 3️⃣ ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Klassifikator trainieren
#   set.seed(42)
#   train_control <- trainControl(method = "cv", number = 10)  # 10-fold Cross Validation
#   
#   model_rf <- train(
#     as.formula(paste(confidential_attr, "~ .")),
#     data = synthetic_data,   
#     method = "rf",           
#     trControl = train_control
#   )
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(model_rf, real_data)
#   
#   # ADR berechnen
#   adr <- mean(predictions == real_data[[confidential_attr]])  # Trefferquote
#   adr_values <- c(adr_values, adr)  # ADR speichern
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# 
# 
# 
# 
# # 4️⃣ Durchschnitts-ADR berechnen & ausgeben
# average_adr_ctgan_5_sex <- mean(adr_values)
# average_adr_ctgan_5_sex
# 
# # 5️⃣ Ergebnisse speichern
# adr_results_ctgan_5_sex <- data.frame(dataset = synthetic_files, ADR = adr_values)
# write.csv(adr_results_ctgan_5_sex, "adr_results_ctgan_5_sex.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #### Sensitive variable: bmi ####
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "bmi"  
# 
# # 3️⃣ ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Klassifikator trainieren
#   set.seed(42)
#   train_control <- trainControl(method = "cv", number = 10)  # 10-fold Cross Validation
#   
#   model_rf <- train(
#     as.formula(paste(confidential_attr, "~ .")),
#     data = synthetic_data,   
#     method = "rf",           
#     trControl = train_control
#   )
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(model_rf, real_data)
#   
#   # ADR berechnen
#   adr <- mean(predictions == real_data[[confidential_attr]])  # Trefferquote
#   adr_values <- c(adr_values, adr)  # ADR speichern
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# 
# 
# 
# 
# # 4️⃣ Durchschnitts-ADR berechnen & ausgeben
# average_adr_ctgan_5_bmi <- mean(adr_values)
# average_adr_ctgan_5_bmi
# 
# # 5️⃣ Ergebnisse speichern
# adr_results_ctgan_5_bmi <- data.frame(dataset = synthetic_files, ADR = adr_values)
# write.csv(adr_results_ctgan_5_bmi, "adr_results_ctgan_5_bmi.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# 
# #### Sensitive variable: children ####
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "children"  
# 
# # 3️⃣ ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Klassifikator trainieren
#   set.seed(42)
#   train_control <- trainControl(method = "cv", number = 10)  # 10-fold Cross Validation
#   
#   model_rf <- train(
#     as.formula(paste(confidential_attr, "~ .")),
#     data = synthetic_data,   
#     method = "rf",           
#     trControl = train_control
#   )
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(model_rf, real_data)
#   
#   # ADR berechnen
#   adr <- mean(predictions == real_data[[confidential_attr]])  # Trefferquote
#   adr_values <- c(adr_values, adr)  # ADR speichern
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# 
# # 4️⃣ Durchschnitts-ADR berechnen & ausgeben
# average_adr_ctgan_5_children <- mean(adr_values)
# average_adr_ctgan_5_children
# 
# # 5️⃣ Ergebnisse speichern
# adr_results_ctgan_5_children <- data.frame(dataset = synthetic_files, ADR = adr_values)
# write.csv(adr_results_ctgan_5_children, "adr_results_ctgan_5_children.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# #### Sensitive variable: smoker ####
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "smoker"  
# 
# # 3️⃣ ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Klassifikator trainieren
#   set.seed(42)
#   train_control <- trainControl(method = "cv", number = 10)  # 10-fold Cross Validation
#   
#   model_rf <- train(
#     as.formula(paste(confidential_attr, "~ .")),
#     data = synthetic_data,   
#     method = "rf",           
#     trControl = train_control
#   )
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(model_rf, real_data)
#   
#   # ADR berechnen
#   adr <- mean(predictions == real_data[[confidential_attr]])  # Trefferquote
#   adr_values <- c(adr_values, adr)  # ADR speichern
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# 
# # 4️⃣ Durchschnitts-ADR berechnen & ausgeben
# average_adr_ctgan_5_smoker <- mean(adr_values)
# average_adr_ctgan_5_smoker
# 
# # 5️⃣ Ergebnisse speichern
# adr_results_ctgan_5_smoker <- data.frame(dataset = synthetic_files, ADR = adr_values)
# write.csv(adr_results_ctgan_5_smoker, "adr_results_ctgan_5_smoker.csv", row.names = FALSE)
# 
# 
# 
# 
# 
# 
# #### Sensitive variable: region ####
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "region"  
# 
# # 3️⃣ ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Klassifikator trainieren
#   set.seed(42)
#   train_control <- trainControl(method = "cv", number = 10)  # 10-fold Cross Validation
#   
#   model_rf <- train(
#     as.formula(paste(confidential_attr, "~ .")),
#     data = synthetic_data,   
#     method = "rf",           
#     trControl = train_control
#   )
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(model_rf, real_data)
#   
#   # ADR berechnen
#   adr <- mean(predictions == real_data[[confidential_attr]])  # Trefferquote
#   adr_values <- c(adr_values, adr)  # ADR speichern
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# 
# # 4️⃣ Durchschnitts-ADR berechnen & ausgeben
# average_adr_ctgan_5_region <- mean(adr_values)
# average_adr_ctgan_5_region
# 
# # 5️⃣ Ergebnisse speichern
# adr_results_ctgan_5_region <- data.frame(dataset = synthetic_files, ADR = adr_values)
# write.csv(adr_results_ctgan_5_region, "adr_results_ctgan_5_region.csv", row.names = FALSE)
# 
# 
# 
# 
# #### Sensitive variable: charges ####
# 
# # Vertrauliches Attribut definieren
# confidential_attr <- "charges"  
# 
# # 3️⃣ ADR für jeden synthetischen Datensatz berechnen
# adr_values <- numeric()  # Liste zur Speicherung der ADR-Werte
# 
# for (file in synthetic_files) {
#   cat("Verarbeite:", file, "\n")
#   
#   # Synthetischen Datensatz laden
#   synthetic_data <- read.csv(file)
#   
#   # Klassifikator trainieren
#   set.seed(42)
#   train_control <- trainControl(method = "cv", number = 10)  # 10-fold Cross Validation
#   
#   model_rf <- train(
#     as.formula(paste(confidential_attr, "~ .")),
#     data = synthetic_data,   
#     method = "rf",           
#     trControl = train_control
#   )
#   
#   # Vorhersage auf Originaldaten
#   predictions <- predict(model_rf, real_data)
#   
#   # ADR berechnen
#   adr <- mean(predictions == real_data[[confidential_attr]])  # Trefferquote
#   adr_values <- c(adr_values, adr)  # ADR speichern
#   
#   # Ergebnis ausgeben
#   cat("ADR für", file, ":", adr, "\n")
# }
# 
# boxplot(real_data$charges, synthetic_data$charges)
# 
# 
# 
# # 4️⃣ Durchschnitts-ADR berechnen & ausgeben
# average_adr_ctgan_5_charges <- mean(adr_values)
# average_adr_ctgan_5_charges
# 
# # 5️⃣ Ergebnisse speichern
# adr_results <- data.frame(dataset = synthetic_files, ADR = adr_values)
# write.csv(adr_results, "adr_results_all_synthetic.csv", row.names = FALSE)
# write.csv(data.frame(Average_ADR = average_adr), "adr_average.csv", row.names = FALSE)
# 




# 3. Measures of record-level similarity ####







