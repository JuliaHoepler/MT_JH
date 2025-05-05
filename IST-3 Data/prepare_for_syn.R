library(data.table)

data_small <- setDT(readRDS("Daten/data_small.Rds"))
data_medium <- setDT(readRDS("Daten/data_medium.Rds"))
data_big <- setDT(readRDS("Daten/data_big.Rds"))

# Remove id cols and sort cols
data_medium[, randhosp_id := NULL]
data_big[, `:=`(randhosp_id = NULL, randpat_id = NULL)]
setcolorder(data_medium, names(data_small))
setcolorder(data_big, names(data_small))

# Fill NAs in data_medium and data_big for TabSyn

# data_medium

factor_cols_medium <- which(lapply(data_medium, is.factor) == T)
numeric_cols_medium <- which(lapply(data_medium, is.numeric) == T)
na_cols_medium <- which(lapply(data_medium, \(x) sum(is.na(x))) > 0)

na_factor_cols_medium <- intersect(factor_cols_medium, na_cols_medium)
na_numeric_cols_medium <- intersect(numeric_cols_medium, na_cols_medium)

data_medium_complete <- copy(data_medium)
data_medium_complete[, (na_factor_cols_medium) := lapply(.SD, \(x) {
  x[is.na(x)] <- sample(x[!is.na(x)], sum(is.na(x)), replace = T)
  x
}), .SDcols = na_factor_cols_medium]

data_medium_complete[, (na_numeric_cols_medium) := lapply(.SD, \(x) {
  x[is.na(x)] <- median(x, na.rm = T)
  x
}), .SDcols = na_numeric_cols_medium]

data_medium_complete[data_medium_complete == "None]"] <- 
  
  # data_big
  
  factor_cols_big <- which(lapply(data_big, is.factor) == T)
numeric_cols_big <- which(lapply(data_big, is.numeric) == T)
na_cols_big <- which(lapply(data_big, \(x) sum(is.na(x))) > 0)

na_factor_cols_big <- intersect(factor_cols_big, na_cols_big)
na_numeric_cols_big <- intersect(numeric_cols_big, na_cols_big)

data_big_complete <- copy(data_big)
data_big_complete[, (na_factor_cols_big) := lapply(.SD, \(x) {
  x[is.na(x)] <- sample(x[!is.na(x)], sum(is.na(x)), replace = T)
  x
}), .SDcols = na_factor_cols_big]

data_big_complete[, (na_numeric_cols_big) := lapply(.SD, \(x) {
  x[is.na(x)] <- median(x, na.rm = T)
  x
}), .SDcols = na_numeric_cols_big]

# Save data


saveRDS(data_small, "Daten/data_small.Rds")
saveRDS(data_medium, "Daten/data_medium.Rds")
saveRDS(data_medium_complete, "Daten/data_medium_complete.Rds")
saveRDS(data_big, "Daten/data_big.Rds")
saveRDS(data_big_complete, "Daten/data_big_complete.Rds")


