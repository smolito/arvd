babies = read.csv("data-input/babies.csv")

# Funkce pro výpočet modu
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Sloupce s průměrnou imputací
mean_impute_cols <- c("gestation", "height", "weight")

# Sloupce s imputací podle modu (nejčastější hodnoty)
mode_impute_cols <- c("parity", "smoke", "age")

# Imputace průměrem
for (col in mean_impute_cols) {
  babies[[col]][is.na(babies[[col]])] <- mean(babies[[col]], na.rm = TRUE)
}

# Imputace modem
for (col in mode_impute_cols) {
  babies[[col]][is.na(babies[[col]])] <- get_mode(babies[[col]])
}

# Výstupní souhrn
summary(babies)

babies_no_nas = write.csv(babies, "data-input/babies_no_nas.csv")
