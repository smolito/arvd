library(dplyr)

data_raw = read.csv("data-input/uB/ukol_B_cirhoza.csv")

summary(data_raw)

# jak naložit s NA?
# hodnoty chybí jen u "Albumin_and_Globulin_Ratio"
# doplň průměrnou hodnotu podle gender, age, skupina (dataset)

data_fill_nas <- data_raw %>%
  group_by(Age, Gender, Dataset) %>%
  mutate(Albumin_and_Globulin_Ratio = if_else(
    is.na(Albumin_and_Globulin_Ratio),
    mean(Albumin_and_Globulin_Ratio, na.rm = TRUE),
    Albumin_and_Globulin_Ratio)
    )

summary(data_fill_nas)

# normalizace - pro k-means <0,1>, pro dendrogram z-score

data_norm = data_fill_nas %>%
  mutate(across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.))))

# z: (hodnota - průměr)/odchylka
data_zscore <- data_fill_nas %>%
  mutate(across(where(is.numeric), ~ scale(.)))
