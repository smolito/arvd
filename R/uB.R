library(tidyverse)
library(dendextend)
library(ggplot2)

data_raw <- read.csv("data-input/uB/ukol_B_cirhoza.csv")

summary(data_raw)

# jak naložit s NA?
# hodnoty chybí jen u "Albumin_and_Globulin_Ratio"
# doplň průměrnou hodnotu podle gender, age, skupina (dataset)

data_fill_nas = data_raw %>%
  group_by(Age, Gender, Dataset) %>%
  mutate(Albumin_and_Globulin_Ratio = if_else(
    is.na(Albumin_and_Globulin_Ratio),
    mean(Albumin_and_Globulin_Ratio, na.rm = TRUE),
    Albumin_and_Globulin_Ratio
  ))

summary(data_fill_nas)

# normalizace - pro k-means <0,1>; pro dendrogram z-score ? ovlivňuje méně výpočet vzdáleností

# ward method ~ "ward.D2" implements that criterion (Murtagh and Legendre 2014).
# With the latter, the dissimilarities are squared before cluster updating."
# from rdocumentation

data_norm <- data_fill_nas %>%
  mutate(across(where(is.numeric), ~ (. - min(.)) / (max(.) - min(.))))

dend = data_fill_nas %>% # data
  select(-c(Dataset)) %>% 
  mutate(Gender = if_else(Gender == "Male", 1, 0)) %>% # gender2numeric
  scale %>% # z: (hodnota - průměr)/odchylka
  dist %>% # distance matrix
  hclust(method = "ward.D2") %>% # hierarchický clustering 
  as.dendrogram

# nepřehledné, potřeba řezat
dend
png("vis/dendrogram_cuts.png", width = 800, height = 600)

plot(dend, main = "Celkový dendrogram s různými řezy shlukování")

colors = c("green", "blue", "red", "pink", "orange", "purple", "cyan",
            "magenta", "yellow", "brown", "gray", "black", "violet", "turquoise",
            "lime")

for (k in 2:6) {
  rect.hclust(as.hclust(dend), k = k, border = colors[k - 1])
}

dev.off()

# přidání různých řezů na průzkum, od kořene
dendrogram_data = data_fill_nas %>%
  mutate(Gender = if_else(Gender == "Male", 1, 0)) %>%
  as_tibble %>% 
  mutate(dend_k2 = as.numeric(cutree(dend, k = 2)),
         dend_k3 = as.numeric(cutree(dend, k = 3)),
         dend_k4 = as.numeric(cutree(dend, k = 4)),
         dend_k5 = as.numeric(cutree(dend, k = 5)),
         dend_k6 = as.numeric(cutree(dend, k = 6)),
         )

# realne rozdeleni skupin

true_means = data_fill_nas %>% 
  group_by(Dataset) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

# prumer pres skupiny, nechci pocitat prumer z Dataset a gender

dend_k2_means = dendrogram_data %>%
  select(-c(dend_k3, dend_k4, dend_k5, dend_k6)) %>% 
  group_by(dend_k2) %>%
  summarise(across(where(is.numeric) & !Gender & !Dataset,
                   ~ mean(.x, na.rm = TRUE)))

summary(dend_k2_means)

dend_k3_means = dendrogram_data %>%
  select(-c(dend_k2, dend_k4, dend_k5, dend_k6)) %>% 
  group_by(dend_k3) %>%
  summarise(across(where(is.numeric) & !Gender & !Dataset,
                   ~ mean(.x, na.rm = TRUE)))

summary(dend_k3_means)

dend_k4_means = dendrogram_data %>%
  select(-c(dend_k2, dend_k3, dend_k5, dend_k6)) %>% 
  group_by(dend_k4) %>%
  summarise(across(where(is.numeric) & !Gender & !Dataset,
                   ~ mean(.x, na.rm = TRUE)))

summary(dend_k4_means)

dend_k5_means = dendrogram_data %>%
  select(-c(dend_k2, dend_k3, dend_k4, dend_k6)) %>% 
  group_by(dend_k5) %>%
  summarise(across(where(is.numeric) & !Gender & !Dataset,
                   ~ mean(.x, na.rm = TRUE)))

summary(dend_k5_means)

dend_k6_means = dendrogram_data %>%
  select(-c(dend_k2, dend_k3, dend_k4, dend_k5)) %>% 
  group_by(dend_k6) %>%
  summarise(across(where(is.numeric) & !Gender & !Dataset,
                   ~ mean(.x, na.rm = TRUE)))

summary(dend_k6_means)

# přesnost shlukování? orig Dataset vs. k = 2 dend
# předpokládám, že labely odpovídají

dendrogram_data %>% count(Dataset)
dendrogram_data %>% count(dend_k2)

conf_mat = table(dendrogram_data$Dataset, dendrogram_data$dend_k2)

print(conf_mat) # problém -> 0 TP ?

acc = sum(diag(conf_mat))/sum(conf_mat)

print(paste("accuracy shlukování: ", round(acc * 100, 2), "%"))

# boxploty pro vizualizaci, potřeba long pivot pro více bxpltu v jednom figure

long_data = dendrogram_data %>% 
  pivot_longer(cols = -c(Gender, Dataset, dend_k2, dend_k3, dend_k4, dend_k5, dend_k6), 
               names_to = "promenna", values_to = "hodnota")

# bxplt pro dendrogram "rootu" vs orig skupiny
plot_k2 = ggplot(long_data, aes(x = as.factor(dend_k2), y = hodnota, fill = as.factor(dend_k2))) +
  geom_boxplot() +
  facet_wrap(~ promenna, scales = "free") +
  labs(title = "Boxploty pro shlukování k = 2", x = "shluk", y = "hodnota") +
  theme_minimal()

ggsave("vis/boxplot_k2.png", plot = plot_k2, width = 10, height = 8)

long_data_original = data_fill_nas %>%
  pivot_longer(cols = -c(Gender, Dataset), 
               names_to = "promenna", values_to = "hodnota")

plot_dataset = ggplot(long_data_original, aes(x = as.factor(Dataset), y = hodnota, fill = as.factor(Dataset))) +
  geom_boxplot() +
  facet_wrap(~ promenna, scales = "free") +
  labs(title = "Boxploty pro originální data podle Dataset", x = "Dataset", y = "hodnota") +
  theme_minimal()

ggsave("vis/boxplot_dataset.png", plot = plot_dataset, width = 10, height = 8)
