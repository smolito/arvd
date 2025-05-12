library(tidyverse)
library(ggplot2)

data_vino <- read.csv("data-input/ukol_A_vino.csv") %>%
  select(-X)

nas <- data_vino %>%
  filter(if_any(everything(), is.na))

if (nrow(nas) == 0) {
  message("no rows contain NA values")
}

colnames(data_vino)
dim(data_vino)
# all attributes numerical, no missing values
summary(data_vino)

# handling outliers

outlier_bounds <- function(df) {
  # calculate outlier bounds for each variable in df
  bounds <- df %>%
    summarise(across(everything(), list(
      lower = ~ quantile(., 0.25) - 1.5 * IQR(.),
      upper = ~ quantile(., 0.75) + 1.5 * IQR(.)
    )))

  return(bounds)
}

bounds <- outlier_bounds(data_vino)

vino_filtered <- data_vino %>%
  filter(across(.cols = -c(quality, sweet), ~
    . >= bounds[[paste0(cur_column(), "_lower")]] &
      . <= bounds[[paste0(cur_column(), "_upper")]]))

# dim(vino_filtered)
# summary(vino_filtered)

data_vino_norm <- imap_dfr(vino_filtered, \(x, idx){
  if (idx != "quality") {
    x <- (x - min(x)) / (max(x) - min(x))
  }
  return(x)
})

summary(data_vino_norm)

# vizualizace práce s daty ----
ggplot() +
  geom_histogram(
    data = data_vino,
    aes(x = total.sulfur.dioxide),
    binwidth = 4,
    fill = "black",
    color = "red",
    alpha = 0.6
  ) +
  geom_histogram(
    data = vino_filtered,
    aes(x = total.sulfur.dioxide),
    binwidth = 4,
    fill = "yellow",
    color = "black",
    alpha = 0.3
  ) +
  labs(title = "Rozložení celkového obsahu síry před a po odstranění odlehlých hodnot") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "white")) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Legenda", labels = c("Před", "Po")))

vino_filtered %>%
  select(density, free.sulfur.dioxide, total.sulfur.dioxide, density, alcohol) %>%
  pairs()

# acidites seem to not trend w pH

vino_filtered %>%
  select(fixed.acidity, volatile.acidity, citric.acid, pH) %>%
  pairs()

vino_filtered %>%
  select(density, pH, alcohol, quality) %>%
  pairs()

# lm ----

dta <- data_vino_norm

m <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + chlorides +
  free.sulfur.dioxide + total.sulfur.dioxide + density + pH +
  sulphates + alcohol + sweet, data = dta)

coef(m)
summary(m)

# trying out my model (on the same data)

# round to get a categorical variable
predpoved <- predict(m, newdata = dta) %>% round()

dta$predicted_quality <- predpoved

porovnani <- tibble(
  actual = dta$quality,
  predicted = dta$predicted_quality
)
