# vykresleni:
with(iris[iris$Species == "virginica", ], plot(Petal.Length ~ Sepal.Length))

# lepsi vykresleni, aby nesplyvaly pozorovani, u nichz byly namereny shodne hodnoty:
with(iris[iris$Species == "virginica", ], plot(Petal.Length ~ jitter(Sepal.Length)))

# uprava mezi (aby byl videt pocatek souradne soustavy):
with(iris[iris$Species == "virginica", ], plot(Petal.Length ~ jitter(Sepal.Length),
  xlim = c(0, max(Sepal.Length)),
  ylim = c(0, max(Petal.Length))
))

# regresni model:
m <- lm(Petal.Length ~ Sepal.Length, iris[iris$Species == "virginica", ])
# alternativne:
m <- lm(Petal.Length ~ Sepal.Length, iris, subset = Species == "virginica")

# prohlidka modelu:
m
coef(m)
summary(m)

# vizualizace modelu:
abline(coef(m), col = "red")
#############
# VICE REGRESORU
tuk <- read.csv("data-input/tuk.csv")
m <- lm(fat ~ height + weight, data = tuk)
m
summary(m)

############
# VLIV ODLEHL�CH POZOROV�N�
# pouzijeme kosatce virginica
d <- iris[iris$Species == "virginica", ]
# a umele zmenime delku korunniho listku u rostliny s nejdelsim kalistnim listkem:
d$Petal.Length[d$Sepal.Length == max(d$Sepal.Length)] <- -5

# data vykreslime:
plot(Petal.Length ~ jitter(Sepal.Length), d)

# modelujeme:
m <- lm(Petal.Length ~ Sepal.Length, d)
summary(m)

# a vykreslime nalezeny model
abline(coef(m), col = "red")

################
# INSURANCE DATA
################

## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("data-input/insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable
summary(insurance$expenses)

# histogram of insurance charges
hist(insurance$expenses)

# table of region
table(insurance$region)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])

# more informative scatterplot matrix
# library(psych)
# pairs(insurance[c("age", "bmi", "children", "expenses")])

## Step 3: Training a model on the data ----
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
  data = insurance
)
ins_model <- lm(expenses ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
ins_model

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
  bmi30 * smoker + region, data = insurance)

summary(ins_model2)
