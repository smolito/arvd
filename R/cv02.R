library(datasets)
data <- iris %>%
  mutate(Species = factor(Species))

# popis dat
dim(data)
names(data)
data[1:5, ]
str(data)
summary(data)

dsum <- apply(data[, 1:4], 2, mean)
dsum <- tapply(data$Sepal.Length, data$Species, mean)

# pokrocilou manipulaci s daty nab�z� knihovna dplyr
library(dplyr)
means1 <- data %>%
  group_by(Species) %>%
  summarize(my_mean = mean(Sepal.Length))

means_all <- data %>%
  group_by(Species) %>%
  summarize(across(1:4, mean))
means_all
# uzitecn� funkce - mean, sd, median, is.na, is.nan, is.finite, ...

# grafy v R
plot(data$Sepal.Length)
plot(data$Sepal.Length, type = "l")
plot(data$Sepal.Length, type = "b")
plot(data$Sepal.Length, data$Sepal.Width)
plot(data$Sepal.Length, data$Sepal.Width, col = data$Species)
plot(data$Sepal.Length, data$Sepal.Width, col = data$Species, pch = 19)
plot(data$Sepal.Length,
  data$Sepal.Width,
  col = data$Species,
  pch = 19,
  main = "Edgar Anderson's Iris data",
  xlab = "Sepal Length",
  ylab = "Sepal Width"
)

# legenda
legend("topright",
  legend = levels(data$Species),
  pch = 19,
  col = 1:length(levels(data$Species))
)

# vykreslit body
avgSepal.Length <- tapply(data$Sepal.Length, data$Species, mean)
avgSepal.Width <- tapply(data$Sepal.Width, data$Species, mean)
points(avgSepal.Length, avgSepal.Width, pch = "O", cex = 1, col = "red")

# vykreslit c�ry
lines(lowess(
  data$Sepal.Length[data$Species == "setosa"],
  data$Sepal.Width[data$Species == "setosa"]
))

lines(lowess(
  data$Sepal.Length[data$Species == "versicolor"],
  data$Sepal.Width[data$Species == "versicolor"]
), col = 2)

lines(lowess(
  data$Sepal.Length[data$Species == "virginica"],
  data$Sepal.Width[data$Species == "virginica"]
), col = 3)

# dals� elementy - line(), abline(),

# BOXPLOT

boxplot(data$Sepal.Width ~ data$Species)

### Sepal.Length - Setosa

dd <- data$Sepal.Length[data$Species == "setosa"]
median(dd)
quantile(dd)
IQR(dd)
5.2 + 1.5 * 0.4
4.8 - 1.5 * 0.4
min(dd)

boxplot(dd)
myjitter <- jitter(rep(1, length(dd)), amount = 0.05)
points(myjitter, dd, pch = 20, col = rgb(0, 0, 0, .2), cex = 3)
axis(2, at = seq(0, 6, 0.05))

#### Sepal.Width - Setosa

dd2 <- data$Sepal.Width[data$Species == "setosa"]
qnt <- quantile(dd2)
IQR(dd2)
qnt[[4]] + 1.5 * IQR(dd2)
qnt[[2]] - 1.5 * IQR(dd2)

boxplot(dd2)
myjitter <- jitter(rep(1, length(dd2)), amount = 0.05)
points(myjitter, dd2, pch = 20, col = rgb(0, 0, 0, .2), cex = 3)
axis(2, at = seq(0, 6, 0.05))

###############
# Basic boxplot
boxplot(data$Sepal.Width ~ data$Species, col = terrain.colors(3))

# Add data points
mylevels <- levels(data$Species)
levelProportions <- summary(data$Species) / nrow(data)

for (i in 1:length(mylevels)) {
  thislevel <- mylevels[i]
  thisvalues <- data[data$Species == thislevel, "Sepal.Width"]

  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter <- jitter(rep(i, length(thisvalues)), amount = levelProportions[i] / 2)
  points(myjitter, thisvalues, pch = 20, col = rgb(0, 0, 0, .2))
}

hist(data$Sepal.Length)

hist(data$Sepal.Length[data$Species == "setosa"],
  50,
  xlim = c(
    min(data$Sepal.Length),
    max(data$Sepal.Length)
  ),
  col = 2,
  main = "Edgar Anderson's Iris data"
)
hist(data$Sepal.Length[data$Species == "versicolor"], 50, col = 3, add = T)
hist(data$Sepal.Length[data$Species == "virginica"], 50, col = 4, add = T)

pairs(data)

#####

data <- airquality

tapply(data$Temp, data$Month, median)

boxplot(data$Temp ~ data$Month)

boxplot(data$Temp ~ data$Month, col = rgb(0.5, 0.5, 1), xlab = "mes�c meren�", ylab = "nam��en� teplota [deg F]", main = "Maxim�ln� denn� teploty ve stupn�ch Fahrenheita na leti�ti La Guardia")

######
# Create data
Ixos <- rnorm(4000, 100, 30)
Primadur <- rnorm(4000, 200, 30)

# Represent separately first
par(mfrow = c(1, 2))
hist(Ixos, breaks = 30, xlim = c(0, 300), col = rgb(1, 0, 0, 0.5), xlab = "v�ska [cm]", ylab = "pocet rostlin", main = "Nameren� v�ska rostliny 1")
hist(Primadur, breaks = 30, xlim = c(0, 300), col = rgb(0, 0, 1, 0.5), xlab = "v�ska [cm]", ylab = "pocet rostlin", main = "Nameren� v�ska rostliny 2")


hist(Ixos,
  breaks = 30, xlim = c(0, 300), col = rgb(1, 0, 0, 0.5), xlab = "v�ska [cm]",
  ylab = "pocet rostlin", main = "Distribuce nameren�ch v�sek rostlin 1 a 2"
)
hist(Primadur, breaks = 30, xlim = c(0, 300), col = rgb(0, 0, 1, 0.5), add = T)
legend("topright", legend = c("rostlina 1", "rostlina 2"), col = c(
  rgb(1, 0, 0, 0.5),
  rgb(0, 0, 1, 0.5)
), pt.cex = 2, pch = 15)
###########
# GGPLOT 2

library(ggplot2)
ggplot(data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()
ggplot(data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_line()
ggplot(data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species)) +
  geom_line()
ggplot(data, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_line(aes(color = Species))
ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(aes(size = 0.5)) +
  geom_line()

ggplot(data, aes(x = Species, y = Petal.Width)) +
  geom_boxplot()

ggplot(data, aes(x = Sepal.Length)) +
  geom_histogram()
ggplot(data, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.1)
ggplot(data, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.1, aes(colour = Species))
ggplot(data, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.1, aes(fill = Species))
ggplot(data, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.1, alpha = 0.4, aes(fill = Species))
# vykreslují se přes sebe, position argument nuluje překreslování do nesprávných
# kategorií, ale musím nastavit průsvitnost
ggplot(data, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.4, aes(fill = Species))

#####

# library
library(ggplot2)

# The mtcars dataset is proposed in R
head(mpg)

# Set a unique color with fill, colour, and alpha
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot(color = "red", fill = "orange", alpha = 0.2)

# Set a different color for each group
ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  geom_boxplot(alpha = 0.3) +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "BuPu")



# create dummy data
data <- data.frame(
  name = letters[1:5],
  value = sample(seq(4, 15), 5),
  sd = c(1, 0.2, 3, 2, 4)
)

ggplot(data) +
  geom_bar(aes(x = name, y = value), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = name, ymin = value - sd, ymax = value + sd), width = 0.4, colour = "orange", alpha = 0.9, size = 1.3)


##############################
# ADULTS
adult <- read.csv("data-input/adult.csv")
str(adult)
summary(adult)
dim(adult)
barplot(table(adult$workclass))
sort(table(adult$`hours.per.week`))

table(adult$class)


####
