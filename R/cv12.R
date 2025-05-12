library(arules)
weather <- read.csv("data-input/weather.csv")
# cast� skupiny polozek (target = "frequent itemsets") s podporou vets� nez 0.22
sets <- apriori(weather,
  parameter = list(minlen = 1, supp = 0.22, target = "frequent itemsets")
)
# 0.22 * 14 = 3
# tj. aby byla mnozina polozek uvazov�na, mus� b�t pr�tomna v >3 transakc�ch
summary(sets)
inspect(sets)

# asociacn� pravidla s podporou supp>0.22 a se spolehlivost� conf=1
rules <- apriori(weather,
  parameter = list(minlen = 2, supp = 0.22, conf = 1, target = "rules"),
  appearance = list(rhs = c("play=no", "play=yes"), default = "lhs")
)
summary(rules)
# inspect(rules[1:10])
inspect(rules)

# vizualizace

library(vcd)
library(arulesViz)

rules <- apriori(weather,
  parameter = list(minlen = 2, supp = 0.2, conf = 0.5, target = "rules"),
  appearance = list(rhs = c("play=no", "play=yes"), default = "lhs")
)
plot(rules, interactive = TRUE)
plot(rules, method = "grouped")
plot(rules, method = "graph")
plot(rules, method = "paracoord")


mosaicplot(~play,
  data = weather,
  main = "Tennis",
  shade = FALSE,
  legend = FALSE
)

mosaicplot(~ outlook + play,
  data = weather,
  main = "Tennis",
  shade = FALSE,
  legend = FALSE
)
mosaicplot(~ play + humidity, data = weather, main = "Tennis", shade = FALSE, legend = FALSE)
mosaicplot(~ outlook + humidity + play, data = weather, main = "Tennis", shade = FALSE, legend = FALSE)


#####
# mushrooms
mushroom <- read.csv("data-input/mushroom.csv")
# e = edible (pozivateln�)
rules <- apriori(mushroom,
  parameter = list(minlen = 2, supp = 0.3, conf = 1, target = "rules"),
  appearance = list(rhs = c("classes=e"), default = "lhs")
)
summary(rules)
inspect(rules)

plot(rules, method = "graph", engine = "htmlwidget")
