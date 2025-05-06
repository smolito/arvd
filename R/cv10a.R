# UVOD
# Pruzkum dat
# strip.white je zde NEZBYTN�!!!
adult <- read.csv("data-input/adult.csv", strip.white = TRUE)

dim(adult)
summary(adult)

# obsahuji data chybejici hodnoty?
# funkce is.na() pres vsechny sloupce
sapply(adult, function(x) sum(is.na(x)))

# atribut workclass
table(adult$workclass)
barplot(table(adult$workclass))

# dominantn� u atributu hours-per-week
sort(table(adult$hours.per.week))

# Do kolika tr�d m�me data klasifikovat?
table(adult$class)

# V�ber podskupiny dat
adult.usa <- adult[adult$native.country == "United-States", ]

# Diskretizace dat

# tri hodnoty: less: (0-39h), standard: (40h), more: (>40h)
# cut -> the ranges are open on the left, and closed on the right
adult.usa$hours.per.week.nominal <- cut(adult.usa$hours.per.week,
  breaks = c(0, 39, 40, Inf),
  labels = c("less", "standard", "more"),
  include.lowest = T,
  ordered_result = T
)
adult.usa$hours.per.week.nominal

# diskretizace fnlwgt
# install.packages("arules")
hist(adult.usa$fnlwgt)
library(arules)
adult.usa$fnlwgt.disk <- discretize(adult.usa$fnlwgt, method = "frequency", breaks = 10)
barplot(table(adult.usa$fnlwgt.disk))


# Chybej�c� hodnoty
adult.usa$workclass[adult.usa$workclass == "?"] <- NA
summary(adult.usa)
# Nahradte chybej�c� hodnoty u atributu "workclass" na nejcastejs� hodnotu
adult.usa$workclass[is.na(adult.usa$workclass)] <- names(which.max(table(adult.usa$workclass)))

# Odvozen� atributy (forma extrakce pr�znaku)
# capital = capital_gain-capital_lost
adult.usa$capital <- adult.usa$capital.gain - adult.usa$capital.lost

# Vzorkov�n�
table(adult.usa$class)
index1 <- which(adult.usa$class == "<=50K")
index2 <- which(adult.usa$class == ">50K")
bal1 <- sample(index1, size = 5000)
bal2 <- sample(index2, size = 5000)

adult.usa.bal <- rbind(adult.usa[bal1, ], adult.usa[bal2, ])
table(adult.usa.bal$class)


######
# TRANSFORMACE
tr <- read.csv("data-input/transformace.csv")
plot(tr$x, tr$y)
plot(log(tr$x), log(tr$y))
