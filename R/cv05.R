# 1. BAYESOVSKE ROZHODOVANI
# nacteni a prohlidka dat
d <- read.csv('data-input/weather.csv')
summary(d)

# naivni Bayes
library(e1071)
# je treba zkonvertovat "logical" priznak na "numeric" 
#d$windy <- as.numeric(d$windy)
# vyytvoreni modelu = vypocet pravdepodobosti
d$play <- factor(d$play)
summary(d)
m <- naiveBayes(play~., d)

# vyzkousejme klasifikator
# predikce ucicich dat "d" pomoci modelu "m"
predict(m,d)
# ctyrpolni tabulka - klasifika podle modelu vs skutecna hodnota parametru "play"
table(predict(m,d),d$play)

# a klasifikujme nove pozorovani:
d.new<-data.frame(outlook='sunny',temperature='hot',humidity='normal',windy=FALSE)
predict(m,d.new)

# 2. k-NN
library(class)

# nacten� iris dat
data(iris)
# rychl� explorace dat
dim(iris)
summary(iris)

# zam�ch�n� dat pred rozdelen�m do mnozin
set.seed(13579)
shuffle <- sample(nrow(iris), nrow(iris), replace=F)
iris <- iris[shuffle,]

# vytvoren� tr�novac� (50%), validacn� (30%) and testovac� (20%) mnoziny
trainIris <- iris[1:(0.5*nrow(iris)),]
validIris <- iris[(0.5*nrow(iris) + 1):(0.8*nrow(iris)),]
testIris <- iris[(0.8*nrow(iris) + 1):(nrow(iris)),]

# overen� velikosti mnozin
dim(trainIris)
dim(validIris)
dim(testIris)

# pr�prava hodnot parametru k
parameter <- seq(from = 1, to = 30, by = 2);

# pr�prava vektoru pro zaznamen�v�n� hodnot klasifikacn� �spesnosti
acc <- vector(length = length(parameter))

for (i in 1:length(parameter)) {
  prediction <- knn(trainIris[,-5],validIris[,-5],cl = trainIris[,5],k = parameter[i])
  acc[i] <- sum(prediction == validIris[,5])/nrow(validIris)
}

# nalezen� nejleps� hodnoty k podle validacn� mnoziny
kValidated <- parameter[which.max(acc)]
plot(parameter,acc)

# odhad �spesnosti klasifikace na testovac� mnozine
trainFull <- rbind(trainIris, validIris)
prediction <- knn(trainFull[,-5],testIris[,-5],cl = trainFull[,5],k = kValidated)
testAcc <- sum(prediction == testIris[,5])/nrow(testIris)
testAcc
