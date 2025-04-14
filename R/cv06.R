library(C50)
library(rpart)
library(rpart.plot)

# 1. IRIS

data(iris)

# data rozdel�me v pomeru 70%:30% na tr�novac� mnozinu 
ind <- sample(nrow(iris),0.7*nrow(iris))
iris_train <- iris[ind,]
iris_test <- iris[-ind,]

tree_rpart <- rpart(Species ~ .,data=iris_train,control=rpart.control(minsplit=10,cp=0),parms=list(split='information'))
summary(tree_rpart)
prp(tree_rpart, type=2, extra=101, nn=TRUE, fallen.leaves=TRUE,
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)

# overen� na testovac�ch datech
table(predict(tree_rpart,newdata=iris_test,type="class"),iris_test$Species)

#konstrukce stromu pomoc� algoritmu C5.0
tree_c50 <- C5.0(Species ~ ., data=iris_train)
summary(tree_c50)
table(predict(tree_c50,newdata=iris_test,type="class"), iris_test$Species)

# 2. TENIS
weather<-read.csv('weather.csv')
summary(weather)
str(weather)
# windy pretypijeme na factor
weather$windy <- as.factor(weather$windy)
weather_tree <- C5.0(play ~ ., data=weather)
summary(weather_tree)
# v kmeni se nach�z� atribut OUTLOOK
weather_test <- data.frame(outlook='sunny',temperature='cool',humidity='normal',windy='TRUE')
str(weather_test)
predict(weather_tree, weather_test)

# 3. MENSI-VETSI
mv<-read.csv('mensi_vetsi.csv')

plot(mv$a, mv$b, col=mv$class)
# nejlepsi rozhodovaci hranice by byla: b=a. Jak si s t�m vsak porad� rozhodovac� strom.

#class mus� b�t typu factor
mv$class<-factor(mv$class)
#konstrukce modelu
model <- rpart(class ~ a + b, data = mv, control = rpart.control(maxdepth=5))
#vizualizace stromu
prp(model, type=2, extra=101, nn=TRUE, fallen.leaves=TRUE,
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)
#vizualizace predikce stromem
plot(mv$a, mv$b, col=predict(model,type="class"))