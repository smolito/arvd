simple = read.csv("data-input/simple.txt")
simple$class <- as.factor(simple$class)
plot(simple$x1, simple$x2, col = simple$class, asp = 1)
abline(h=5.2, col="purple", lwd=5)

ind = sample(2,nrow(simple),replace=T,prob=c(0.7,0.3))

# vytvoren� tr�novac� (70%) a testovac� (30%) mnoziny
train <- simple[ind == 1,]
test <- simple[ind == 2,]

# Jednoduch� decision stump klasifik�tor
library(rpart)
library(rpart.plot)
stump <- rpart(class ~ .,data = train,control = rpart.control(maxdepth=1,cp=-1))
prp(stump, type=2, extra=101, nn=TRUE, fallen.leaves=TRUE,
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)
stumpError <- sum(predict(stump,newdata=test,type='class') != test$class)/nrow(test)
stumpError
stumpConfusion <- table(predict(stump,newdata=test,type='class'),test$class)
stumpConfusion

library(adabag)
library(rpart)

# Porota slozen� z klasifik�tor� - bagging
#set.seed(123)
baggedStumps <- bagging(class ~ .,data=train,mfinal=20, control=rpart.control(maxdepth=1,cp=-1))
baggedStumps.perf <- predict.bagging(baggedStumps,newdata=test)
baggedStumps.perf
prp(baggedStumps$trees[[1]], type=2, extra=101, nn=TRUE, fallen.leaves=TRUE,
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)
prp(baggedStumps$trees[[2]], type=2, extra=101, nn=TRUE, fallen.leaves=TRUE,
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)
prp(baggedStumps$trees[[15]], type=2, extra=101, nn=TRUE, fallen.leaves=TRUE,
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)

plot(train$x1, train$x2, col = as.factor(baggedStumps$class), asp = 1)


# Porota slozen� z klasifik�tor� - AdaBoost
boostedStumps <- boosting(class ~ .,data=train,mfinal=20,boos=T,control=rpart.control(maxdepth=1,cp=-1))
boostedStumps.perf <- predict.boosting(boostedStumps,newdata=test)
boostedStumps.perf
plot(train$x1, train$x2, col = as.factor(boostedStumps$class), asp = 1)