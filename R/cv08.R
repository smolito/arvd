library(e1071)

# nacten� iris dat
data(iris)

# podmnozina dat a jej� vykreslen�
# vyb�r�me parametry Sepal.Width/Length pouze pro tr�dy setosa a virginica
iris.subset = subset(iris, select=c("Sepal.Length", "Sepal.Width", "Species"),
                     Species %in% c("setosa","virginica"))
str(iris.subset)
iris.subset$Species <- droplevels(iris.subset$Species)
str(iris.subset)

plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width,col=iris.subset$Species, pch=19)

# line�rn� hranice - ve 2D to je pr�mka
svm.model = svm(Species ~ ., data=iris.subset,
                kernel='linear',
                cost=1, 
                scale=FALSE)

plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)

w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
# dělím a odečítám protože se snažím w dostat do tvaru y = kx + q
# více vysvětlené ve cvičení
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red", lty=5)

# LADEN� PARAMETRU - ZDE POLY J�DRO S PARAMETRY - COST, GAMMA A COEF0
tuned = tune.svm(Species ~ .,
                 data=iris.subset,
                 kernel="polynomial",
                 degree=2,
                 cost = 10^(1:4),
                 gamma = 10^(-6:-1),
                 coef0 = c(0.1, 1, 10)
                 )
summary(tuned)
tuned$best.parameters$cost
tuned$best.parameters$gamma
tuned$best.parameters$coef0

##################
# NELINE�RN� DATA
##################

# �prava dat na kruh a jeho okol�
kruh = read.csv2("data-input/empty.csv") %>% 
  mutate(a = as.numeric(a),
         b = as.numeric(b))
kruh$class <- 0 #pro jistotu
kruh$class[(kruh$a-0.3)^2+(kruh$b-0.5)^2<0.04] <- 1
kruh$class <- as.factor(kruh$class)
plot(kruh$a, kruh$b, col=kruh$class)

m <- svm(class ~ ., data=kruh, kernel='polynomial', degree=2, cost=0.1)
# kontingencn� tabulka
table(m$fitted, kruh$class, dnn = c("m", "kruh"))
# vykreslen� chybne klasifikovan�ch bodu
kruh.pred <- kruh[m$fitted != kruh$class,]
par(mfrow = c(1,2))
plot(kruh$a, kruh$b, col=kruh$class)
plot(kruh.pred$a, kruh.pred$b, pch=19, xlim=c(0,1), ylim=c(0,1))
