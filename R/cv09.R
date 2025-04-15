library(neuralnet)
library(PlaneGeometry)

#####################################
# LINEÁRNÍ KLASIFIKÁTOR -  PERCEPTRON
#####################################

d <- read.csv(file='data-input/ctvrtkruh.csv')
summary(d)
with(d, plot(a, b, col=c('red','blue')[class+1]))

n <- neuralnet(class ~ a+b, d, hidden=0)
plot(n)
summary(n)

# predikce 
pred <- as.numeric(unlist(n$net.result) > 0.5)
table(d$class, pred)
round(prop.table(table(d$class, pred)),2)
table(d$class!=pred)

# vizualizace:
with(d, plot(a, b, col=c('red','blue')[class+1]))
weights <- n$weights[[1]][[1]]
weights
abline(a=(0.5-weights[1])/weights[3],
       b=-weights[2]/weights[3],
       col='green',
       lwd=3)
with(d[d$class!=pred,], points(a, b, pch=19))

#################################
# VYROVNÁNÍ PŘÍZNAKOVÉHO PROSTORU
#################################

d$a2 <- d$a^2
d$b2 <- d$b^2
d$ab <- d$a*d$b
n2 <- neuralnet(class ~ a+b+a2+b2+ab, d, hidden=0)
pred2 <- as.numeric(unlist(n2$net.result) > 0.5)
w2 <- unlist(n2$weights)
plot(n2)

with(d, plot(a, b, col=c('red','blue')[class+1]))
with(d[d$class!=pred2,], points(a, b, pch=19))


draw_ellipse_from_equation <- function(A, B, C, D, E, F, xlim = c(-3, 3), ylim = c(-3, 3), n = 500, ...) {
  x <- seq(xlim[1], xlim[2], length.out = n)
  y <- seq(ylim[1], ylim[2], length.out = n)
  z <- outer(x, y, Vectorize(function(x, y) {
    A*x^2 + B*x*y + C*y^2 + D*x + E*y + F
  }))
  contour(x, y, z, levels = 0, drawlabels = FALSE, add = TRUE, ...)
}

draw_ellipse_from_equation(A = w2[4], B = w2[6], C = w2[5],
                           D = w2[2], E = w2[3], F = w2[1] - 0.5,
                           col = "green", lwd = 3)

table(d$class!=pred2)

##########################
# VíCEVRSTVá NEURONOVá SíŤ
##########################

# vice vrstev
d <- read.csv(file='data-input/ctvrtkruh.csv')
n3 <- neuralnet(class ~ a+b, d, hidden=3, lifesign = "minimal") 
plot(n3)
pred3 <- as.numeric(compute(n3,d[,-3])$net.result > 0.5)
with(d, plot(a, b, col=c('red','blue')[class+1]))
with(d[d$class!=pred3,], points(a, b, pch=19))
table(d$class!=pred3)

##################################
# KLASIFIKACE RUCNE PSANÝCH ČÍSLIC
##################################

# V?cevrstv? perceptronov? s?t klasifikace rucn? psan?ch c?slic
# nacteni z CSV
d.train <- read.csv('data-input/digits-train.csv')

# kolik mame dat?
dim(d.train)

# vypsani jmen prvnich 10ti sloupcu
colnames(d.train)[1:10]

# jmeno prvniho sloupce zmenime na 'class' pro snazsi (intuitivnejsi) praci:
colnames(d.train)[1] <- 'class'
# a typ dat ve sloupci zmenime na 'factor' (bude se to hodit pri konstrukci 
# neuronove site, protoze funkce 'nnet' ocekava, ze odezva bude typu 'factor')
d.train$class <- as.factor(d.train$class)

# Funkce k vykresleni jedne cislice.
# parametry:
#  d - data frame
#  idx - index cislice
plotDigit<-function(d,idx) {
  # transformace radku z datove matice na matici 13x13
  m <- matrix(as.numeric(d[idx,-1]), 13, 13)
  # prevraceni matice "vzhuru nohama" a transpozice tak, aby
  # funkce 'image' vykreslila to, co chceme:
  m <- t(m[nrow(m):1,])
  image(m,col=gray.colors(10), xaxt='n', yaxt='n')
}

# budeme kreslit v rastru 2x5 obrazku
opar <- par(mfrow=c(2,5)) # uschovame si soucasne rozlozeni obrazku, tj. 1x1)
# vykreslime jednoho reprezentanta od kazde cislice:
for (idx in 100*(1:10)) plotDigit(d.train,idx)
par(opar) # obnovime puvodni nastaveni obrazku, tj. 1x1

#####
d.train <- cbind(d.train,model.matrix(~d.train$class-1))
names(d.train)[171:180] <- paste0('class',0:9)

form <- as.formula(paste(paste(names(d.train)[171:179],' +',collapse=''),'class9 ~ V2',paste(' +',names(d.train)[3:170],collapse='')))
n <- neuralnet(form, d.train, hidden=20)

# vypsani vah:
summary(n)

plot(n)

# predikce na trenovaci mnozine:
pred <- as.factor(apply(n$net.result[[1]],1,which.max))

# tabulka popisujici uspesnost klasifikace:
tbl.train <- table(d.train$class,pred)
tbl.train

# uspesnost ciselne (podil souctu spravne klasifikovanych cislic (hodnoty na diagonale) vuci vsem cislicim):
sum(diag(tbl.train)) / sum(tbl.train)

# testovaci
d.test <- read.csv('data-input/digits-test.csv')
colnames(d.test)[1] <- 'class'
d.test$class <- as.factor(d.test$class)
d.test <- cbind(d.test,model.matrix(~d.test$class-1))
names(d.test)[171:180] <- paste0('class',0:9)

#pred.test <- as.factor(apply(n$net.result[[1]],1,which.max))
pred2 <- apply(compute(n, d.test[,2:170])$net.result,1, which.max)
tbl.test <- table(d.test$class,pred2)
tbl.test
sum(diag(tbl.test)) / sum(tbl.test)
