#####################################
# A) ANAL�ZA HLAVN�CH KOMPONENT (PCA)
#####################################

df.iris <- iris
df.iris

#################
# 1. PCA manu�lne
#################

# �prava dat
# odecten� stredu dat a normalizace
# stejn�ho v�sledku bychom dos�hli i za pouzit� funkce 'scale'
n_rows <- nrow(df.iris[, 1:4])
iris.mean <- sapply(df.iris[, 1:4], mean)
iris.mean
iris.sd <- sapply(df.iris[, 1:4], sd)
iris.sd
df.iris_c <- (df.iris[, 1:4] - matrix(rep(iris.mean, each = n_rows), nrow = n_rows)) / matrix(rep(iris.sd, each = n_rows), nrow = n_rows)

# kovariancn� matice
S <- cov(df.iris_c)
S

# rozklad S na vlastn� c�sla a vektory
ev <- eigen(S)
ev
# ev$values - vlastn� c�sla = rozptyly dat v nov�ch souradnic�ch
# ev$vectors - vlastn� vektory = sloupce odpov�daj� koeficientum line�rn� kombinace mapuj�c�
#             puvodn� souradnice do nov�ch komponent

# pod�v�me se na data v nov�ch souradnic�ch
# x'= iris * eigenvector
# %*% je oper�tor maticov�ho soucinu
x.pca <- data.matrix(df.iris_c) %*% ev$vectors
head(x.pca, 5)

######################
# 2. PCA pomoc� prcomp
######################

iris.pca <- prcomp(df.iris[, c(1:4)], center = TRUE, scale. = TRUE)
iris.pca
summary(iris.pca)

# zde si muzeme overit, jak� stredy a smerodatn� odchylky byly pri sk�lov�n� pouzity
# vhodn� pro prid�v�n� dals�ch dat
iris.pca$center
iris.pca$scale

# rozptyl = smerodatn� odchylka^2
lambda <- iris.pca$sdev^2
lambda

# iris.pca$x - hodnoty transformovan� do nov�ch souradnic
head(iris.pca$x, 5)

# iris.pca$rotation - transformacn� matice
iris.pca$rotation

# norm(iris.pca$rotation[,1] , type="2")
pc1 <- iris.pca$rotation[, 1]



############################
# 3. graf rozptylu komponent
############################
# install.packages("factoextra")
library(factoextra)
fviz_eig(iris.pca)

#############################################
# 4. scatter plot v prvn�ch dvou komponent�ch
#############################################
plot(iris.pca$x[, 1], iris.pca$x[, 2],
  col = df.iris$Species,
  xlab = "PC1", ylab = "PC2"
)


#######################
# 5. korelace souradnic
#######################
fviz_pca_var(iris.pca,
  repel = TRUE
)

######################
######################

#########################################
# B) LINE�RN� DISKRIMINACN� ANAL�ZA (LDA)
#########################################
# Breast Cancer Wisconsin data set
# dataset obsahuje dekriptory (popis) bunecn�ch jader z biopsie prsu. Data vznikla na z�klade popisu obrazov�ch dat.
# 32 promenn�ch
# ID, diagnoza, 30 dals�ch pr�znaku:
# "The mean, standard error, and "worst" or largest (mean of the three largest values) of these features were computed for each image,
# resulting in 30 features. For instance, field 3 is Mean Radius, field 13 is Radius SE, field 23 is Worst Radius."
wdbc <- read.csv("data-input/wdbc.csv", header = F)
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(wdbc) <- c("id", "diagnosis", paste0(features, "_mean"), paste0(features, "_se"), paste0(features, "_worst"))

##############################
# 1. projekce do "radius_mean"
##############################

library(sm)
library(ggplot2)
library(scales)

sm.density.compare(wdbc$radius_mean, as.factor(wdbc$diagnosis), xlab = "mean radius", col = hue_pal()(2), lwd = 3)
legend(25, 0.18,
  legend = c("B", "M"),
  col = hue_pal()(2), lty = 1:2, cex = 0.8
)

m.B <- mean(wdbc$radius_mean[wdbc$diagnosis == "B"])
m.M <- mean(wdbc$radius_mean[wdbc$diagnosis == "M"])
sd.B <- var(wdbc$radius_mean[wdbc$diagnosis == "B"])
sd.M <- var(wdbc$radius_mean[wdbc$diagnosis == "M"])
# fisheruv pomer
k1 <- (m.B - m.M)^2 / (sd.B + sd.M)
k1


##############################
# 2. projekce do PC1
##############################
wdbc.pca <- prcomp(wdbc[, 3:32], center = TRUE, scale. = TRUE)
fviz_pca_ind(wdbc.pca,
  geom = "point",
  habillage = wdbc$diagnosis,
  repel = TRUE
)
# odhad hustoty
sm.density.compare(wdbc.pca$x[, 1], wdbc$diagnosis, xlab = "PC1", col = hue_pal()(2), lwd = 3)
legend(7, 0.24,
  legend = c("B", "M"),
  col = hue_pal()(2), lty = 1:2, cex = 0.8
)
m2.B <- mean(wdbc.pca$x[wdbc$diagnosis == "B", 1])
m2.M <- mean(wdbc.pca$x[wdbc$diagnosis == "M", 1])
sd2.B <- var(wdbc.pca$x[wdbc$diagnosis == "B", 1])
sd2.M <- var(wdbc.pca$x[wdbc$diagnosis == "M", 1])
k2 <- (m2.B - m2.M)^2 / (sd2.B + sd2.M)
k2

########
# 3. LDA
########

wdbc.data <- as.matrix(wdbc[, c(3:32)])
row.names(wdbc.data) <- wdbc$id
wdbc_raw <- cbind(wdbc.data, as.numeric(as.factor(wdbc$diagnosis)) - 1)
colnames(wdbc_raw)[31] <- "diagnosis"

# rozdelen� dat na tr�novac� a testovac� mnozinu
smp_size_raw <- floor(0.75 * nrow(wdbc_raw))
train_ind_raw <- sample(nrow(wdbc_raw), size = smp_size_raw)
train_raw.df <- as.data.frame(wdbc_raw[train_ind_raw, ])
test_raw.df <- as.data.frame(wdbc_raw[-train_ind_raw, ])

library(MASS)

# natr�nov�n� modelu
wdbc_raw.lda <- lda(diagnosis ~ ., data = train_raw.df)
wdbc_raw.lda

# transformace tr�novac�ch dat do nov� souradnice
p <- predict(wdbc_raw.lda, train_raw.df)
proj.lda <- p$x
proj.lda
sm.density.compare(as.vector(proj.lda), train_raw.df$diagnosis, xlab = "LD1", col = hue_pal()(2), lwd = 3)
legend(6, 0.3,
  legend = c("B", "M"),
  col = hue_pal()(2), lty = 1:2, cex = 0.8
)

m3.B <- mean(proj.lda[train_raw.df$diagnosis == "0"])
m3.M <- mean(proj.lda[train_raw.df$diagnosis == "1"])
sd3.B <- var(proj.lda[train_raw.df$diagnosis == "0"])
sd3.M <- var(proj.lda[train_raw.df$diagnosis == "1"])
k3 <- (m3.B - m3.M)^2 / (sd3.B + sd3.M)
k3

pred <- predict(wdbc_raw.lda, test_raw.df)
pred
table(test_raw.df$diagnosis, pred$class)

######################
# C) FAKTOROV� ANAL�ZA
######################
od.data <- read.table("http://static.lib.virginia.edu/statlab/materials/data/decathlon.dat")
od.data <- as.matrix(od.data)
rownames(od.data) <- colnames(od.data) <- c(
  "100m", "LJ", "SP", "HJ",
  "400m", "100mH", "DS", "PV",
  "JV", "1500m"
)
fa1 <- factanal(covmat = od.data, factors = 3, n.obs = 280, rotation = "varimax")
fa1
