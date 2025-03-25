library(tidyverse)
##################
# ZAKLADNI K-MEANS
##################
kruhy = read.csv("data-input/kruhy.csv")
# pocet shluku
k = 3

# KRUHY
plot(kruhy$x, kruhy$y, col = kruhy$class+1, asp = 1)

kc <- kmeans(kruhy[, -3], k)
print(kc)

plot(kruhy$x, kruhy$y, col = kc$cluster, pch =  kruhy$class,
     asp = 1) # ~ aspect (není to šišaté)

points(kc$centers[,1],
       kc$centers[,2],
       pch='H',
       cex=2,col=4)

# TVARY
tvary = read.csv("data-input/tvary.csv")
plot(tvary$x, tvary$y, col = tvary$class+1, asp = 1)

kc2 <- kmeans(tvary[, -3], k)
print(kc2)

plot(tvary$x, tvary$y, col = kc2$cluster, pch = tvary$class, asp = 1)
points(kc2$centers[,1],kc2$centers[,2],pch='X',cex=2,col=4)

##############
# POCET SHLUKU
##############

cluster_num_plot <- function(data,n){
  result <- vector(length=n-1)
  for(i in 2:n){
    cl <- kmeans(data, i)
    result[i-1] <- sum(cl$withinss)
  }
  plot(2:n,result,xlab="po�et shluk�", ylab="pr�m�rn� vzd�lenost")
}

cluster_num_plot(kruhy[, -3], 30)

# shluky.csv
shluky = read.csv("data-input/shluky.csv")
plot(shluky$x, shluky$y, col = shluky$class+1, asp = 1)
cluster_num_plot(shluky[, -3], 30)

###################
# PRAKTICKA ULOHA 1
# SNS DATA
###################

teens <- read.csv("data-input/snsdata.csv") %>% 
  mutate(gender = as.factor(gender))

str(teens)
summary(teens)

# chybejici data - pohlavi
table(teens$gender)
table(teens$gender, useNA = "ifany")

# chybejici data - vek
summary(teens$age)

# zbavime se outlieru - nastavimne je na NA
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)

# zkontrolujeme
summary(teens$age)

# pohlavi - nova promenna female a no_gender
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# check our recoding work
table(teens$gender, useNA = "ifany")
# 1 - female, 0 - male+NA
table(teens$female, useNA = "ifany")
# 1 - NA, 0 - male+female
table(teens$no_gender, useNA = "ifany")

# prumerny vek
mean(teens$age) # to nelze kvuli NA
mean(teens$age, na.rm = TRUE) # lepsi

# prumerny vek agregovany podle gradyear
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
# dela to same
#tapply(teens$age, teens$gradyear, mean, na.rm = T)

# vytvorime vektor s prumernym vekem pro kazdy gradyear, opakovane pro kazdou osobu
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))

# pokud je vek NA, nahradime ho promennou ave_age, jinak nechame puvodni
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# kontrola
summary(teens$age)

## Natrenujeme k-means na sloupcich 5 at 40, zvolime 5 shluku
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

# kolik zaznamu skoncilo v jednotlivych shlucich?
teen_clusters$size

# podivame se na stredu shluku. Lze o shlucich a jejich typickych zastupcich neco rict?
teen_clusters$centers

# dalsi pruzkum
# vlozime cislo clusteru do puvodnich dat
teens$cluster <- teen_clusters$cluster

# podivame se na prvnich 5 zaznamu
teens[1:5, c("cluster", "gender", "age", "friends")]

# prumerny vek pres cluster
aggregate(data = teens, age ~ cluster, mean)

# pomer zen v clusteru
aggregate(data = teens, female ~ cluster, mean)

# prumerny pocet kamaradu
aggregate(data = teens, friends ~ cluster, mean)

#########################
# KONEC PRAKTICKE ULOHY 1
#########################

########################################################################
# Gaussian mixture model estimated by expectation maximization algorithm
########################################################################

# tvary.csv
library(mclust)
cl_gmm <- Mclust(tvary[, -3],G=3)
summary(cl_gmm)
# zvolit moznost 2: classification z nabidky
plot(cl_gmm)
# porovnejte vysledkem k-means

#########################
# Hierarchicke shlukovani
#########################

# zvirata.csv
zvirata = read.csv("data-input/zvirata.csv", header = T, sep = ";")
hc <- hclust(dist(zvirata[,2:14]), method="complete")
plot(hc, hang = -1, labels=zvirata[,1])

help(cutree)
ct <-cutree(hc, 4)
ct
# ale pozor na poradi
hc$order

######################
# alternativni zapis prikazu pomoci pipe
library(dendextend)
library(magrittr)

dend <- zvirata[,2:14] %>%
  dist %>%
  hclust(method = "complete") %>%
  as.dendrogram

dend %>%
  set("labels", zvirata[,1]) %>%
  set("labels_col", value = c("green", "blue"), k=2) %>%
  plot

#######
# pak nasledujici kod:
data <- scale(USArrests)
dist.res <- dist(data)
hc <- hclust(dist.res, method = "ward.D2")
dend <- as.dendrogram(hc)
plot(dend)

# je ekvivalentni k tomuto:
dend <- USArrests %>% # data
  scale %>% # Scale the data
  dist %>% # calculate a distance matrix, 
  hclust(method = "ward.D2") %>% # Hierarchical clustering 
  as.dendrogram # Turn the object into a dendrogram.
plot(dend)

##################

# pocitani vzdalenosti mezi body

library(ggplot2)
x <- c(0.5, 2.5, 4.2, 5.5, 4.8, 7.0, 8.5)
y <- c(2.0, 3.0, 0.7, 0.3, 3.5, 2.5, 2.8)
df <- data.frame(a = x, b = y)
vzd <- dist(cbind(x, y), method = "euclidean", diag = T, upper = F)
vzd
ggplot(df, aes(x=a, y=b)) + geom_point(aes(size = 5))
