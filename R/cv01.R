library(tidyverse)

df = datasets::iris

# konfidencni intervaly ----

# moje reseni
alpha = 0.1
data = rnorm(n = 1000, mean = 0, sd = 1)

prum = mean(data)
margin_of_error = 850/1000
se_data = sd(data)/sqrt(length(data))

z_score = qnorm(1 - alpha / 2)
margin_of_error = z_score * se_data

lower_limit = z_score - margin_of_error
upper_limit = z_score + margin_of_error

conf_interval = c(lower_limit, upper_limit)

# skript z moodlu

n = 1000
n1 = 850
p_hat = n1/n
alpha = 0.1

q1 = qnorm(alpha/2, mean = p_hat, sd = sqrt((p_hat*(1-p_hat))/n))
q2 = qnorm(1-alpha/2, mean = p_hat, sd = sqrt((p_hat*(1-p_hat))/n))
q1
q2

# normovane normalni rozdeleni
q1n = p_hat - sqrt((p_hat*(1-p_hat))/n)*qnorm(1-alpha/2)
q1n
q2n = p_hat + sqrt((p_hat*(1-p_hat))/n)*qnorm(1-alpha/2)
q2n

binom.test(n1, n, p = 0.5,
           alternative = "two.sided",
           conf.level = 1-alpha)

# uloha 3
high_protein <- c(134, 146, 104, 119, 124, 161, 107, 83, 113, 129, 97, 123)
low_protein <- c(70, 118, 101, 85, 107, 132, 94)	
t.test(high_protein, low_protein, alternative = "greater", var.equal = TRUE)

m <- length(high_protein)
n <- length(low_protein)
s2 <- ((m-1)*var(high_protein)+(n-1)*var(low_protein))/(m+n-2)
t <- (mean(high_protein)-mean(low_protein))/(sqrt(s2)*sqrt(1/m+1/n))
t
alpha <- 0.05
kappa <- qt(1-alpha, df = m + n - 2) 
kappa
p_value <- 1 - pt(t, df = m+n-2)
p_value