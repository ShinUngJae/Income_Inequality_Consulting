rm(list = ls())

getwd()
setwd(''install.packages('ggplot2')
library(ggplot2)
# install.packages('nortest')
library(nortest)
# install.packages("forecast")
library(forecast)
library(MASS)

# install.packages('pgirmess')
library(pgirmess)
# install.packages('clinfun')
library(clinfun)



data <- read.csv('north3.csv')


rdata <- data
str(data)
str(rdata)
rdata <- rdata[, -1]
str(rdata)


rdata$gender <- factor(rdata$gender, levels = c(1, 2))
str(rdata)
rdata$age <- factor(rdata$age, levels = c(20, 30, 40, 50, 60))
str(rdata)
rdata$urban <- factor(rdata$urban, levels = c(1, 2))
str(rdata)
rdata$job1 <- factor(rdata$job1, levels = c(1 : 3))
str(rdata)
rdata$job2 <- factor(rdata$job2, levels = c(1 : 3))
str(rdata)
rdata$education <- factor(rdata$education, levels = c(1 : 2))
str(rdata)
rdata$region <- factor(rdata$region, levels = c(1 : 3))
str(rdata)

north <- rdata
str(north)

# index : index -> 1 : 243
# gender : factor -> 1 : man, 2 : woman
# age : factor -> <20(10, 20), 30, 40, 50, 60
# region : factor -> 1(1) -> capital, 2(2, 4, 5, 7, 8) -> inner, 3(3, 6, 9, 10) -> outer
# job1 : factor -> 1 -> low(4, 6, 7, 11), 2(3, 5, 8, 9) -> medium, 3(1, 2, 10) -> high
# job2 : factor -> 1 -> low(4, 7, 11), 2(3, 5, 6, 8, 9) -> medium, 3(1, 2, 10) -> high
# education : factor -> 1(1, 2) -> non-university, 2(3, 4) -> university
# urban : factor -> 1 -> city, 2 -> local
# family : number -> 1 : 7
# unofficial1 : number
# unofficial2 : number -> unofficial / sqrt(family)
# official1 : number
# official2 : number -> official / sqrt(family)
str(north)
############################################################################################################
############################################################################################################
############################################################################################################


################################################################# 1. chisq and exact

# (1) zero_or_not_official-age

n <- nrow(north)
zero_or_not_official <- rep(0, n)
for (i in 1 : n)
{
  if (north$official1[i] > 0) {
    zero_or_not_official[i] <- 1 
  } else { 
    zero_or_not_official[i] <- 0
  }
}
temp1 <- table(north$age, zero_or_not_official)
temp1
sum(north$official1 == 0)

chisq.test(north$age, zero_or_not_official)

a1 <- apply(temp1, 1, sum)
names(a1) <- NULL
a2 <- apply(temp1, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

# (2) zero_or_not_official-region

temp2 <- table(north$region, zero_or_not_official)
temp2

chisq.test(north$region, zero_or_not_official)

a1 <- apply(temp2, 1, sum)
names(a1) <- NULL
a2 <- apply(temp2, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

# (3) zero_or_not_official-job1

temp3 <- table(north$job1, zero_or_not_official)
temp3
chisq.test(north$job1, zero_or_not_official)

a1 <- apply(temp3, 1, sum)
names(a1) <- NULL
a2 <- apply(temp3, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n


# (4) zero_or_not_official-education

temp4 <- table(north$education, zero_or_not_official)
temp4
chisq.test(north$education, zero_or_not_official)

a1 <- apply(temp4, 1, sum)
names(a1) <- NULL
a2 <- apply(temp4, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

# (5) zero_or_not_official-urban

temp5 <- table(north$urban, zero_or_not_official)
temp5
chisq.test(north$urban, zero_or_not_official)

a1 <- apply(temp5, 1, sum)
names(a1) <- NULL
a2 <- apply(temp5, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

#################################################################################

# (1) zero_or_not_unofficial-age

n <- nrow(north)
zero_or_not_unofficial <- rep(0, n)
for (i in 1 : n)
{
  if (north$unofficial1[i] > 0) {
    zero_or_not_unofficial[i] <- 1 
  } else { 
    zero_or_not_unofficial[i] <- 0
  }
}
temp1 <- table(north$age, zero_or_not_unofficial)
temp1
sum(north$unofficial == 0)

chisq.test(north$age, zero_or_not_unofficial)

a1 <- apply(temp1, 1, sum)
names(a1) <- NULL
a2 <- apply(temp1, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

fisher.test(temp1, alternative = 'two.sided')

# (2) zero_or_not_unofficial-region

temp2 <- table(north$region, zero_or_not_unofficial)
temp2
sum(north$unofficial == 0)

chisq.test(north$region, zero_or_not_unofficial)

a1 <- apply(temp2, 1, sum)
names(a1) <- NULL
a2 <- apply(temp2, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

fisher.test(temp2, alternative = 'two.sided')


# (8) zero_or_not_unofficial-job2

temp3 <- table(north$job2, zero_or_not_unofficial)
temp3

chisq.test(north$job2, zero_or_not_unofficial)

a1 <- apply(temp3, 1, sum)
names(a1) <- NULL
a2 <- apply(temp3, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

fisher.test(temp3, alternative = 'two.sided')

# (9) zero_or_not_unofficial-education

temp4 <- table(north$education, zero_or_not_unofficial)
temp4

chisq.test(north$education, zero_or_not_unofficial)

a1 <- apply(temp4, 1, sum)
names(a1) <- NULL
a2 <- apply(temp4, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

fisher.test(temp4, alternative = 'two.sided')



# (10) zero_or_not_unofficial-urban

temp5 <- table(north$urban, zero_or_not_unofficial)
temp5

chisq.test(north$urban, zero_or_not_unofficial)

a1 <- apply(temp5, 1, sum)
names(a1) <- NULL
a2 <- apply(temp5, 2, sum)
names(a2) <- NULL
outer(a1, a2) / n

fisher.test(temp5, alternative = 'two.sided')






