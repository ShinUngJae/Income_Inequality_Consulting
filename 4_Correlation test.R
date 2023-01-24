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


################################################################# 1. parametric

# (1) official1-unofficial1

temp1 <- log(north$official1 + 1)
temp2 <- log(north$unofficial1 + 1)

plot(north$official1 ~ north$unofficial1, 
     xlab = 'official1', ylab = 'unofficial1', cex = 3, col = 'red', cex.lab = 1.5)
plot(temp1 ~ temp2, xlab = 'official1', ylab = 'unofficial1'
     , cex = 3, col = 'red', cex.lab = 1.5)

cor(temp1, temp2, method = 'pearson')
cor.test(temp1, temp2)




# (2) official2-unofficial2

temp1 <- log(north$official2 + 1)
temp2 <- log(north$unofficial2 + 1)

plot(north$official2 ~ north$unofficial2, 
     xlab = 'official2', ylab = 'unofficial2', cex = 3, col = 'red', cex.lab = 1.5)
plot(temp1 ~ temp2, xlab = 'official2', ylab = 'unofficial2'
     , cex = 3, col = 'red', cex.lab = 1.5)

cor(temp1, temp2, method = 'pearson')
cor.test(temp1, temp2)



# (3) official-unofficial -> non zero

tempindex <- which(north$official1 == 0 | north$unofficial1 == 0)
temp <- north[-tempindex, ]
nrow(temp)

temp1 <- log(temp$official1 + 1)
temp2 <- log(temp$unofficial1 + 1)

plot(temp$official1 ~ temp$unofficial1, 
     xlab = 'official1', ylab = 'unofficial1', cex = 3, col = 'red')
plot(temp1 ~ temp2, xlab = 'official', ylab = 'unofficial'
     , cex = 3, col = 'red')

cor(temp1, temp2, method = 'pearson')
cor.test(temp1, temp2)


# (4) official2-unofficial2 -> non zero

tempindex <- which(north$official2 == 0 | north$unofficial2 == 0)
temp <- north[-tempindex, ]
nrow(temp)

temp1 <- log(temp$official2 + 1)
temp2 <- log(temp$unofficial2 + 1)

plot(temp$official2 ~ temp$unofficial2, 
     xlab = 'official2', ylab = 'unofficial2', cex = 3, col = 'red')
plot(temp1 ~ temp2, xlab = 'official2', ylab = 'unofficial2'
     , cex = 3, col = 'red')

cor(temp1, temp2, method = 'pearson')
cor.test(temp1, temp2)


################################################################# 2. non-parametric

# (1) official1-unofficial1 kendall

cor(north$official1, north$unofficial1, method = 'kendall')
cor.test(north$official1, north$unofficial1, method = 'kendall')

# (2) official1-unofficial1 spearman


cor(north$official1, north$unofficial1, method = 'spearman')
cor.test(north$official1, north$unofficial1, method = 'spearman')



# (3) official2-unofficial2 kendall

temp1 <- log(north$official2 + 1)
temp2 <- log(north$unofficial2 + 1)


cor(north$official2, north$unofficial2, method = 'kendall')
cor.test(north$official2, north$unofficial2, method = 'kendall')

# (4) official2-unofficial2 spearman


cor(north$official2, north$unofficial2, method = 'spearman')
cor.test(north$official2, north$unofficial2, method = 'spearman')


###################################################
tempindex <- which(north$official1 == 0 | north$unofficial1 == 0)
temp <- north[-tempindex, ]
nrow(temp)

cor(temp$official1, temp$unofficial1, method = 'kendall')
cor.test(temp$official1, temp$unofficial1, method = 'kendall')

cor(temp$official1, temp$unofficial1, method = 'spearman')
cor.test(temp$official1, temp$unofficial1, method = 'spearman')

cor(temp$official2, temp$unofficial2, method = 'kendall')
cor.test(temp$official2, temp$unofficial2, method = 'kendall')

cor(temp$official2, temp$unofficial2, method = 'spearman')
cor.test(temp$official2, temp$unofficial2, method = 'spearman')




