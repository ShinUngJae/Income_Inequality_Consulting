rm(list = ls())

getwd()
setwd(' install.packages('ggplot2')
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
# install.packages("lawstat")
library(lawstat)
# install.packages("agricolae")
library(agricolae)



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


########################################### 1. unofficial1

group_1 <- north$unofficial1[north$region == 1]
group_2 <- north$unofficial1[north$region == 2]
group_3 <- north$unofficial1[north$region == 3]

group1 <- log(group_1 + 1)
group2 <- log(group_2 + 1)
group3 <- log(group_3 + 1)


shapiro.test(group1)
qqnorm(group1, xlab = 'group1', cex.lab = 1.5)
qqline(group1)
lillie.test(group1)

shapiro.test(group2)
qqnorm(group2, xlab = 'group2', cex.lab = 1.5)
qqline(group2)
lillie.test(group2)

shapiro.test(group3)
qqnorm(group3, xlab = 'group3', cex.lab = 1.5)
qqline(group3)
lillie.test(group3)

data1 <- c(group1, group2, group3)
group <- c(rep(1, length(group1)), rep(2, length(group2)), rep(3, length(group3)))

levene.test(data1, group)

group <-as.factor(group)

anova1 <- aov(data1 ~ group)
summary(anova1)

scheffe.test(anova1, 'group', group = TRUE, console = TRUE)



########################################### 2. unofficial2

group_1 <- north$unofficial2[north$region == 1]
group_2 <- north$unofficial2[north$region == 2]
group_3 <- north$unofficial2[north$region == 3]

group1 <- log(group_1 + 1)
group2 <- log(group_2 + 1)
group3 <- log(group_3 + 1)


shapiro.test(group1)
qqnorm(group1, xlab = 'group1', cex.lab = 1.5)
qqline(group1)
lillie.test(group1)

shapiro.test(group2)
qqnorm(group2, xlab = 'group2', cex.lab = 1.5)
qqline(group2)
lillie.test(group2)

shapiro.test(group3)
qqnorm(group3, xlab = 'group3', cex.lab = 1.5)
qqline(group3)
lillie.test(group3)

data2 <- c(group1, group2, group3)
group <- c(rep(1, length(group1)), rep(2, length(group2)), rep(3, length(group3)))

levene.test(data2, group)

group <-as.factor(group)

anova2 <- aov(data2 ~ group)
summary(anova2)

scheffe.test(anova2, 'group', group = TRUE, console = TRUE)


########################################### 3. official

group_1 <- north$official1[north$region == 1]
group_2 <- north$official1[north$region == 2]
group_3 <- north$official1[north$region == 3]

group1 <- log(group_1 + 1)
group2 <- log(group_2 + 1)
group3 <- log(group_3 + 1)


shapiro.test(group1)
qqnorm(group1, xlab = 'group1', cex.lab = 1.5)
qqline(group1)
lillie.test(group1)

shapiro.test(group2)
qqnorm(group2, xlab = 'group2', cex.lab = 1.5)
qqline(group2)
lillie.test(group2)

shapiro.test(group3)
qqnorm(group3, xlab = 'group3', cex.lab = 1.5)
qqline(group3)
lillie.test(group3)

data3 <- c(group1, group2, group3)
group <- c(rep(1, length(group1)), rep(2, length(group2)), rep(3, length(group3)))

levene.test(data3, group)

group <- as.factor(group)

anova3 <- aov(data3 ~ group)
summary(anova3)

scheffe.test(anova3, 'group', group = TRUE, console = TRUE)


########################################### 4. official2

group_1 <- north$official2[north$region == 1]
group_2 <- north$official2[north$region == 2]
group_3 <- north$official2[north$region == 3]

group1 <- log(group_1 + 1)
group2 <- log(group_2 + 1)
group3 <- log(group_3 + 1)


shapiro.test(group1)
qqnorm(group1, xlab = 'group1', cex.lab = 1.5)
qqline(group1)
lillie.test(group1)

shapiro.test(group2)
qqnorm(group2, xlab = 'group2', cex.lab = 1.5)
qqline(group2)
lillie.test(group2)

shapiro.test(group3)
qqnorm(group3, xlab = 'group3', cex.lab = 1.5)
qqline(group3)
lillie.test(group3)

data4 <- c(group1, group2, group3)
group <- c(rep(1, length(group1)), rep(2, length(group2)), rep(3, length(group3)))

levene.test(data4, group)

group <- as.factor(group)

anova4 <- aov(data4 ~ group)
summary(anova4)

scheffe.test(anova4, 'group', group = TRUE, console = TRUE)




