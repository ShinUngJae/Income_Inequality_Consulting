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

###################################################################### 1. KRUCKAL-WALLIS
# 1. unofficial

group_1 <- north$unofficial1[north$region == 1]
group_2 <- north$unofficial1[north$region == 2]
group_3 <- north$unofficial1[north$region == 3]

data1 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(2, length(group_2)), rep(3, length(group_3)))

kruskal.test(data1, group)

kruskalmc(data1, group)



# 2. unofficial2

group_1 <- north$unofficial2[north$region == 1]
group_2 <- north$unofficial2[north$region == 2]
group_3 <- north$unofficial2[north$region == 3]

data2 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(2, length(group_2)), rep(3, length(group_3)))

kruskal.test(data2, group)

kruskalmc(data2, group)


########################################### 3. official

group_1 <- north$official1[north$region == 1]
group_2 <- north$official1[north$region == 2]
group_3 <- north$official1[north$region == 3]

data3 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(2, length(group_2)), rep(3, length(group_3)))

kruskal.test(data3, group)

kruskalmc(data3, group)


########################################### 4. official2

group_1 <- north$official2[north$region == 1]
group_2 <- north$official2[north$region == 2]
group_3 <- north$official2[north$region == 3]

data4 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(2, length(group_2)), rep(3, length(group_3)))

kruskal.test(data4, group)

kruskalmc(data4, group)

###################################################################### 1. KRUCKAL-WALLIS
# 1. unofficial1

group_1 <- north$unofficial1[north$region == 1]
group_2 <- north$unofficial1[north$region == 2]
group_3 <- north$unofficial1[north$region == 3]

data1 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(3, length(group_2)), rep(2, length(group_3)))

jonckheere.test(data1, group, 'decreasing')

# 2. unofficial2

group_1 <- north$unofficial2[north$region == 1]
group_2 <- north$unofficial2[north$region == 2]
group_3 <- north$unofficial2[north$region == 3]

data2 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(3, length(group_2)), rep(2, length(group_3)))

jonckheere.test(data2, group, 'decreasing')


# 3. official1

group_1 <- north$official1[north$region == 1]
group_2 <- north$official1[north$region == 2]
group_3 <- north$official1[north$region == 3]

data3 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(3, length(group_2)), rep(2, length(group_3)))

jonckheere.test(data3, group, 'decreasing')


########################################### 4. official2

group_1 <- north$official2[north$region == 1]
group_2 <- north$official2[north$region == 2]
group_3 <- north$official2[north$region == 3]

data4 <- c(group_1, group_2, group_3)
group <- c(rep(1, length(group_1)), rep(3, length(group_2)), rep(2, length(group_3)))

jonckheere.test(data4, group, 'decreasing')





