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
library(car)
library(sampleSelection)
# install.packages('stargazer')
library(stargazer)



data <- read.csv('north3.csv')

rdata <- data
str(data)
str(rdata)
rdata <- rdata[, -1]
str(rdata)

rdata$official1[69 : 73]
rdata$official1[70] <- 0

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
# gender : number -> 1, 2
# gender2 : factor -> 1 : man, 2 : woman
# age : number -> 10, 20, 30, 40, 50, 60
# age2 : factor -> 10, 20, 30, 40, 50, 60
# age3 : factor -> <20(10, 20), 30, 40, 50, 60
# region : number -> 1 : 10
# region2 : factor -> 1 : 10
# region3 : factor -> 1(1) -> capital, 2(2, 4, 5, 7, 8) -> inner, 3(3, 6, 9, 10) -> outer
# job : number -> 1 : 11
# job2 : factor -> 1 : 11
# job3 : factor -> 1 -> low(4, 7, 11), 2(3, 5, 6, 8, 9) -> medium, 3(1, 2, 10) -> high
# education : number -> 1 : 4
# education2 : factor -> 1 : 4
# education3 : factor -> 1(1, 2) -> non-university, 2(3, 4) -> university
# urban : factor -> 1 -> city, 2 -> local
# family : number -> 1 : 7
# unofficial : number
# unofficial2 : number -> unofficial / sqrt(family)
# official : number
# official2 : number -> official / sqrt(family)
str(north)
############################################################################################################
############################################################################################################
############################################################################################################
n <- nrow(north)
north$zero_or_not_unofficial <- rep(0, n)
for (i in 1 : n)
{
  if (north$unofficial1[i] > 0) {
    north$zero_or_not_unofficial[i] <- 1 
  } else { 
    north$zero_or_not_unofficial[i] <- 0
  }
}

north$zero_or_not_official <- rep(0, n)
for (i in 1 : n)
{
  if (north$official1[i] > 0) {
    north$zero_or_not_official[i] <- 1 
  } else { 
    north$zero_or_not_official[i] <- 0
  }
}

str(north)

# gender, age, region, job1, education, urban


############################################################################# (1) north$unofficial

ols1 <- lm(unofficial ~ region3 + education3, data = north)

heck1 <- selection(zero_or_not_unofficial ~ gender,
                   unofficial1 ~ region, 
                   data = north, method = '2step')
summary(heck1)

stargazer(ols1, heck1,    
          title = "North", type = "text", 
          df = FALSE, digits = 0)

shapiro.test(log(north$unofficial1 + 1))

qqnorm(log(north$unofficial1 + 1))
qqline(log(north$unofficial1 + 1))

lillie.test(log(north$unofficial1 + 1))




############################### not log

ols1 <- lm(unofficial ~ gender2 + age3 + region3 
           + job3 + education3 + urban, data = north)
summary(ols1)

step(ols1)

# unofficial ~ age3 + job3 + urban -> not region3

ols1 <- lm(unofficial ~ gender2 + age3 + region3 
           + job3 + education3, data = north)
summary(ols1)

step(ols1)

# age3 + region3 + job3 -> exist region3

ols1 <- lm(unofficial ~ age3 + region3 + job3, data = north)
summary(ols1)



############################### log

ols1 <- lm(log(unofficial1 + 1) ~ gender + age + region 
           + job2 + education + urban, data = north)
summary(ols1)

step(ols1)

# unofficial ~ age3 + region3 + job3 + urban -> i want to exclude urban

ols1 <- lm(log(unofficial1 + 1) ~ age + region + job2 + urban, data = north)
summary(ols1)



# unofficial ~ age3 + region3 + job3

ols1 <- lm(log(unofficial + 1) ~ gender + age + region 
           + job2 + education, data = north)
summary(ols1)

step(ols1)


ols1 <- lm(log(unofficial + 1) ~ age3 + region3 + job3, data = north)
summary(ols1)


############################################################### non zero
nrow(north)

nrow(north[-c(which(north$unofficial1 == 0)), ])
north_non_zero_unofficial <- north[-c(which(north$unofficial1 == 0)), ]

shapiro.test(log(north_non_zero_unofficial$unofficial1))

qqnorm(log(north_non_zero_unofficial$unofficial1))
qqline(log(north_non_zero_unofficial$unofficial1))

lillie.test(log(north_non_zero_unofficial$unofficial1))



############################### not log

ols1 <- lm(unofficial ~ gender2 + age3 + region3 
           + job3 + education3 + urban, data = north_non_zero_unofficial)
summary(ols1)

step(ols1)

# unofficial ~ age3 + job3 + urban -> not region3

ols1 <- lm(unofficial ~ gender2 + age3 + region3 
           + job3 + education3, data = north_non_zero_unofficial)
summary(ols1)

step(ols1)

# age3 + region3 + job3 -> exist region3

ols1 <- lm(unofficial ~ age3 + region3 + job3, data = north_non_zero_unofficial)
summary(ols1)


############################### log

ols1 <- lm(log(unofficial1) ~ gender + age + region
           + job2 + education + urban, data = north_non_zero_unofficial)
summary(ols1)

step(ols1)

# unofficial ~ age3 + region3 + job3 + urban -> i want to exclude urban

ols1 <- lm(log(unofficial1) ~ gender + age + region + job2 + urban, 
           data = north_non_zero_unofficial)
summary(ols1)



# unofficial ~ age3 + region3 + job3

ols1 <- lm(log(unofficial1) ~ gender + age + region 
           + job2 + education, data = north_non_zero_unofficial)
summary(ols1)

step(ols1)


ols1 <- lm(log(unofficial1) ~ age3 + region3 + job3, 
           data = north_non_zero_unofficial)
summary(ols1)


################################################################################(2) north$unofficial2
############################### not log

ols2 <- lm(unofficial2 ~ gender2 + age3 + region3 
           + job3 + education3 + urban, data = north)
summary(ols2)

step(ols2)

# unofficial2 ~ age3 + job3 + urban -> not region3

ols2 <- lm(unofficial2 ~ gender2 + age3 + region3 
           + job3 + education3, data = north)
summary(ols2)

step(ols2)

# age3 + region3 + job3 -> exist region3

ols2 <- lm(unofficial2 ~ age3 + region3 + job3, data = north)
summary(ols2)



############################### log

ols2 <- lm(log(unofficial2 + 1) ~ gender2 + age3 + region3 
           + job3 + education3 + urban, data = north)
summary(ols2)

step(ols2)

# unofficial2 ~ age3 + region3 + job3 + urban -> i want to exclude urban

ols2 <- lm(log(unofficial2 + 1) ~ age3 + region3 + job3 + urban, data = north)
summary(ols2)



# unofficial2 ~ age3 + region3 + job3

ols2 <- lm(log(unofficial2 + 1) ~ gender2 + age3 + region3 
           + job3 + education3, data = north)
summary(ols2)

step(ols2)


ols2 <- lm(log(unofficial2 + 1) ~ age3 + region3 + job3, data = north)
summary(ols2)


############################################################### non zero
nrow(north)

nrow(north[-c(which(north$unofficial2 == 0)), ])
north_non_zero_unofficial2 <- north[-c(which(north$unofficial2 == 0)), ]

shapiro.test(north_non_zero_unofficial2$unofficial2)

qqnorm(north_non_zero_unofficial2$unofficial2)
qqline(north_non_zero_unofficial2$unofficial2)

lillie.test(north_non_zero_unofficial2$unofficial2)



############################### not log

ols2 <- lm(unofficial2 ~ gender2 + age3 + region3 
           + job3 + education3 + urban, data = north_non_zero_unofficial2)
summary(ols2)

step(ols1)

# unofficial2 ~ age3 + job3 + urban -> not region3

ols2 <- lm(unofficial2 ~ gender2 + age3 + region3 
           + job3 + education3, data = north_non_zero_unofficial2)
summary(ols2)

step(ols2)

# age3 + region3 + job3 -> exist region3

ols2 <- lm(unofficial2 ~ age3 + region3 + job3, data = north_non_zero_unofficial2)
summary(ols2)


############################### log

ols2 <- lm(log(unofficial2) ~ gender2 + age3 + region3 
           + job3 + education3 + urban, data = north_non_zero_unofficial2)
summary(ols2)

step(ols2)

# unofficial2 ~ age3 + region3 + job3 + urban -> i want to exclude urban

ols2 <- lm(log(unofficial2) ~ age3 + region3 + job3 + urban, 
           data = north_non_zero_unofficial2)
summary(ols2)



# unofficial2 ~ age3 + region3 + job3

ols2 <- lm(log(unofficial2) ~ gender2 + age3 + region3 
           + job3 + education3, data = north_non_zero_unofficial2)
summary(ols2)

step(ols2)


ols2 <- lm(log(unofficial2) ~ age3 + region3 + job3, 
           data = north_non_zero_unofficial2)
summary(ols2)

################################################################################# (3) north$official

# gender, age, region, job1, education, urban

ols3 <- lm(official1 ~ region + age + gender, data = north)

heck3 <- selection(zero_or_not_official ~ job1 + education,
                    official1 ~ region + gender + age, 
                    data = north, method = '2step')
summary(heck3)

stargazer(ols3, heck3,    
          title = "North", type = "text", 
          df = FALSE, digits = 0)



################################################################################ (4) north$official2

ols4 <- lm(official2 ~ region3 + age3 + gender2, data = north)
heck4 <- selection(zero_or_not_official ~ job3 + education3,
                   official2 ~ region3 + age3 + gender2, 
                   data = north, method = '2step')
summary(heck4)
temp2 <- summary(heck4)

temp2$estimate["invMillsRatio", 4]


stargazer(ols4, heck4,    
          title = "North", type = "text", 
          df = FALSE, digits = 0)





##########################################################################





