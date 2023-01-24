rm(list = ls())

getwd()
setwd('')
# install.packages('openxlsx')
library(openxlsx)
# install.packages('ggplot2')
library(ggplot2)
data <- read.xlsx('rawdata.xlsx')



rdata <- data
str(data)
str(rdata)
rdata <- rdata[, c(1, 2, 3, 6, 7, 8, 9, 11, 15, 16)]
str(rdata)
names(rdata) <- c('index', 'gender', 'age', 'urban', 'region', 
                  'job', 'education', 'family', 'unofficial', 'official')
str(rdata)

sum(is.na(rdata))

rdata$gender2 <- factor(rdata$gender, levels = c(1, 2))
str(rdata)


rdata$age <- rdata$age * 10
rdata$age2 <- factor(rdata$age, levels = c(10, 20, 30, 40, 50, 60))
str(rdata)
i = 1
for (i in 1 : nrow(rdata))
{
  if (rdata$age[i] == 10)
  {
    rdata$age3[i] = 20
  } else {
    rdata$age3[i] <- rdata$age[i]
  }
}
str(rdata)
rdata$age3 <- factor(rdata$age3, levels = c(20, 30, 40, 50, 60))
str(rdata)
str(rdata$age3)


rdata$urban <- factor(rdata$urban, levels = c(1, 2))
str(rdata)



rdata$job2 <- factor(rdata$job, levels = c(1 : 11))
str(rdata)
for (i in 1 : nrow(rdata))
{
  if (rdata$job[i] == 4 | rdata$job[i] == 7 | rdata$job[i] == 11)
  {
    rdata$job3[i] = 1
  } else if (rdata$job[i] == 1 | rdata$job[i] == 2 | rdata$job[i] == 10)
  {
    rdata$job3[i] = 3
  } else {
    rdata$job3[i] = 2
  }
}
str(rdata)
rdata$job3 <- factor(rdata$job3, levels = c(1 : 3))



for (i in 1 : nrow(rdata))
{
  if (rdata$job[i] == 4 | rdata$job[i] == 6 | rdata$job[i] == 7 | rdata$job[i] == 11)
  {
    rdata$job4[i] = 1
  } else if (rdata$job[i] == 1 | rdata$job[i] == 2 | rdata$job[i] == 10)
  {
    rdata$job4[i] = 3
  } else {
    rdata$job4[i] = 2
  }
}
str(rdata)
rdata$job4 <- factor(rdata$job4, levels = c(1 : 3))



str(rdata)



rdata$education2 <- factor(rdata$education, levels = c(1 : 4))
str(rdata)
for (i in 1 : nrow(rdata))
{
  if (rdata$education[i] == 1 | rdata$education[i] == 2)
  {
    rdata$education3[i] <- 1
  } else {
    rdata$education3[i] <- 2
  }
}
str(rdata)
rdata$education3 <- factor(rdata$education3, levels = c(1 : 2))
str(rdata)

rdata$region2 <- factor(rdata$region, levels = c(1 : 10))
str(rdata)
for (i in 1 : nrow(rdata))
{
  if (rdata$region[i] == 1)
  {
    rdata$region3[i] = 1
  } else if (rdata$region[i] == 3 | rdata$region[i] == 6 | rdata$region[i] == 9 | rdata$region[i] == 10)
  {
    rdata$region3[i] = 3
  } else {
    rdata$region3[i] = 2
  }
}
str(rdata)
rdata$region3 <- factor(rdata$region3, levels = c(1 : 3))
str(rdata)


for (i in 1 : nrow(rdata))
{
  rdata$unofficial2[i] <- rdata$unofficial[i] / sqrt(rdata$family[i])
}
str(rdata)
for (i in 1 : nrow(rdata))
{
  rdata$official2[i] <- rdata$official[i] / sqrt(rdata$family[i])
}
str(rdata)


rdata2 <- rdata
str(rdata2)

rdata2 <- rdata2[, c('index', 'gender', 'gender2', 'age', 'age2', 'age3', 
                     'region', 'region2', 'region3', 
                     'job', 'job2', 'job3', 'job4', 
                     'education', 'education2', 'education3', 'urban', 'family',
                     'official', 'official2', 'unofficial', 'unofficial2')]

str(rdata2)

names(rdata2)

names(rdata2) <- c('index', 'gender1', 'gender2', 'age1', 'age2', 'age3', 
                   'region1', 'region2', 'region3', 
                   'job1', 'job2', 'job4', 'job3', 
                   'education1', 'education2', 'education3', 'urban', 'family',
                   'official1', 'official2', 'unofficial1', 'unofficial2')
str(rdata2)

rdata2 <- rdata2[, c('index', 'gender1', 'gender2', 'age1', 'age2', 'age3', 
                     'region1', 'region2', 'region3', 
                     'job1', 'job2', 'job3', 'job4', 
                     'education1', 'education2', 'education3', 'urban', 'family',
                     'official1', 'official2', 'unofficial1', 'unofficial2')]
str(rdata2)

north2 <- rdata2
str(north2)

min <- boxplot(log(north2$unofficial1 + 1))$stats[1, ]
max <- boxplot(log(north2$unofficial1 + 1))$stats[5, ]
outlier_north2 <- north2$index[log(north2$unofficial1 + 1) > max]

rdata3 <- north2[-outlier_north2, ]
nrow(rdata3)
# write.csv(rdata3, 'north3.csv')

# rdata2 <- rdata2[, c('index', 'gender2', 'age3', 'region3', 
#                     'job3', 'job4', 'education3', 'urban', 'family',
#                    'official', 'official2', 'unofficial', 'unofficial2')]
# str(rdata2)

# names(rdata2) <- c('index', 'gender', 'age', 'region', 
#                   'job2', 'job1', 'education', 'urban', 'family',
#                   'official1', 'official2', 'unofficial1', 'unofficial2')
# str(rdata2)

# rdata2 <- rdata2[, c('index', 'gender', 'age', 'region', 
#                     'job1', 'job2', 'education', 'urban', 'family',
#                     'official1', 'official2', 'unofficial1', 'unofficial2')]

str(rdata2)
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


north2 <- rdata2
str(north2)

# write.csv(north2, 'north2.csv')


############################################################################################################
############################################################################################################
############################################################################################################


min <- boxplot(log(north2$unofficial1 + 1))$stats[1, ]
max <- boxplot(log(north2$unofficial1 + 1))$stats[5, ]
outlier_north2 <- north2$index[log(north2$unofficial1 + 1) > max]

rdata3 <- north2[-outlier_north2, ]
nrow(rdata3)
write.csv(rdata3, 'north2.csv')



