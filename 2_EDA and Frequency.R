rm(list = ls())

getwd()
setwd(''install.packages('ggplot2')
library(ggplot2)

data <- read.csv('north2.csv')


rdata <- data
str(data)
str(rdata)
rdata <- rdata[, -1]
str(rdata)

rdata$gender2 <- factor(rdata$gender1, levels = c(1, 2))
str(rdata)
rdata$age2 <- factor(rdata$age2, levels = c(10, 20, 30, 40, 50, 60))
str(rdata)
rdata$age3 <- factor(rdata$age3, levels = c(20, 30, 40, 50, 60))
str(rdata)
rdata$urban <- factor(rdata$urban, levels = c(1, 2))
str(rdata)
rdata$job2 <- factor(rdata$job2, levels = c(1 : 11))
str(rdata)
rdata$job3 <- factor(rdata$job3, levels = c(1 : 3))
str(rdata)
rdata$job4 <- factor(rdata$job4, levels = c(1 : 3))
str(rdata)
rdata$education2 <- factor(rdata$education2, levels = c(1 : 4))
str(rdata)
rdata$education3 <- factor(rdata$education3, levels = c(1 : 2))
str(rdata)
rdata$region2 <- factor(rdata$region2, levels = c(1 : 10))
str(rdata)
rdata$region3 <- factor(rdata$region3, levels = c(1 : 3))
str(rdata)

north <- rdata
str(north)
# index : index -> 1 : 243
# gender : factor -> 1 : man, 2 : woman
# age : factor -> <20(10, 20), 30, 40, 50, 60
# region : factor -> 1(1) -> capital, 2(2, 4, 5, 7, 8) -> inner, 3(3, 6, 9, 10) -> outer
# job1 : factor -> 1 -> low(4, 7, 11), 2(3, 5, 6, 8, 9) -> medium, 3(1, 2, 10) -> high
# job2 : factor -> 1 -> low(4, 6, 7, 11), 2(3, 5, 8, 9) -> medium, 3(1, 2, 10) -> high
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

################################################################# 1. age

table(north$age2)
ggplot(north, aes(x = age3, fill = age3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = age3, fill = region3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))
  
################################################################# 2. region
str(north)

table(north$region3)
ggplot(north, aes(x = region3, fill = region3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))

table(north$region3) / length(north$region3)


################################################################# 3. job
str(north)

table(north$job2)
ggplot(north, aes(x = job2, fill = job2)) + geom_bar() +
  theme(axis.title = element_text(size = 30))
# 6 -> the lady of the house
# 9 -> labor
table(north$gender2)
# man : 73, woman : 170

ggplot(north, aes(x = job3, fill = job3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = job3, fill = gender2)) + geom_bar() +
  theme(axis.title = element_text(size = 30))

table(north$job3) / length(north$job3)

ggplot(north, aes(x = job3, fill = region3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))



ggplot(north, aes(x = job4, fill = job4)) + geom_bar() +
  theme(axis.title = element_text(size = 30))
ggplot(north, aes(x = job4, fill = gender2)) + geom_bar() +
  theme(axis.title = element_text(size = 30))
table(north$job4) / length(north$job4)

ggplot(north, aes(x = job4, fill = region3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))
################################################################# 4. education
str(north)

table(north$education2)
ggplot(north, aes(x = education2, fill = education2)) + geom_bar() +
  theme(axis.title = element_text(size = 30))

table(north$education3)
ggplot(north, aes(x = education3, fill = education3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = education3, fill = region3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))


################################################################# 5. urban
str(north)

table(north$urban)
ggplot(north, aes(x = urban, fill = urban)) + geom_bar() +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = urban, fill = region3)) + geom_bar() +
  theme(axis.title = element_text(size = 30))


################################################################# 6. unofficial1
str(north)

log_unofficial1 <- log(north$unofficial1 + 1)

sum(north$unofficial1 == 0)
sum(log_unofficial1 == 0)

ggplot(north, aes(x = region3, y = unofficial1, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = region3, y = log_unofficial1, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))


ggplot(north, aes(x = unofficial1)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))
ggplot(north, aes(x = log_unofficial1)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))

################################################################# 7. unofficial2
log_unofficial2 <- log(north$unofficial2 + 1)
sum(north$unofficial2 == 0)
sum(log_unofficial2 == 0)


ggplot(north, aes(x = region3, y = unofficial2, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = region3, y = log_unofficial2, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))


ggplot(north, aes(x = unofficial2)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))
ggplot(north, aes(x = log_unofficial2)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))



################################################################# 8. official1

log_official1 <- log(north$official1 + 1)

sum(north$official1 == 0)
sum(log_official1 == 0)

ggplot(north, aes(x = region3, y = official1, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = region3, y = log_official1, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))


ggplot(north, aes(x = official1)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))
ggplot(north, aes(x = log_official1)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))


################################################################# 9. official2

log_official2 <- log(north$official2 + 1)

sum(north$official2 == 0)
sum(log_official2 == 0)

ggplot(north, aes(x = region3, y = official2, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))

ggplot(north, aes(x = region3, y = log_official2, colour = region3)) + 
  geom_jitter(size = 5) +
  theme(axis.title = element_text(size = 30))


ggplot(north, aes(x = official2)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))
ggplot(north, aes(x = log_official2)) + geom_histogram(bins = 15) +
  theme(axis.title = element_text(size = 30))


#########################################################################################

str(north)

rdata <- north[, c('index', 'gender2', 'age3', 'region3', 
                    'job3', 'job4', 'education3', 'urban', 'family',
                    'official1', 'official2', 'unofficial1', 'unofficial2')]
str(rdata)

names(rdata) <- c('index', 'gender', 'age', 'region', 
                   'job1', 'job2', 'education', 'urban', 'family',
                   'official1', 'official2', 'unofficial1', 'unofficial2')
str(rdata)

write.csv(rdata, 'north3.csv')





