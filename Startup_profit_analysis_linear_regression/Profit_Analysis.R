#Loading the required packages
library("tidyverse")
library("readr")
library("caTools")


#Loading the dataset 
startup_data <- read.csv("startup_data.csv")
str(startup_data)

#Splitting the data
set.seed(123)
split = sample.split(startup_data,SplitRatio = 0.7)
train = subset(startup_data, split == "TRUE")
test = subset(startup_data, split == "FALSE")

#doing regression 
regressor <- lm(formula = Profit ~.,
                data = train)

par(mfrow <- c(2,2))
plot(regressor)
library(car)
vif(regressor)

summary(regressor)

#First iteration 
#removing state 
regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = train) 
summary(regressor)

#Second iteration 
#removing Administration 
regressor <- lm(formula = Profit ~ `R.D.Spend` + `Marketing.Spend`,
                data = train) 
summary(regressor)

#third iteration 
#removing Administration 
#Final one 
regressor <- lm(formula = Profit ~ `R.D.Spend`,
                data = train) 
summary(regressor)

y_predict <- predict(regressor, newdata = test)

y_predict

test$Profit

plot(y_predict, test$Profit)
