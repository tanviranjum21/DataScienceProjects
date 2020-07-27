library(tidyverse)
library(ggplot2)
library(caTools)
library(gains)

heartdata <- read.csv("heart.csv")
#str(heartdata)

#splitting Data 

set.seed(123)
split = sample.split(heartdata,SplitRatio = 0.7)
train = subset(heartdata, split == "TRUE")
test = subset(heartdata, split == "FALSE")

#creating model 
mod = glm(train$target~.,data = train,family = "binomial")
summary(mod)

#to find out optimized values 
step(mod,direction = "both")

#collecting the optimal value 
mod1 = glm(formula = train$target ~ sex + cp + trestbps + chol + restecg + 
             exang + oldpeak + slope + ca + thal, family = "binomial", 
           data = train)
summary(mod1)

#prediction 
r = predict(mod1,test,type = "response")
r
table(heartdata$target)/nrow(heartdata)

#confusion matrix 
tab = (table(actual_value = test$target , predicted_value = r > 0.3 ))
tab
sum(diag(tab)/sum(tab))

#Gains chart 
gains(test$target, predict(mod1, type = "response",newdata = test), groups = 10)
