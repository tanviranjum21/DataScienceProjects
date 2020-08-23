library(tidymodels)
library(caTools)
library(tidyverse)
library(ggplot2)
library(readr)
library(skimr)
library(broom)
library(yardstick)
library(gridExtra)
library(GoodmanKruskal)
library(vcd)
library(bestglm)
library(pROC)
library(caret)


heart <- read_csv("/Users/tanvir/Desktop/softanbees_files/Logistic\ Regression/framingham.csv")

head(heart)

dim(heart)

missing.oberservation <- sum(is.na(heart))
missing.oberservation

skim(heart)

Cleaned.heart.Data <- na.omit(data.frame(heart[,-c(3,6,14)]))


dim(Cleaned.heart.Data)

skim(Cleaned.heart.Data)


#Data exploration 
# The number of smokers with absolute short-term CHD Risk is
Cleaned.heart.Data %>% 
  filter(currentSmoker == 1 & TenYearCHD == 1) %>% 
  select(currentSmoker, TenYearCHD) %>% 
  summarise(sum_smoker_CHD = sum(currentSmoker))

# The number of smokers without absolute short-term CHD Risk is
Cleaned.heart.Data %>% 
  filter(currentSmoker == 1 & TenYearCHD == 0) %>% 
  select(currentSmoker, TenYearCHD) %>% 
  summarise(sum_smoker_not_CHD = sum(currentSmoker))

# The number of non-smokers with absolute short-term CHD Risk is
Cleaned.heart.Data %>% 
  filter(currentSmoker == 0 & TenYearCHD == 1) %>% 
  select(currentSmoker, TenYearCHD) %>% 
  summarise(sum_non_smoker_CHD = sum(TenYearCHD))

# The number of non-smokers without absolute short-term CHD Risk is
Cleaned.heart.Data %>% 
  filter(currentSmoker == 0 & TenYearCHD == 0) %>% 
  select(currentSmoker) %>% 
  summarise(non_smoker_without_short_term = n())

# Bi-variate plots of the dataset: relation between response variable 
#and predictors  

dataset <- data.frame(Cleaned.heart.Data)
ptlist_bi <- list()

for (var in colnames(dataset)) {
  if(class(dataset[,var]) %in% c("factor", "logical")){
    ptlist_bi[[var]] <- ggplot(data = dataset) +
      geom_bar(aes_string(x = var, fill= "TenYearCHD"), position = "fill")+
      theme_linedraw()+
      xlab(var)
  }
  else if(class(dataset[,var]) %in% c("numeric","double","integer") ) {
    ptlist_bi[[var]] <- ggplot(data = dataset) + 
      geom_boxplot(aes_string(y = var, x ="TenYearCHD" )) + 
      theme_linedraw() +
      xlab("TenYearCHD") + 
      ylab(var)
  }
}
marrangeGrob(grobs=ptlist_bi, nrow=2, ncol=2)

#Randomly splitting the data into train and test sets 
set.seed(1000)
split = sample.split(dataset$TenYearCHD, SplitRatio = 0.65)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)

heartlog = glm(TenYearCHD ~. , data = train, family = binomial) 
summary(heartlog)

#model with only significant value 
modheartnew = glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + sysBP + glucose, data = train, family = binomial)

summary(modheartnew)

attach(test)
CHDheartupdated <- data.frame(male, age, cigsPerDay, prevalentStroke, prevalentHyp, sysBP, glucose, TenYearCHD)

# validate the model 
attach(CHDheartupdated)

modheartnew.probs <- predict(object = modheartnew, newdata = CHDheartupdated, type = "response" )

#creating ROC curve using the entire sample 
par(pty = "s")
modheartnew.ROC <- roc(response = TenYearCHD,
                       predictor = modheartnew.probs,
                       plot = TRUE, legacy.axes = TRUE,
                       ylab = "Sensitivity", col = "blue",
                       col.axis = "blue", col.lab = "blue",
                       col.main = "blue", 
                       main = "ROC curve for Logistic Regression Model")
modheartnew.ROC$auc

coords(roc = modheartnew.ROC, x = "best", best.method = "youden")

# Confusion matrix validation 

modheartnew.validation.predictions <- rep(x = 0, times = 2071)

modheartnew.validation.predictions[modheartnew.probs >= 0.1235685] <- 1

#produce confusion matrix 
attach(CHDheartupdated)
tab = (table(actual_value = CHDheartupdated$TenYearCHD , predicted_value = modheartnew.probs > 0.1235685 ))
tab
sum(diag(tab)/sum(tab))











