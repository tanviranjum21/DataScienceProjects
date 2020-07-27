#Mushroom Analysis target variable classes wether edible or 
#poisonous 
#required libraries 
library(tidyverse)
library(caret)
library(ggplot2)
library(tidymodels)
library(stringr)
library(rpart)
library(rpart.plot)
library(rattle) #this is a graphical interface for data mining 
library(skimr)
#load the dataset 
mushroom <- read_csv("mushroom_dataset.csv")
glimpse(mushroom)
#making each variable as a factor 
mushroom <- mushroom %>% map_df(function(.x) as.factor(.x))
glimpse(mushroom)

#exploring the data
#naming the data
colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")
skim(mushroom)
head(mushroom)
#checking the number of levels of each variable 

number_of_class <- function(x) {
  x <- length(levels(x))
}

x <- mushroom %>% 
  map_dbl(function(x) number_of_class(x)) %>% 
  as_tibble() %>% 
  rownames_to_column() %>% 
  arrange(desc(value))
colnames(x) <- c("Variable name", "Number of levels")
print(x)

#Veil type variable has only one factor, which doesn't make 
#any sense so we will remove veil_type from the dataset
mushroom <- mushroom %>% 
  select(-veil_type)
#checking missing data
map_dbl(mushroom, function(.x){
  sum(is.na(.x))
})

#visualize the data
library(scales)
ggplot(mushroom, aes(x = cap_surface, y = cap_color, col = edibility))+
  geom_jitter(alpha = 0.5)+
  scale_color_manual(values = c("green","red"))
#so if cap surface is frivorous then its safer but when it is 
#on smooth surface its dangerous mostly 

#taking cap_shap and cap_color as variable 
ggplot(mushroom, aes(x = cap_shape, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(values = c("green", "red"))
#in this case better to stick with bell shape mushrooms

#now visualizing  edibility with odor 
ggplot(mushroom, aes(x = edibility, y = odor, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(values = c("green", "red"))
#so if it smells fishy, spicy or pungent it's dangerous 

###############Modeling###############################

set.seed(1234)
edibility_split = mushroom%>%
  initial_split(strata = edibility)

train1 = training(edibility_split)

test1  = testing(edibility_split)

#checking the quality of the splits in regard to our predicted
#varibale
round(prop.table(table(mushroom$edibility)),2)

round(prop.table(table(train1$edibility)),2)

round(prop.table(table(test1$edibility)),2)

#using regression tree
set.seed(1234)
model_tree_mushroom <- rpart(edibility~.,data = train1, method = "class")
model_tree_mushroom

confusionMatrix(data = predict(model_tree_mushroom, type = "class"),
                reference = train1$edibility,
                positive = "e")


#so we have 38 mushrooms which is predicted as edible but it is actually poisonous 

penalty_matrix <- matrix(c(0,1,10,0), byrow = TRUE, nrow = 2)
model_tree_penalty_mushroom <- rpart(edibility~., data = train1, method = "class",
                                     parms = list(loss = penalty_matrix))
confusionMatrix(data = predict(model_tree_penalty_mushroom, type = "class"),
                reference = train1$edibility,
                positive = "e")
#another way of increasing the accuracy is to use cp parameter. initially we will
#have very low cp (very deep tree) then we will prune the tree and increase the 
#efficiency 

model_tree_mushroom <- rpart(edibility~., data = train1, method = "class", cp = 0.00001)
printcp(model_tree_mushroom)
plotcp(model_tree_mushroom)
#the lowest xerror is on number 5th split 
model_tree_mushroom$cptable[which.min(model_tree_mushroom$cptable[,"xerror"]),"CP"]

#now let's start pruning to get the lowest cross validation error

bestcp <- round(model_tree_mushroom$cptable[which.min(model_tree_mushroom$cptable[,"xerror"]),"CP"], 4)
model_tree_mushroom_pruned <- prune(model_tree_mushroom, cp = bestcp) 
rpart.plot(model_tree_mushroom_pruned, extra = 104, box.palette = "GnBu",
           branch.lty = 3, shadow.col = "gray", nn = TRUE)

confusionMatrix(data = predict(model_tree_mushroom_pruned, type = "class"),
                reference = train1$edibility,
                positive = "e")

#now let's check for testing data
test_tree_mushroom <- predict(model_tree_mushroom, newdata = test1)
confusionMatrix(data = predict(model_tree_mushroom, newdata = test1, type = "class"),
                reference = test1$edibility,
                positive = "e")
