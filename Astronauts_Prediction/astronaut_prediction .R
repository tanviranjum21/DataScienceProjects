#The problem that we are going to solve is:

#Predict astronauts' mission duration with tidymodels and  bootstrap aggregation 
#Like for how long the mission will take 

##################library files#####################
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(skimr)



##################exploring the data################
#read the data
astronauts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv")

skim(astronauts)

#visualize the change of mission year in decades 
astronauts %>% 
  mutate(year_of_mission_in_decades = 10 * (year_of_mission %/% 10),
         year_of_mission_in_decades = factor(year_of_mission_in_decades)) %>%
  ggplot(aes(year_of_mission_in_decades, hours_mission,
             fill = year_of_mission_in_decades, color = year_of_mission_in_decades)) + 
  geom_boxplot(show.legend = FALSE, alpha = 0.2, size = 1.5)+
  scale_y_log10()+
  labs(x = NULL, y = "Duration of mission in hours")


#Selecting the dataset that we are going to use 
astronauts_df <- astronauts %>% 
  select(name, mission_title, hours_mission, military_civilian, occupation,
         year_of_mission, in_orbit) %>% 
  mutate(in_orbit = case_when(str_detect(in_orbit, "^Salyut")~"Salyut",
                              str_detect(in_orbit, "^STS")~ "STS",
                              TRUE ~ in_orbit),
         occupation = str_to_lower(occupation)) %>% 
  filter(hours_mission > 0) %>% 
  mutate(hours_mission = log(hours_mission)) %>% 
  na.omit()

#splitting the data

set.seed(123)
astro_split <- initial_split(astronauts_df, strata = hours_mission)
astro_train <- training(astro_split)
astro_test <- testing(astro_split)

#Data preprocessing 
astro_recipe <- recipe(hours_mission ~ ., data = astro_train) %>% 
  update_role(name, mission_title, new_role = "id") %>% 
  step_other(occupation, in_orbit, threshold = 0.005) %>% 
  step_dummy(all_nominal(), -has_role("id"))

#model bulding and bootstrap aggregate 
library(baguette)

astro_workflow <- workflow() %>% 
  add_recipe(astro_recipe)

tree_spec <- bag_tree() %>% 
  set_engine("rpart", times = 25) %>% 
  set_mode("regression")
mars_spec <- bag_mars() %>% 
  set_engine("earth", times = 25) %>% 
  set_mode("regression")

tree_rs <- astro_workflow %>% 
  add_model(tree_spec) %>% 
  fit(astro_train)
mars_rs <-astro_workflow %>% 
  add_model(mars_spec) %>% 
  fit(astro_train)

##Evaluate the modeling 

test_result <- astro_test %>% 
  bind_cols(predict(tree_rs, astro_test)) %>% 
  rename(.pred_tree = .pred) %>% 
  bind_cols(predict(mars_rs, astro_test)) %>% 
  rename(.pred_mars = .pred)
test_result

test_result %>%
  metrics(hours_mission, .pred_tree)

test_result %>%
  metrics(hours_mission, .pred_mars)  

#Let's make some "new" astronauts to understand the kinds of 
#prediction our bagged tree model is making 

new_astronauts <- crossing(in_orbit = c("ISS", "STS", "Mir", "other"),
                           military_civilian = "civilian",
                           occupation = "other",
                           year_of_mission = seq(1960,2020, by = 10),
                           name = "id", mission_title = "id") %>% 
  filter(!(in_orbit == "ISS" & year_of_mission < 2000),
         !(in_orbit == "Mir" & year_of_mission < 1990),
         !(in_orbit == "STS" & year_of_mission < 1980),
         !(in_orbit == "STS" & year_of_mission < 2010))
predict(tree_rs, new_astronauts)

new_astronauts %>%
  bind_cols(predict(tree_rs, new_astronauts)) %>%
  ggplot(aes(year_of_mission, .pred, color = in_orbit)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    x = NULL, y = "Duration of mission in hours (predicted, on log scale)",
    color = NULL, title = "How did the duration of astronauts' missions change over time?",
    subtitle = "Predicted using bagged decision tree model"
  )

new_astronauts %>%
  bind_cols(predict(mars_rs, new_astronauts)) %>%
  ggplot(aes(year_of_mission, .pred, color = in_orbit)) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    x = NULL, y = "Duration of mission in hours (predicted, on log scale)",
    color = NULL, title = "How did the duration of astronauts' missions change over time?",
    subtitle = "Predicted using mars model"
  )

#SO here we can see that mission to space station are longer that space shuttles 
#and also "other" category change characteristics over time dramatically according 
#to our prediction






