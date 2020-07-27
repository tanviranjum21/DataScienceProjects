library(tidyverse)
library(tidymodels)
library(skimr)

#load the data
cost = read.csv("https://raw.githubusercontent.com/Rishikesh321/Breast-cancer-prediction-by-Tidymodels/master/insurance.csv")

head(cost)

skim(cost)

#splitting the model

set.seed(63456)

cost_split = cost%>%
  initial_split(strata = charges)

train1 = training(cost_split)

test1  = testing(cost_split)

nrow(train1)
nrow(test1)

cost_rec = recipe(charges~.,data = train1)%>%
  step_dummy(all_nominal(),-all_outcomes())%>%
  step_zv(all_numeric())%>%
  step_normalize(all_numeric())%>%
  prep() 
cost_rec

DPP_train1 = juice(cost_rec)
DPP_test1 = bake(cost_rec,test1) 

DPP_train1
DPP_test1 = bake(cost_rec,test1)
DPP_test1

lm_spec = linear_reg()%>%
  set_engine("lm")%>%
  set_mode("regression")

lm_fit = lm_spec%>%
  fit(charges~.,data = train1)

lm_fit

#random_forest
rd_spec1 = rand_forest()%>%
  set_engine("ranger",importance = "impurity")%>%
  set_mode("regression") 

rad_fit1 = rd_spec1%>%
  fit(charges ~.,data = train1) 
rad_fit1


#Evaluating the model
results_train =  lm_fit %>%
  predict(new_data = train1) %>%
  mutate(
    truth = train1$charges,
    model = "lm"
  ) %>%
  bind_rows(rad_fit1 %>%
              predict(new_data = train1) %>%
              mutate(
                truth = train1$charges,
                model = "rf"
              ))



results_test = lm_fit %>%
  predict(new_data = test1) %>%
  mutate(
    truth = test1$charges,
    model = "lm"
  ) %>%
  bind_rows(rad_fit1 %>%
              predict(new_data = test1) %>%
              mutate(
                truth = test1$charges,
                model = "rf"
              ))

#root mean square error 
results_train %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

results_test %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)


results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred, color = model)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train) +
  labs(
    x = "Truth",
    y = "Predicted Charges",
    color = "Type of model"
  )

#Resampling and Cross validation 
set.seed(489876)

validation_splits1 = mc_cv(DPP_train1,prop = 0.9,strata = charges)

validation_splits1


rd_res1 = fit_resamples(
  
  charges ~.,
  rd_spec1,
  validation_splits1,
  control = control_resamples(save_pred = TRUE)
  
)

rd_res1%>%
  collect_metrics()


rd_res1 %>%
  unnest(.predictions) %>%
  ggplot(aes(charges, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Truth",
    y = "Predicted Charges",
    color = NULL
  )
