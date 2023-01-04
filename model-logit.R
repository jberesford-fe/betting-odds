source('make-train-test.R')

## Logit model

# Define recipe
match_pred <- 
  recipe(home_wins~.,data = df_train)%>%
  update_role(match_id,new_role='id')%>%
  step_dummy(all_nominal_predictors())%>%
  step_zv(all_predictors())

summary(match_pred)

# Define model
lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

# Fit model with recipe
match_workflow <-
  workflow()%>%
  add_model(lr_mod)%>%
  add_recipe(match_pred)

match_workflow


match_fit <- 
  match_workflow %>% 
  fit(data = df_train)

coefs<-match_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()%>%
  arrange(p.value)


results_augmented <- augment(match_fit, df_test)

results_augmented%>%
  select(home_wins,match_id,.pred_class)


results_augmented%>%
  accuracy(home_wins,.pred_class)
