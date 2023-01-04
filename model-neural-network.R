source('make-train-test.R')

# Define recipe
match_pred <- 
  recipe(home_wins~.,data = df_train)%>%
  update_role(match_id,new_role='id')%>%
  step_dummy(all_nominal_predictors())%>%
  step_zv(all_predictors())

summary(match_pred)

# Define model
neuralnet_mod<-
  mlp(
    hidden_units = integer(1),
    penalty = double(1),
    dropout = double(1),
    epochs = integer(1),
    activation = character(1)
  ) %>% 
  set_engine("keras") %>% 
  set_mode("classification") %>% 
  translate()

# Fit model with recipe
match_workflow <-
  workflow()%>%
  add_model(neuralnet_mod)%>%
  add_recipe(match_pred)

match_workflow

match_fit <- 
  match_workflow %>% 
  fit(data = df_train)


results_augmented <- augment(match_fit, df_test)

results_augmented%>%
  select(home_wins,match_id,.pred_class)


results_augmented%>%
  accuracy(home_wins,.pred_class)
