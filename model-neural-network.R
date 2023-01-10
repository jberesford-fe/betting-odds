
# Define recipe
match_pred <- 
  recipe(home_wins~.,data = df_train)%>%
  step_dummy(all_nominal_predictors())%>%
  step_zv(all_predictors())

# Define model
nnet_spec <- 
  mlp(penalty = 0, epochs = 100) %>% 
  # This model can be used for classification or regression, so set mode
  set_mode("classification") %>% 
  set_engine("nnet")

  # Fit model with recipe
nnet_fit <- nnet_spec %>% fit(home_wins ~., data=df_train)

augmented <- augment(nnet_fit,df_test)

augmented%>%
  accuracy(home_wins,.pred_class.pred_{something})  # To get probaility


augmented%>%
  conf_mat(home_wins,.pred_class)  
