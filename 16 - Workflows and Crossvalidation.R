library(tidymodels)
# install.packages('ranger')

data(ames)
housing <- ames %>% janitor::clean_names()
housing %>% glimpse

set.seed(42)
housing_split <- initial_split(housing, strata = sale_price)

housing_training <- housing_split %>% training()
housing_testing <- housing_split %>% testing()

# Build feature engineering pipeline
housing_rec <- recipe(sale_price ~ .,
                      data = housing_training) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.85) %>% 
  step_nzv(all_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

housing_rf_model <- rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')

# show_engines('rand_forest')

# create a reusable workflow

housing_wkfl <- workflow() %>% 
  add_model(housing_rf_model) %>% 
  add_recipe(housing_rec)


# First, just do an overall shortcut to fit/evaluate

single_result <- housing_wkfl %>% 
  last_fit(split = housing_split)

single_result %>% collect_metrics()
single_result %>% collect_predictions()

# Now some k-fold validation
set.seed(42)
housing_folds <- vfold_cv(housing_training, v = 10, strata = sale_price)


cv_results <- housing_wkfl %>% 
  fit_resamples(resamples = housing_folds)


# Use collect_metrics with and without summarization
cv_results %>% collect_metrics()
cv_results %>% collect_metrics(summarize = F)

