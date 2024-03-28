library(tidyverse)
library(tidymodels)
# install.packages('vip')
# install.packages('DALEX')
library(vip)
library(DALEX)



# Data and Model Setup  ---------------------------------------------------

set.seed(42)
cars <- read_csv('https://www.dropbox.com/scl/fi/xavej23qpauvx3xfdq7zh/car_sales.csv?rlkey=4mfp6tpia0uqkcoiqf9jleau3&dl=1') %>% 
  slice_sample(prop = .2)

cars_split <- initial_split(cars, strata = sellingprice_log)

cars_training <- cars_split %>% training()
cars_testing <- cars_split %>% testing()

cars_rec <- recipe(sellingprice_log ~ .,
                   data = cars_training) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_other(make, threshold = 0.03) %>% 
  step_other(model, threshold = 0.01) %>% 
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())

# in case you want to look at the effect of the recipe:
cars_rec %>% prep() %>% juice() %>% glimpse()

lr_spec <- linear_reg()

xgb_spec <- boost_tree() %>% 
  set_engine('xgboost') %>% 
  set_mode('regression')

wkfl_xgb <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_recipe(cars_rec)

wkfl_lr <- workflow() %>% 
  add_model(lr_spec) %>% 
  add_recipe(cars_rec)

# Now run some models:
final_lr <- wkfl_lr %>% 
  last_fit(split = cars_split)

final_xgb <- wkfl_xgb %>% 
  last_fit(split = cars_split)



# Let's look at some explainability: --------------------------------------

top_features <- final_lr %>% extract_fit_parsnip() %>% tidy() %>%  dplyr::slice(2:11) %>% pull(term)


# Why does this produce "Warnings"? See also the next question. Check out the
# documentation for `DALEX::explain()` and try to understand the warning as well
# as the error on the next command.
explainer_lr <- DALEX::explain(model = final_lr %>% extract_fit_parsnip(), 
                               data = cars_training %>% select(-sellingprice_log),
                               y = cars_training %>% pull(sellingprice_log), 
                               label = "Linear Regression")

# Those warnings lead to an error on the next command below. Why?
# Hint: You can also compare the DALEX::explain() function call above to the one below.
model_profile(explainer_lr, variables = top_features)


# Here is one method for generating "explainers":
explainer_lr <- DALEX::explain(model = final_lr %>% extract_fit_parsnip(), 
                               data = cars_rec %>% prep() %>% juice() %>% select(-sellingprice_log),
                               y = cars_training %>% pull(sellingprice_log), 
                               label = "Linear Regression")

explainer_xgb <- DALEX::explain(model = final_xgb %>% extract_fit_parsnip(), 
                                data = cars_rec %>% prep() %>% juice() %>% select(-sellingprice_log),
                                y = cars_training %>% pull(sellingprice_log), 
                                label = "XGBoost")


# Compare performance of the two models:
final_lr %>% collect_metrics()
final_xgb %>% collect_metrics()

# Please go here to learn about partial dependence plots:
# https://chat.openai.com/share/f391c97a-3133-497c-a853-5227901d1699

# Now see if you can interpret these plots:
# Why do they look so different? 
# What does this tell you about why xgboost has better performance than linear regression?
pdp_lr <- model_profile(explainer_lr, variables = top_features)
pdp_xgb <- model_profile(explainer_xgb, variables = top_features)
plot(pdp_lr)
plot(pdp_xgb)

cars_training %>% 
  ggplot(aes(x = condition)) +
  geom_histogram()

# And another method:
# Why do these variable importance plots look so different?
# Again, what does that tell you about the fundamental differences between xgboost and linear regression?
vip(final_lr %>% extract_fit_parsnip())
vip(final_xgb %>% extract_fit_parsnip())





