library(tidyverse)
library(tidymodels)


# Setup w/ Credit Data --------------------------------------------------------

cr_data <- read_csv('https://www.dropbox.com/scl/fi/vykejw5ud9ejjvcc442gd/credit_small.csv?rlkey=zuyurxikxickgdjchh6681j91&dl=1') %>% 
  mutate(status = as.factor(status))

cr_data %>% glimpse()

# Model setup:
set.seed(42)
cr_split <- initial_split(cr_data, strata = status)
cr_training <- cr_split %>% training()
cr_testing <- cr_split %>% testing()

# Let's create a recipe that:
#    - imputes missing numeric values
#    - log transforms assets, debt, income, price, expenses
#    - normalizes all numeric predictors
#    - dummy codes all categories
cr_rec <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = 1)

cr_training %>% count(status)
cr_rec %>% prep() %>% juice() %>% count(status)

# Now let's setup a model spec (rpart decision tree), workflow, and do a cross validation.

dt_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_wkfl <- workflow() %>% 
  add_model(dt_spec) %>% 
  add_recipe(cr_rec)

set.seed(42)
cr_folds <- vfold_cv(cr_training, strata = status)

cv_fit <- cr_wkfl %>% 
  fit_resamples(resamples = cr_folds)

cv_fit %>% collect_metrics(summarize = F)

# Next, a tunable model specification, recipe, and workflow:

dt_spec_tune <- decision_tree(
  cost_complexity = tune(),
  min_n = tune()
) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_rec_tune <- recipe(status ~ ., 
                      data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = tune())

cr_wkfl_tune <- workflow() %>% 
  add_model(dt_spec_tune) %>% 
  add_recipe(cr_rec_tune)

# Extract parameters, create a grid to search, then search:

cr_grid <- grid_random(extract_parameter_set_dials(cr_wkfl_tune), size = 50)

doParallel::registerDoParallel(cores = 7)

grid_search_result <- cr_wkfl_tune %>% 
  tune_grid(resamples = cr_folds,
            grid = cr_grid)


# Look at the results, selecting the best one and using those to finalize.
grid_search_result %>% collect_metrics()
grid_search_result %>% collect_metrics(summarize = F)

grid_search_result %>% show_best(metric = 'roc_auc')

winners <- grid_search_result %>% select_best(metric = 'roc_auc')

finalized_wkfl <- cr_wkfl_tune %>% 
  finalize_workflow(winners)

all_done <- finalized_wkfl %>% 
  last_fit(split = cr_split)

all_done %>% collect_metrics()





