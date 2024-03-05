library(tidyverse)
library(tidymodels)

# Okay let's work through the (cleaned) titanic data from last week:
titanic <- read_csv('https://www.dropbox.com/s/92funarubgk5rzh/titanic_clean.csv?dl=1')


# First let's make sure factors are factors
leo <- titanic %>% 
  mutate(across(c(survived, pclass, had_cabin), ~as.factor(.x)))


# Now let's do a train/test split
set.seed(42)
leo_split <- initial_split(leo, strata = survived)

leo_training <- leo_split %>% training()
leo_testing <- leo_split %>% testing()

# Plan the model setup, including the engine and mode
leo_model <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')

show_engines('logistic_reg')

# relevant model types: logistic_reg(), linear_reg(), decision_tree(), rand_forest(), boost_tree()
# show_engines('logistic_reg')

# Now fit a model, look at output
leo_fit <- leo_model %>% 
  fit(survived ~ .,
      data = leo_training)

leo_fit %>% tidy()

# Calculate predictions, 
# including class predictions _and_ probabilities
leo_predictions <- leo_fit %>% 
  predict(new_data = leo_testing)

leo_predictions_prob <- leo_fit %>% 
  predict(new_data = leo_testing,
          type = 'prob')

leo_results <- leo_testing %>% 
  bind_cols(leo_predictions, leo_predictions_prob)
  

# Now let's build a confusion matrix and explore a few of the related metrics.
# conf_mat(), sens()
leo_results %>% 
  conf_mat(truth = survived,
           estimate = .pred_class) %>% 
  summary()

leo_results %>% 
  sens(truth = survived,
       estimate = .pred_class)

# Let's get fancy:
# roc_curve(), roc_auc(), autoplot()
roc_data <- leo_results %>% 
  roc_curve(truth = survived, .pred_0)

roc_data %>% 
  autoplot()

leo_results %>% 
  roc_auc(truth = survived, .pred_0)

# Finalize the model with last_fit()
leo_last_fit <- leo_model %>% 
  last_fit(survived ~ .,
         split = leo_split)

# finalized object, extract predictions, metrics 
# with dedicated collect_* functions:
leo_last_fit %>% collect_predictions()
leo_last_fit %>% collect_metrics()


