library(tidyverse)
library(tidymodels)

# Same Titanic data
titanic <- read_csv('https://www.dropbox.com/scl/fi/1v8a71opb3s2zro2lpqus/titanic_roc.csv?rlkey=k350qlym4wg6r40ipnxgkv0lv&dl=1')

# convert factors like before:
leo <- titanic %>% 
  mutate(across(c(pclass, had_cabin, sex, survived), ~as.factor(.x))) 
  
leo %>% count(witnessed_wwi_begin, survived)

##### Brief aside: note the order of the `survived` factor: 
##### "survived" is the event of interest, and it's listed SECOND. 
##### (This will be relevant below.)
levels(leo$survived)

# Okay back to the action:
# Train/Test Split, model specification
set.seed(42)
leo_split <- initial_split(leo, strata = survived)
leo_training <- leo_split %>% training()
leo_testing <- leo_split %>% testing()

leo_model <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')


# Now let's fit three different models:
leo_fit_all <- leo_model %>% 
  fit(survived ~ .,                                   # <-- All predictors
      data = leo_training)

leo_fit_some <- leo_model %>% 
  fit(survived ~ had_cabin + fare + age + pclass,     # <-- Some predictors
      data = leo_training)

leo_fit_month <- leo_model %>% 
  fit(survived ~ birth_month,                         # <-- Just the birth_month
      data = leo_training)

leo_training %>% count(survived, witnessed_wwi_begin)
# Generate survival probabilities for each of the model fits:
leo_predictions_all <- leo_fit_all %>% 
  predict(new_data = leo_testing,
          type = 'prob') %>% 
  select(.pred_survived) %>% 
  rename(.pred_survived_all = .pred_survived) 

leo_predictions_some <- leo_fit_some %>% 
  predict(new_data = leo_testing,
          type = 'prob') %>% 
  transmute(.pred_survived_some = .pred_survived)  # <-- transmute is a select + mutate shortcut!

leo_predictions_month <- leo_fit_month %>% 
  predict(new_data = leo_testing,
          type = 'prob') %>% 
  transmute(.pred_survived_month = .pred_survived)

# Combine them together so we can compare them:
leo_results <- leo_testing %>% 
  bind_cols(leo_predictions_all, leo_predictions_some, leo_predictions_month)


# Reminder: "survived" is the second level in our outcome:
levels(leo_results$survived)

# Now use `roc_curve()` and `autoplot()` to look at some ROC curves:
# Note that we have to tell the function that the event of interest (survival) is
# the "second" of the two levels in our outcome factor.
leo_results %>% 
  roc_curve(truth = survived, .pred_survived_all, event_level = 'second') %>% 
  autoplot() +
  labs(title = 'ROC Curve: All Predictors')

leo_results %>% 
  roc_curve(truth = survived, .pred_survived_some, event_level = 'second') %>% 
  autoplot() +
  labs(title = 'ROC Curve: Some Predictors')

leo_results %>% 
  roc_curve(truth = survived, .pred_survived_month, event_level = 'second') %>% 
  autoplot() +
  labs(title = 'ROC Curve: Birth Month')

# These curves can be summarized numerically by calculating the area underneath:
# This is called the "ROC AUC", or just "auc" for short.
leo_results %>% 
  roc_auc(truth = survived, .pred_survived_all, event_level = 'second')
leo_results %>% 
  roc_auc(truth = survived, .pred_survived_some, event_level = 'second')
leo_results %>% 
  roc_auc(truth = survived, .pred_survived_month, event_level = 'second')

