library(tidyverse)
library(tidymodels)

# Install these if you haven't:
# install.packages('vetiver')
# install.packages('pins')
# install.packages('plumber')
# install.packages('ranger')

library(ranger)
library(vetiver)
library(pins)
library(plumber)

# Data and Model Setup
housing <-  read_csv('https://www.dropbox.com/scl/fi/9s2pkk23rsqokh2hymp92/housing_training.csv?rlkey=45f14zqlghrzwrmrhu47xapy9&dl=1')

set.seed(42)
housing_split <- initial_split(housing, strata = sale_price)

housing_training <- housing_split %>% training()
housing_testing <- housing_split %>% testing()

housing_rec <- recipe(sale_price ~ .,
                      data = housing_training) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_other(ms_sub_class,ms_zoning,exterior_1st,exterior_2nd,utilities,electrical) %>% 
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())


housing_rf_model <- rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')

housing_wkfl <- workflow() %>% 
  add_model(housing_rf_model) %>% 
  add_recipe(housing_rec)

# Assume we did all the things (crossvalidation, tuning, etc.), 
# and that we're ready to deploy. Drumroll, please.....

# What's first? (Or, I should say last?)


# Now we need to extract the trained workflow from the larger final fit result:


# And create from that (trained) workflow a deployable model object. 
# We'll use vetiver for this. Give it a name, too.


# Now let's create a plumber-based API server for local testing
# pr(), vetiver_api(), pr_run()



###### Let's go check out our temporary API.
###### Copy the code below into a new session (Session >> New Session)
# 
# library(tidyverse)
# library(tidymodels)
# library(vetiver)
# 
# next_months_data <- read_csv('https://www.dropbox.com/scl/fi/ztf8ipi4i4n5rfwtwcmh0/housing_testing.csv?rlkey=yecihhbp6hbf88rfr9w3kysbk&dl=1')
# 
# 
# local_api <- vetiver_endpoint("http://127.0.0.1:8888/predict")
# 
# # Now use the standard predict function to hit the api with our new data:
# api_preds <- predict(local_api,
#                      new_data = next_months_data)
# 
# # Look at performance over the past month:
# next_months_data %>%
#   bind_cols(api_preds) %>%
#   rsq(truth = sale_price, estimate = .pred)
#
#
###### [--End Copy--]





# Let's look at publishing with the pins package:
# First, create a "pin_board"


# Write the (deployable) vetiver model object to the board:


# Boards can hold lots of different models (and versions). These can be listed 
# and retreived. 
pin_board %>% pin_list()
pin_board %>% pin_versions('[model name]')


# Moving from a pin board to a docker-based plumber API is as simple as preparing
# the docker scripts:










