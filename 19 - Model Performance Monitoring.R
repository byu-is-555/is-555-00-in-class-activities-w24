library(tidyverse)
library(tidymodels)
library(vetiver)
library(pins)

# Data and Model Setup
rentals <-  read_csv('https://www.dropbox.com/scl/fi/khs0biz27katxeoduhy8c/housing_with_dates.csv?rlkey=2xvu3sr5eqbtxteh28cz1df0r&dl=1')

set.seed(42)
rentals_split <- initial_split(rentals, strata = sale_price)

rentals_training <- rentals_split %>% training()
rentals_testing <- rentals_split %>% testing()

rentals_rec <- recipe(sale_price ~ .,
                      data = rentals_training) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_other(ms_sub_class,ms_zoning,exterior_1st,exterior_2nd,utilities,electrical) %>% 
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())

rentals_rf_model <- rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')

rentals_wkfl <- workflow() %>% 
  add_model(rentals_rf_model) %>% 
  add_recipe(rentals_rec)

# finalize the model
final_model <- rentals_wkfl %>% 
  last_fit(split = rentals_split)

# Prep for deployment:


# Write the (deployable) vetiver model object to a pin board:



# Now let's set ourselves up to do some performance monitoring. First we need 
# some metrics that represent "expected" performance based on our test set used
# during training.



# Let's look at performance over time during the training window. This requires
# generating predictions from the training window. Which data should we use?



# The metrics are intentionally arranged for easy plotting. Create a line chart
# that summarizes performance over time.




# Metrics are designed to be pinned alongside models for later referencing:


# Now let's roll the clock forward...
# 
# Tick...
# 
#   Tock...
# 
# Tick...
# 
#   Tock...
# 
# Tick...
# 
#   Tock...
# 
# Tick...
# 
#   Tock...

# Read in new data:
new_rental_listings <- read_csv('https://www.dropbox.com/scl/fi/sgehr76gch8tux9pzdgfz/new_housing_with_dates.csv?rlkey=do0s9jbrgb2y53e728zrxl6zo&dl=1')

# Read in model from the pin board:


# Calculate performance metrics on new data



# Update metrics (on the pin_board) so that we can use them next time:



# Reusable plot from the pin_board "stash":




