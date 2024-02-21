library(tidyverse)

df <- read_csv('https://www.dropbox.com/s/petmujrpxa3qn3p/titanic.csv?dl=1') %>% 
  janitor::clean_names()

df %>% glimpse

# Data Dictionary:
# 
# passenger_id = passenger identifier
# survived     = did the passenger survive?
# pclass       = passenger class (1st, 2nd, 3rd)
# name         = passenger name
# sex          = passenger sex
# age          = passenger age
# sib_sp       = number of this passenger's siblings or spouses on board
# parch        = number of this passenger's parents or children on board
# ticket       = ticket number
# fare         = ticket cost/fare
# cabin        = cabin number
# embarked     = Port of embarkation (C=Cherbourg, Q=Queenstown, S=Southampton)


# Missingness -------------------------------------------------------------------------------------------
# Algorithms don't like missing values. It messes with the math.

# Get a feel for the missingness



# first check: is the missingness relevant?
# use summarize across


# fill in missing age values, check our work


# now handle embarked, this time using replace_na()
# Again, check our work


# What about cabin missingness? Random?
# use summarize across again.
# context: private cabins were assigned for some but not all.



# Outlier Treatments ------------------------------------------------------------------------------------


# Pass the four columns to summary() to check means, maxes
 

# calculate extreme threshold caps based on 99th percentile


# Now check how many are beyond the percentile caps


# cap age and fare, and check work before saving


# save the result to df




