library(tidyverse)

df_raw <- read_csv('https://www.dropbox.com/s/petmujrpxa3qn3p/titanic.csv?dl=1')

df_raw %>% glimpse

# Data Dictionary:
# 
# $ PassengerId  = passenger identifier
# $ Survived     = did the passenger survive?
# $ Pclass       = passenger class (1st, 2nd, 3rd)
# $ Name         = passenger name
# $ Sex          = passenger sex
# $ Age          = passenger age
# $ SibSp        = number of this passenger's siblings or spouses on board
# $ Parch        = number of this passenger's parents or children on board
# $ Ticket       = ticket number
# $ Fare         = ticket cost/fare
# $ Cabin        = cabin number
# $ Embarked     = Port of embarkation (C=Cherbourg, Q=Queenstown, S=Southampton)

df <- df_raw %>% janitor::clean_names()


# Missingness -------------------------------------------------------------------------------------------
# Algorithms don't like missing values. It messes with the math.

# Get a feel for the missingness
df %>% 
  summarize(across(everything(), ~sum(is.na(.x))))



# first check: is the missingness relevant?
# use summarize across
df %>% 
  group_by(is.na(age)) %>% 
  summarize(across(everything(), ~mean(.x, na.rm =T)))


# fill in missing age values, check our work
df <- df %>% 
  mutate(age = if_else(is.na(age), mean(age, na.rm = T), age))
 
mean(df$age)
# now handle embarked, this time using replace_na()
# Again, check our work
df <- df %>% 
  mutate(embarked = replace_na(embarked, 'O'))


# What about cabin missingness? Random?
# use summarize across again.
# context: private cabins were assigned for some but not all.
df %>% 
  group_by(is.na(cabin)) %>% 
  summarize(across(everything(), ~mean(.x, na.rm =T)))




# Outlier Treatments ------------------------------------------------------------------------------------



# Pass the four columns to summary() to check means, maxes
outlier_candidates <- c('age', 'sib_sp', 'parch', 'fare')

df %>% 
  select(all_of(outlier_candidates)) %>% 
  summary()

df %>% 
  select(all_of(outlier_candidates)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(y = value, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, scales = 'free_y')

# calculate extreme threshold caps based on 99th percentile
age_cap     <- quantile(df$age, .99)
sib_sp_cap  <- quantile(df$sib_sp, .99)
parch_cap   <- quantile(df$parch, .99)
fare_cap    <- quantile(df$fare, .99)

age_cap     <- quantile(pull(df,age), .99)
sib_sp_cap  <- quantile(pull(df,sib_sp), .99)
parch_cap   <- quantile(pull(df,parch), .99)
fare_cap    <- quantile(pull(df,fare), .99)

# Now check how many are beyond the percentile caps
df %>% 
  summarize(count_over_age = sum(age > age_cap),
            count_over_sib_sp = sum(sib_sp > sib_sp_cap),
            count_over_parch = sum(parch > parch_cap),
            count_over_fare = sum(fare > fare_cap))

# cap age and fare, and check work before saving

df %>%
  mutate(
    age = if_else(age > age_cap, age_cap, age),
    fare = if_else(fare > fare_cap, fare_cap, fare)
  )

# save the result to df







# Transforming Features ---------------------------------------------------------------------------------
# Here's the basic idea behind Box-Cox transformations:
tribble(
  ~value_orig, ~transformation, ~value_tr,
  30,          'y^-3',         30^-3,
  30,          'y^-2',         30^-2,
  30,          'y^-1',         30^-1,
  30,          'y^-(1/2)',     30^-(1/2),
  30,          'log(y)',       log(30),
  30,          'y^(1/2)',      30^(1/2),
  30,          'y^1',          30^1,
  30,          'y^2',          30^2,
  30,          'y^3',          30^3
)


# Examine distributions of age and fare



# Let's transform the fare column
# y^-2
# y^-1
# y^-(1/2)
# log(y)
# y^(1/2)
# y^1
# y^2
# 
# Don't worry. I'll give you the code. :) 
df %>% 
  mutate(
    fare_t2_ = fare^-2,
    fare_t1_ = fare^-1,
    fare_th_ = fare^-(1/2),
    fare_tln = log(fare),
    fare_th  = fare^(1/2),
    fare_t1  = fare^1,
    fare_t2  = fare^2
  ) 

# now let's visualize the effect of the transformations to see 
# which one makes sense.




# Note here that we need to add one to the fare before applying the power transformation. Zero raised to the
# power of -1/2 is not a number. Sometimes, to make a transformation work, we have to protect the transformation
# from the possible math errors. In this case, a horizontal shift of 1 does not affect the distribution and
# the column is eventually going to be standardized anyway. 
# (This is what caused the errors we saw in class...)
df %>% count(fare)

df <- df %>% 
  mutate(fare = (fare+1)^-(1/2))



# Now we can scale the numeric columns using the old-school `scale()` function. Because it's an old function,
# it returns the data in a fairly annoying format (as a one-column matrix), so we'll have to cast the result
# as a numeric vector so that it can be treated as a column in the mutate function:
df %>% 
  select(where(is.numeric)) %>% 
  mutate(across(c(age,sib_sp,parch,fare), ~as.numeric(scale(.x))))

# Alternatively, we could write our own scaling function and use it in the mutate, which would look something
# like this:
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# Then we could use the function as we normally would inside the mutate:
df %>% 
  select(where(is.numeric)) %>% 
  mutate(across(c(age,sib_sp,parch,fare), ~scale_this(.x)))

