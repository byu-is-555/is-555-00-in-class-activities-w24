library(tidyverse)

df <- read_csv('https://www.dropbox.com/scl/fi/a37h3rf9rmi7yw9pg4n16/titanic_day2.csv?rlkey=xgd4i14bei6gikmg6tbhdqu9u&dl=1')


# Outlier Treatments ------------------------------------------------------------------------------------


# Pass the four columns to summary() to check means, maxes
outlier_candidates <- c('age', 'sib_sp', 'parch', 'fare')


# calculate extreme threshold caps based on 99th percentile
df %>% 
  select(all_of(outlier_candidates)) %>% 
  summary()

df %>% 
  select(all_of(outlier_candidates)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(y = value, fill = name)) +
  geom_boxplot() +
  facet_wrap(~name, nrow = 1, scales = 'free_y')+
  theme_bw()
  

age_cap <- quantile(df$age, .99)
# sib_sp_cap <- quantile(df$sib_sp, .99)
# parch_cap <- quantile(df$parch, .99)
fare_cap <- quantile(df$fare, .99)

age_cap <- quantile(pull(df, age), .99)
# sib_sp_cap <- quantile(pull(df, sib_sp), .99)
# parch_cap <- quantile(pull(df, parch), .99)
fare_cap <- quantile(pull(df, fare), .99)



# Now check how many are beyond the percentile caps
df %>% 
  summarize(fare_over_cap = sum(fare > fare_cap))


# cap age and fare, and check work before saving

df %>% 
  mutate(fare = if_else(fare > fare_cap, fare_cap, fare))


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

df %>% 
  select(age, fare) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  facet_wrap(~name, ncol = 1, scales = 'free')

  
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
  ) %>% 
  select(starts_with('fare_')) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  facet_wrap(~name, ncol = 1, scales = 'free')

# now let's visualize the effect of the transformations to see 
# which one makes sense.




# Note here that we need to add one to the fare before applying the power transformation. Zero raised to the
# power of -1/2 is not a number. Sometimes, to make a transformation work, we have to protect the transformation
# from the possible math errors. In this case, a horizontal shift of 1 does not affect the distribution and
# the column is eventually going to be standardized anyway. 
# (This is what caused the errors we saw in class...)
df %>% count(fare)

0^(-1/2)

df %>% 
  mutate(fare = (fare)^-(1/2)) %>% 
  count(fare) %>% print(n=300)

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

