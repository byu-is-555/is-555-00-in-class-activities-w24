library(tidyverse)
library(moments)
library(lubridate)

df <- tribble(
  ~age, ~gender, ~hr1, ~hr2, ~hr3,
  29,  'male', 98.1, 110, 76,
  55,  'female', 78, 120, 87,
  65,  'male', 65,  129, 77,
  18,  'female', 64, 141, 59
)
df

df <- df %>%
  mutate(hr_mean = (hr1 + hr2 + hr3)/3,
         age_sqrt = sqrt(age))

df


df %>%
  # sample_n(1000) %>%
  mutate(hr1_sq = hr1^(1/2),
         hr1_cu = hr1^(1/3),
         hr1_ln = log(hr1)) %>%
  select(starts_with('hr1')) %>%
  skewness()

df <- df %>%
  mutate(hr1_ln = log(hr1))




df_new <- df %>%
  mutate(gender = ifelse(gender == 'male', 1, 0))
# OR:
df_new <- df %>%
  mutate(gender = recode(gender,
                         male = 1,
                         female = 0))
df_new



df <- df %>%
  mutate(signup_date = c('02/14/2019', '01/05/2018', '05/23/2020', '12/10/2019'),
         complete_date = c('02/27/2020', '10/22/2020', '09/11/2021', '07/05/2022'))



df <- df %>%
  mutate(signup_date = mdy(signup_date),
         complete_date = mdy(complete_date),
         days_since_signup = ymd("2023-1-1") - signup_date,
         days_to_completion = complete_date - signup_date,
         years_to_completion = days_to_completion/365,
         weeks_to_completion = days_to_completion/7,
         months_to_completion = days_to_completion/(365/12))







cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

cost2 <- cost %>%
  select(name, type, degree_length, in_state_tuition)

income <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv')


income2 <- income %>%
    select(name, total_price, year) %>%
    group_by(name, year) %>%
    summarise(avg_per_year = mean(total_price, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(name, year)




# see which university has the most consistently increasing tuition
income3 <- income2 %>%
    group_by(name) %>%
    # get difference from year to year for every university
    mutate(years_of_data = n(),
           tuition_increase = c(NA, diff(avg_per_year))) %>%
    # filter out universities where tuition was not increasing over time
    filter(tuition_increase > 0 | is.na(tuition_increase)) %>%
    # after filtering out universities count again
    mutate(years_with_tuition_increase = n()) %>%
    # compare counts to first ones and filter out universities
    # that have not had increasing tuition for every single year
    filter(years_of_data == years_with_tuition_increase)






# We now have the universities who have always raised tuition.
# Which universities are the worst offenders in terms of the amount increased?
# Let's calculate their average yearly increase and sort:
income3 %>%
  group_by(name) %>%
  summarize(ave_yearly_increase = mean(tuition_increase, na.rm = T)) %>%
  arrange(-ave_yearly_increase)



