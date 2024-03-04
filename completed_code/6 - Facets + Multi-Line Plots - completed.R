# Facets and multi-lines --------------------------------------------------------------------------------

tips <- read_csv('https://www.dropbox.com/s/rydxlxdarjdoj7a/tips.csv?dl=1')

# Let's plot tip_percentage vs. total_bill,
# then split that across lots of categories
tips %>% glimpse
tips %>% 
  mutate(tip_perc = tip/total_bill) %>% 
  mutate(bad_smoker_tip = ifelse(smoker == 'Yes', tip*100, tip)) %>%
  # mutate(bad_day_total = ifelse(day %in% c('Sun', 'Sat'), total_bill*1000, total_bill))
  ggplot(aes(x = total_bill, y = bad_smoker_tip, color = sex)) +
  geom_point(size = 3) +
  facet_wrap(~smoker)

# 
tips %>% 
  mutate(tip_perc = tip/total_bill) %>% 
  mutate(bad_smoker_tip = ifelse(smoker == 'Yes', tip*100, tip)) %>%
  mutate(bad_day_total = ifelse(day %in% c('Sun', 'Sat'), total_bill*1000, total_bill)) %>% 
  ggplot(aes(x = bad_day_total, y = bad_smoker_tip, color = as.factor(sex))) +
  geom_point(size = 3) +
  facet_grid(smoker~day, scales = 'free')

tips %>% 
  mutate(bad_smoker_tip = ifelse(smoker == 'Yes', tip*100, tip)) %>% 
  mutate(bad_day_total = ifelse(day %in% c('Sun', 'Sat'), total_bill*1000, total_bill)) %>%
  mutate(tip_perc = tip/total_bill) %>%
  ggplot(aes(x = bad_day_total, y = bad_smoker_tip, color = as.factor(sex))) +
  geom_point(size = 3) +
  facet_grid(smoker~day, scales = 'free')


econ <- read_csv('https://www.dropbox.com/s/8bq9rw0rk46hru2/econ.csv?dl=1')

# Let's plot two measures over time: savings rate & unemployment weeks
# It's easiest if we pivot to make this work
econ %>% 
  select(date, savings_rate, unempl_weeks) %>% 
  # filter(date < lubridate::ymd('1975-01-01')) %>% 
  pivot_longer(!date, 
               names_to = 'Measure', 
               values_to = 'Rate') %>% 
  ggplot(aes(x = date, y = Rate, linetype = Measure)) +
  geom_line(size = 2) +
  facet_wrap(~Measure, ncol=1)
