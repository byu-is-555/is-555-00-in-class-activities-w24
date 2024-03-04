library(tidyverse)


movies <- read_csv('https://www.dropbox.com/scl/fi/pi7nexxuoqnvviwfzwun9/movie_ratings.csv?rlkey=x419gluseq6p8e8xzu12ndfc9&dl=1')

# How do we compare ratings from different ratings sites? (Distribution, how they compare, etc.) 
# Or ratings from critics vs. users?
movies %>% 
  ggplot(aes(x = rating, y = rotten_tomatoes, color = rating, fill = rating )) +
  geom_col()

movies %>% 
  pivot_longer(!rating,
               names_to = 'source',
               values_to = 'percentage') %>% 
  filter(!is.na(percentage)) %>%
  mutate(review_type = if_else(str_detect(source, 'user'), 'Audience','Critic')) %>% 
  ggplot(aes(x = rating, y = percentage, color = rating, fill = rating)) +
  geom_col(position = 'dodge') +
  facet_grid(review_type~source)


movies %>% 
  pivot_longer(!rating,
               names_to = 'source',
               values_to = 'percentage') %>% 
  filter(!is.na(percentage)) %>%
  mutate(review_type = if_else(str_detect(source, 'user'), 'Audience','Critic')) %>% 
  ggplot(aes(x = rating, y = percentage, color = rating, fill = rating)) +
  geom_col(position = 'dodge') +
  facet_grid(~review_type)  
  
movies %>% 
  pivot_longer(!rating,
               names_to = 'source',
               values_to = 'percentage') %>% 
  filter(!is.na(percentage)) %>%
  mutate(review_type = if_else(str_detect(source, 'user'), 'Audience','Critic'))
  
  
movies_start





steak <- read_csv('https://www.dropbox.com/scl/fi/mzg5oxenh9oonbwpwgxzm/steak_data.csv?rlkey=2gbf1kfqfkln0zf2alwo32nza&dl=1')


# Here's a dumb chart:
steak %>% 
  ggplot(aes(x = steak_prep, fill = educ)) +
    geom_bar() +
    # facet_grid(educ ~ hhold_income) +
    labs(title = "Steak Preparation Preference by Education Level",
         x = "Steak Preparation",
         y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# What else can you find? What other variables are in the dataset that might 
# shed some light on steak-eaters' preferences?
