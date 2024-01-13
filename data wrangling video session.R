library(tidyverse)
df <- starwars


# Grouping ----------------------------------------------------------------


# top 5 tallest overall - using slice_head
df %>% 
  arrange(desc(height)) %>% 
  slice_head(n = 5)



# or just using slice_max
df %>% 
  slice_max(mass, n = 5)

# what is the shortest character for each species? 
df %>% 
  group_by(species) %>% 
  slice_max(height)

df %>% 
  group_by(species) %>% 
  summarize(species_count = n())


# Also summarize isn't always the goal what if we want a relative 
# comparison of height for each species? I.e., are you tall for a human?
# calculate tallest for each species without summarizing, 
# compare to species max, show a few of the columns, sort
df %>% 
  select(name, species, height) %>%
  group_by(species) %>% 
  mutate(species_count = n(),
         species_max = max(height, na.rm = T)) %>% 
  mutate(difference_from_max = species_max - height) %>% 
  slice_min(height, n=2) %>% 
  arrange(species, height) %>% 
  print(n=20)


# Grouping by multiple categoricals
# Was is the average birth year for each gender from each homeworld
# This is a simple example, but it groups first by homeworld, 
# then within each homeworld it groups by gender
df %>% 
  group_by(homeworld,gender) %>% 
  summarize(count = n(),
            avg_birth_year = mean(birth_year, na.rm = T))


# Joins -------------------------------------------------------------------

# install.packages('nycflights13')
library(nycflights13)
df_flights <- flights
df_airlines <- airlines
df_planes <- planes



# Get a smaller set of columns that will be easier to see
flights2 <- df_flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

# Join in carrier names, hopefully specifying the key explicitly to avoid surprises
flights2 %>% 
  left_join(df_airlines, by = join_by(carrier))

flights2 %>% 
  left_join(df_airlines)

left_join(flights2, df_airlines)

# Just an example for when the keys aren't identical:
alternative_name <- df_airlines %>% rename(car_code = carrier)


flights2 %>% 
  left_join(alternative_name, by = join_by(carrier == car_code))



# This is a mostly useless question, but we used it to illustrate what happens when naming conflicts occur 
# during a join.
# 
# What is the average "birth year" of planes associated with delayed departures among flights to Atlanta?

# Joining the data to get the "birth year" in creates unexpected results with the two year columns. 
# Notice the mostly empty columns from the `df_planes` tibble... this is because the join is attempting
# to join on both tailnum and year, and the years mean different things
df_flights %>% 
  left_join(df_planes) %>% 
  # filter(!is.na(engine)) %>% 
  glimpse()

# Here's one strategy: just rename the right-side year on the fly to something useful. Name conflict solved.
df_flights %>% 
  left_join(df_planes %>% select(tailnum, year) %>% rename(mfctr_year = year), by = join_by(tailnum)) %>% 
  # filter(!is.na(engine)) %>% 
  glimpse()

# Or you could do it as a separate operation... with more lines of code :(
fixed_planes <- df_planes %>% select(tailnum, year) %>% rename(mfctr_year = year)

df_flights %>% 
  left_join(fixed_planes, by = join_by(tailnum)) %>% 
  # filter(!is.na(engine)) %>% 
  glimpse()

# Finally, the suffix parameter lets you pass an automatic "label" for each side that gets appended in any 
# case where there is a naming conflict.
df_flights %>% 
  left_join(df_planes %>% select(tailnum, year), by = join_by(tailnum), suffix = c("_flight", "_mfctr")) %>% 
  # filter(!is.na(engine)) %>% 
  glimpse()



# Binding data - stacking chunks with bind_rows()

# I'm just manually splitting this data into two tibbles with the same columns and two chunks of the full data:

flights_half1 <- flights %>% 
  slice(1:199388)

flights_half2 <- flights %>% 
  slice(199389:336776)

flights_half1
flights_half2



# I usually like to use bind rows outside a pipe because it can take any number of tibble as parameters, and
# that can get confusing in a piped scenario. (I'm obviously piping it after the bind_rows so we can see what
# it looks like after.)
bind_rows(flights_half1, flights_half2) %>% 
  glimpse


