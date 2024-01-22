library(tidyverse)

raw <- read_csv('https://www.dropbox.com/scl/fi/ug8tbxsdd2qtsfqwnnox1/dollar_store.csv?rlkey=fu36g6uhfpx8u644d1rpsq11i&dl=1')

ds <- janitor::clean_names(raw)

library(lubridate)

#Parsing
ds %>% 
  select(date_added, first_sold_day, first_sold_time) %>% 
  mutate(date_added_c = dmy(date_added)) %>% 
  mutate(first_together = str_c(first_sold_day, first_sold_time, sep=" ")) %>% 
  mutate(first_dt = mdy_hms(first_together),
         hour = hour(first_dt),
         wday = wday(first_dt, label = T),
         month = month(first_dt, label = T))


ds %>% 
  select(unit_size, price) %>% 
  mutate(price_c = parse_number(price),
         unit_size_c = parse_number(unit_size)) #results in an error because the unit size has some string values saying the unit size is not available

#str manipulation
str_detect() #similar to the like command in SQL
str_remove() #removes what you specify
str_extract() #pulls out what matches the given parameter
str_replace() #replaces what you specify with what you say to replace it with


#str_detect
ds %>% 
  select(unit_size, price) %>% 
  filter(str_detect(unit_size, 'each|ounce')) #uses a regular expression


#str_extract
ds %>% 
  select(unit_size, price) %>% 
  mutate(unit_type = str_extract(unit_size, 'each|ounce')) %>% 
  count(unit_type)

ds %>% 
  select(unit_size, price) %>% 
  mutate(unit_type = str_extract(unit_size, 'each|ounce')) %>% 
  filter(is.na(unit_type))


#str_remove/str_remove_all
ds %>% 
  select(product_info) %>% 
  mutate(product_info = str_remove(product_info, 'Brand:'))

ds %>% 
  select(product_info) %>% 
  mutate(product_info = str_remove_all(product_info, 'Brand:|Product:'))


#str_replace/str_replace_all
ds %>% 
  select(product_info) %>% 
  mutate(product_info = str_replace(product_info, 'Brand:', 'replace_me'))

ds %>% 
  select(product_info) %>% 
  mutate(product_info = str_replace_all(product_info, 'Brand:|Product:', 'replace_me'))


#Separate
#this gives an error because there are two instances of the delim in one row and it is complaining
ds %>% 
  select(product_info) %>% 
  separate_wider_delim(product_info, 
                       delim = ' - ', 
                       names = c('brand', 'product'))

#fix the multi-delim instances - be explicit
#drop: deletes whats to the right of the second and more instances of the delim
#merge: just cuts out the additioanl delims
ds %>% 
  select(product_info) %>% 
  separate_wider_delim(product_info, 
                       delim = ' - ', 
                       names = c('brand', 'product'),
                       too_many = "merge")









# Convert to number formats: price, star_rating, review_count, stock_status, unit_size
#   Goals:   (1) Don't lose information
#            (2) Missing where appropriate, but only where appropriate



# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Convert date columns to date and/or date-time columns, then calculate how many
# days each product sat on the shelf before its first sale.


