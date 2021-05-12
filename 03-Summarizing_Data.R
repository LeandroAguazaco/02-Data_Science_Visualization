library(tidyverse)
library(dslabs)

data("heights")

s <- heights %>% 
  filter(sex == "Male") %>% 
  summarize(average = mean(height), 
            sd = sd(height))

data("murders")

murders <- murders %>%
  mutate(murder_rate =  total / population * 10^5) # Create a new variable inside dataset

# Summarize ====

# Summarize function always return unique value inside a new data frame

summarize(murders, mean(murder_rate))

us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>% 
  .$rate # equivalent to pull(rate)

is.data.frame(us_murder_rate)

# Group_by ====

class_avg <- heights %>% 
  group_by(sex) %>%  # grouping by a categorical variable
  summarize(avg = mean(height), 
            sd = sd(height))

murder_by_region <- murders %>% 
  group_by(region) %>% 
  summarize(avg_rate = mean(murder_rate), 
            median_rate = median(murder_rate))

# Sorting data table  ====

# The arrange function is used

murders %>% arrange(population) %>% # Ascending order
  head()

murders %>%  arrange(desc(population)) %>% # Descending order
  head()

# Order according values of two or more attributes

murders %>% arrange(region, desc(murder_rate)) %>% 
  head()

# Selecting top n ====

murders %>% top_n(10, murder_rate)

murders %>% arrange(desc(murder_rate)) %>% 
  top_n(10)

# slice function, current alternative to select n number of rows

murders %>% slice_max(order_by = murder_rate, 
                      n = 10)



