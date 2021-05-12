library(dslabs)
library(tidyverse)

# 1. Gapminder dataset ====

data("gapminder")
table(gapminder$region)

countries <- list(c("Sri Lanka", "Turkey"), 
                  c("Poland", "South Korea"), 
                  c("Malaysia", "Russia"), 
                  c("Pakistan", "Vietnam"), 
                  c("Thailand", "South Africa"))

countries_pair <- function(countries) {
  gapminder %>% filter(year == 2015 & country %in% countries) %>% 
    select(country, infant_mortality)
}

lapply(countries, countries_pair)

# 2. Life expectancy vs Fertility Rate ====

gapminder %>% 
  filter(year == 1962) %>% 
  ggplot(mapping = aes(x = fertility, 
                       y = life_expectancy, 
                       col = continent)) +
  geom_point() + 
  labs(title = "Life Expectancy vs Fertility Rate", 
       x = "Fertility Rate", 
       y = "Life Expectancy (year)") + 
  scale_color_discrete(name = "Continent") +
  theme_bw()

# 3. Faceting ====
 
  # facet_grip(): plots in the same row, one or two variables
gapminder %>% 
  filter(year %in% c(1962, 2012)) %>% 
  ggplot(mapping = aes(x = fertility, 
                       y = life_expectancy, 
                       col = continent)) +
    geom_point() +
    labs(title = "Life Expectancy vs Fertility Rate (1962 - 2012)",
         x = "Fertility Rate",
         y = "Life Expectancy") +
    scale_color_discrete(name = "Country") + 
    facet_grid(.~ year) + # facet_grid(country ~ year) Use one or two variables to classification
    theme_bw()

  # facet_wrap(): plots by rows and columns, 2d sequence of panels, especially for one variable
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")

gapminder %>% 
  filter(year %in% years & continent %in% continents) %>% 
  ggplot(mapping = aes(x = fertility,
                       y = life_expectancy, 
                       col = continent)) +
    geom_point() +
    labs(title = "Life Expectancy vs Fertility Rate",
         x = "Fertility Rate",
         y = "Life Expectancy") +
    scale_color_discrete(name = "Country") + 
    facet_wrap(~ year) +
    theme_bw()

# 4. Time Series Plots ====

gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>% 
  ggplot(mapping = aes(x = year,
                       y = fertility,
                       col = country)) +
    # geom_point() + 
    geom_line() + 
    labs(title = "Fertility rate across years",
         x = "Year",
         y = "Fertility Rate") + 
    scale_color_discrete(name = "Country") +
    theme_bw()

  # Adding labels instead legends 

labels <- data.frame(country = c("South Korea", "Germany"), 
                     x = c(1975, 1965), 
                     y = c(61, 72))

gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>% 
    ggplot(mapping = aes(x = year,
                         y = life_expectancy,
                         col = country)) +
    # geom_point() + 
    geom_line() + 
    labs(title = "Life Expectancy",
         x = "Year",
         y = "Life Expectancy") + 
    geom_text(data = labels, 
              mapping = aes(x = x, 
                            y = y, 
                            label = country), 
              size = 3.5) +
    theme_bw() + 
    theme(legend.position = "none")

# 5. Transformations, Log transformations ====

gapminder <- gapminder %>% 
  mutate(dollar_per_day = gdp/population/365)

gapminder %>% 
  filter(year == 1970, 
         !is.na(dollar_per_day)) %>% 
  ggplot(mapping = aes(x = log2(dollar_per_day))) + 
    geom_histogram(binwidth = 1, 
                   col = "black")

# Using base 2 for example, means that every time a value doubles,
# the log transformation increases by one. The variable scale ranges between few units, example from one to fifty.

# ¿How we choose the base? Depends on the variable scale. In general, we do not recommend using the natural log
# for data exploration and visualization because it's no easy to calculate.

# In base 10, this turns to a range that includes very few integers. The variable scale ranges between thousands to millions.

gapminder %>% 
  filter(year == 1970, 
         !is.na(dollar_per_day)) %>% 
    ggplot(mapping = aes(x = log10(population))) + 
    geom_histogram(binwidth = 1, 
                   col = "black")

# If we log the data, we can more easily interpret intermediate values in the scale.

gapminder %>% 
  filter(year == 1970, 
         !is.na(dollar_per_day)) %>% 
  ggplot(mapping = aes(x =  dollar_per_day)) + 
  geom_histogram(binwidth = 1, 
                 col = "black") + 
  scale_x_continuous(trans = "log2")

# There are other transformations available through the trans argument. 
# As we learn later on, the square root (sqrt) transformation is useful when considering counts. 
# The logistic transformation (logit) is useful when plotting proportions between 0 and 1. 
# The reverse transformation is useful when we want smaller values to be on the right or on top.

# 6. Stratify and Boxplot ====

length(levels(gapminder$region))

gapminder %>% 
  filter(year == 1970, 
         !is.na(gdp)) %>% 
  ggplot(mapping = aes(x = region, 
                       y = dollar_per_day)) +
    geom_boxplot() + 
    labs(x = "Region",
         y = "Dollars per Day") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1)) # Change from horizontal to vertical text in the x axis tags

# reorder() function lets change the order of levels of a factor variable based on a summary computed on a numeric vector

fact <- factor(c("Asia", "Asia", rep("West", 3)))
levels(fact) # By default, factor organize alphabetically
value <- c(10:12, 6, 4)

fact <- reorder(x = fact, 
                X = value, 
                FUN = mean)
levels(fact)

# Reorder the regions by their median income level

gapminder %>% 
  filter(year == 1970) %>% 
  drop_na(gdp) %>% 
  mutate(region = reorder(region, 
                          dollar_per_day, 
                          median)) %>% 
  ggplot(mapping = aes(x = region, 
                       y = dollar_per_day, 
                       fill = continent)) + 
    geom_boxplot() + 
    scale_fill_discrete(name = "Continent") + # Change the legend title and the order of levels
    labs(x = "Region", 
         y = "Dollar per Day") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1))

# Changing the axis x scale for better understanding

gapminder %>% 
  filter(year == 1970) %>% 
  drop_na(gdp) %>% 
  mutate(region = reorder(region, 
                          dollar_per_day, 
                          FUN = median)) %>% 
  ggplot(mapping = aes(x = region, 
                       y = dollar_per_day, 
                       fill = continent)) + 
    geom_boxplot() + 
    geom_jitter(show.legend = FALSE, 
                alpha = 0.2) + 
    labs(x = "Region", 
         y = "Income: Dollar per Day") +
    scale_fill_discrete(name = "Continent") +
    scale_y_continuous(trans = "log2") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust = 1))


# 7. Comparing Distributions ====

west <- c("Western Europe", 
          "Northern Europe", 
          "Southern Europe", 
          "Northern America", 
          "Australia and New Zealand")

gapminder %>% 
  filter(year %in% c(1970, 2010)) %>% 
  drop_na(gdp) %>% 
  mutate(group = ifelse(region %in% west,
                        yes =  "West", 
                        no = "Developing")) %>% 
  ggplot(mapping = aes(x = dollar_per_day, 
                       fill = group)) + 
    geom_histogram(binwidth = 1,
                   color = "black") +
    labs(x = "Income: dollars/day",
         y = "Frequency") + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(year ~ group) + 
    theme_bw() + 
    theme(legend.position = "none")

# Select countries with information for both 1970 and 2010

country_list_1 <- gapminder %>% 
  filter(year == 1970,
         !is.na(gdp)) %>% 
  pull(country)

country_list_2 <- gapminder %>% 
  filter(year ==  2010) %>% 
  drop_na(gdp) %>% 
  .$country

country_list <- intersect(country_list_1, country_list_2)

# Comparison alternative 1

gapminder %>% 
  filter(year %in% c(1970, 2010),
         country %in% country_list) %>% 
  drop_na(gdp) %>% 
  mutate(group = ifelse(region %in% west, 
                        yes = "West", 
                        no = "Developin")) %>% 
  ggplot(mapping = aes(x = dollar_per_day,
                       fill = group)) +
    geom_histogram(binwidth = 1, 
                   color = "black") +
    labs(x = "Income: dollars/day", 
         y = "Frequency") + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(year ~ group) + 
    theme_bw() + 
    theme(legend.position = "none")

gapminder %>% 
  filter(year %in% c(1970, 2010), 
         country %in% country_list) %>% 
  drop_na(gdp) %>% 
  mutate(region = reorder(region, 
                          dollar_per_day, 
                          FUN = median)) %>% 
  ggplot(mapping = aes(x = region, 
                       y = dollar_per_day,
                       fill = continent)) +
    geom_boxplot() + 
    scale_fill_discrete(name = "Continent") + 
    labs(x = "Region", 
         y = "Income: dollar/day") +
    scale_y_continuous(trans = "log2") + 
    theme_bw() + 
    theme(axis.text.x = element_text(hjust = 1, 
                                     angle = 90)) +
    facet_grid(year ~ ., 
               scales = "free")

# Comparison alternative 2

gapminder %>% 
  filter(year %in% c(1970, 2010), 
         country %in% country_list) %>% 
  drop_na(gdp) %>% 
  mutate(region = reorder(region, 
                          dollar_per_day, 
                          FUN = median)) %>% 
  ggplot(mapping = aes(x = region, 
                       y = dollar_per_day,
                       fill = factor(year))) +
    geom_boxplot() + 
    scale_fill_discrete(name = "Year") + 
    labs(x = "Region", 
         y = "Income: dollar/day") +
    scale_y_continuous(trans = "log2") + 
    theme_bw() + 
    theme(axis.text.x = element_text(hjust = 1, 
                                     angle = 90)) 

# 8. Density Plots ====

# Density plot

gapminder %>% 
  filter(year %in% c(1970, 2010), 
         country %in% country_list) %>% 
  drop_na(gdp) %>% 
  ggplot(mapping = aes(x = dollar_per_day)) +
    geom_density(fill = "gray") + 
    labs(x = "Income: dollars/day",
         y = "Density") + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(year ~ .) + 
    theme_bw()

# Counting the number of countries in each group

gapminder %>% 
  filter(year == 1970,
         country %in% country_list) %>% 
  mutate(group = ifelse(region %in% west,
                        yes = "West", 
                        no = "Developing")) %>% 
  group_by(group) %>% 
  summarize(n = n()) %>% 
  knitr::kable()

# Overlapping groups in the density plot

gapminder %>% 
  filter(year %in% c(1970, 2010),
         country %in% country_list) %>% 
  drop_na(gdp) %>% 
  mutate(group = ifelse(region %in% west,
                        yes = "West", 
                        no = "Developing")) %>% 
  ggplot(mapping = aes(x = dollar_per_day, 
                       fill = group)) + 
    geom_density(alpha = 0.5) + 
    labs(x = "Income: dollar/day", 
         y = "Density") +
    scale_x_continuous(trans = "log2") +
    theme_bw() + 
    facet_grid(year ~ .)

# Correcting density plot due the difference between groups size

gapminder %>% 
  filter(year %in% c(1970, 2010), 
         country %in% country_list) %>% 
  drop_na(gdp) %>% 
  mutate(group = ifelse(region %in% west,
                        yes = "West", 
                        no = "Developing")) %>% 
  ggplot(mapping = aes(x = dollar_per_day, 
                       y = ..count.., 
                       fill = group)) + 
    geom_density(alpha = 0.5, 
                 bw = 0.75) + 
    labs(x = "Income: dollars/day", 
         y = "Density") + 
    scale_fill_discrete(name = "Group") +
    scale_x_continuous(trans = "log2") +
    facet_grid(year ~ .) + 
    theme_bw()

# case_when function 

gapminder <- gapminder %>% 
  mutate(group = case_when(.$region %in% west ~ "West",
                           .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia", 
                           .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
                           .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Sahara Africa", 
                           TRUE ~ "Others"))

gapminder <- gapminder %>% 
  mutate(Group = factor(group, 
                        levels = c("Others", 
                                   "Latin America", 
                                   "East Asia", 
                                   "Sub-Sahara Africa", 
                                   "West"))) 

gapminder %>% 
  filter(year %in% c(1970, 2010), 
         country %in% country_list, 
         !is.na(gdp)) %>% 
  ggplot(mapping = aes(x = dollar_per_day, 
                       y = ..count.., 
                       fill = Group, 
                       col = Group)) +
    geom_density(alpha = 0.2, 
                 bw = 0.75, 
                 lwd = 0.2, # new
                 show.legend = T, 
                 position = "stack") + # new
    scale_x_continuous(trans = "log2") +
    # scale_color_discrete(name = "Group") +
    labs(x = "Income: Dollars/Day", 
         y = "Count") +
    facet_grid(year ~ .) +
    theme_bw()

# weight argument

gapminder %>% 
  filter(year %in% c(1970, 2010), 
         country %in% country_list, 
         !is.na(gdp)) %>% 
  group_by(year) %>% 
  mutate(weight = population/sum(population)*2) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = dollar_per_day, 
                       #y = ..count.., 
                       fill = Group, 
                       col = Group, 
                       weight = weight)) + # new
    geom_density(alpha = 0.2, 
                 bw = 0.75, 
                 lwd = 0.2,
                 show.legend = T, 
                 position = "stack") +
    scale_x_continuous(trans = "log2") +
    # scale_color_discrete(name = "Group") +
    labs(x = "Income: Dollars/Day", 
         y = "Count") +
    facet_grid(year ~ .) +
    theme_bw()

# 9. Ecological Fallacy ====

gapminder <- gapminder %>% 
  mutate(group = case_when(.$region %in% west ~ "The West", 
                           .$region %in% "Northern Africa" ~ "Northern Africa", 
                           .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
                           .$region == "Southern Asia" ~ "Southern Asia", 
                           .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
                           .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Sahara Africa",
                           .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income <- gapminder %>% 
  filter(year == 2010) %>% 
  drop_na(gdp, infant_mortality, group) %>% 
  group_by(group) %>% 
  summarize(income = sum(gdp)/sum(population)/365, 
            infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population)) %>% 
  arrange(income)

surv_income %>% 
  ggplot(mapping = aes(x = income, 
                       y = infant_survival_rate, 
                       label = group, 
                       col = group)) +
    geom_point(show.legend = F) + 
    geom_label(nudge_x = 0.5, 
               size = 3,
               show.legend = F) + 
    scale_x_continuous(trans = "log2", 
                       limits = c(0.25, 150)) + # new
    scale_y_continuous(trans = "logit", # odd concept
                       limits = c(0.875, 0.9981), 
                       breaks = c(0.85, 0.90, 0.95, 0.998)) + # new
    labs(x = "Income", 
         y = "Infant Survival Rate") +
    theme_bw()

gapminder <- gapminder %>%
  mutate(infant_survival_rate = 1-infant_mortality/1000*population/population)

gapminder %>% 
  filter(year == 2010) %>%
  drop_na(dollar_per_day, infant_survival_rate, group)  %>%  
  ggplot(mapping = aes(x = dollar_per_day,
                       y = infant_survival_rate, 
                       color = group, 
                       label = country)) +
    geom_point(size = 2, 
               alpha = 0.4) + 
    geom_text(data = filter(gapminder, gapminder$country %in% c("Singapore", "Sweden", 
                                                                "United Staets", "Chile", 
                                                                "Colombia", "Mauritius", 
                                                                "Tunisia", "Serbia", 
                                                                "Bolivia", "Botswana", 
                                                                "Angola", "Sudan",
                                                                "Cambodia", "Sierra Leone",
                                                                "Haiti"), 
                                       year == 2010) %>% 
                     select(country, dollar_per_day, infant_survival_rate, group),
              show.legend = F, 
              size = 3, 
              nudge_x = 0.2) + 
    scale_color_discrete(name = "Group") +
    scale_x_continuous(trans = "log2", 
                       limits = c(0.25, 150)) +
    scale_y_continuous(trans = "logit", 
                       limits = c(0.875, 0.9981), 
                       breaks = c(0.85, 0.90, 0.95, 0.998)) +
    labs(x = "Income", 
         y = "Infant Survival Rate") +
    theme_bw()

