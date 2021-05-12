library(tidyverse)
library(dslabs)
library(ggrepel)
library(RColorBrewer)

## 5.1 ====

# Encoding Data Using Visual Cues ====

poll_browser <- data.frame(browser = c("Opera", "Safari", "Firefox", "Chrome", "IE"), 
                           poll_2000 = c(3, 21, 23, 26, 28), 
                           poll_2015 = c(2, 22, 21, 29, 27))

poll_browser <- poll_browser %>% 
  pivot_longer(cols = -browser, 
               names_to = "year", 
               values_to = "votes") %>% 
  arrange(year)

poll_browser %>%
  pivot_wider(names_from = year, 
              values_from = votes)

poll_browser %>%
  mutate(browser = reorder(browser, votes)) %>% 
  ggplot(mapping = aes(x = browser, 
                       y = votes)) + 
    geom_col(fill = rep(blues9[3:7], 2), 
             color = "black", 
             width = 0.5) +
    labs(x = "Browser", 
         y = "Votes") + 
    facet_wrap(. ~ year) + 
    theme_bw()

# Bar plots represent visual cues as position and length. 
# Humans are good at visually quantifying linear measures, 
# making bar plots a strong alternative to pie or donut charts.

# know when to include zero ====

# When using bar plots, always start at 0. 
# It is deceptive not to start at 0 because bar plots imply length is proportional to the quantity displayed. 
# Cutting off the y-axis can make differences look bigger than they actually are.

# When using position rather than length, it is not necessary to include 0 (scatterplot, dot plot, boxplot).
# Do Not Distort Quantities ====

# Make sure your visualizations encode the correct quantities.
# For example, if you are using a plot that relies on circle area, 
# make sure the area (rather than the radius) is proportional to the quantity.
# Order by a meaningful value ====

# Use reorder() function to reorganize factor levels according a meaningful
# value from other numeric variable inside dataset

# The default ordering for categories is alphabetical if the categories are strings or by factor level if factors. 
# However, we rarely want alphabetical order.







## 5.2 ====

# Show the data ====

data("heights")

heights %>% 
  ggplot(aes(x = sex, 
             y = height)) +
  #geom_point() + 
  geom_jitter(width = 0.1, # new
              alpha = 0.3) + 
  labs(x = "Genre",
       y = "Height (inches)") + 
  theme_bw()

# Ease comparison: use common axes ====

heights %>% 
  ggplot(aes(x = height, 
             fill = sex)) + 
    geom_histogram(alpha = 0.3, 
                   binwidth = 1, 
                   col = "black") +
    labs(x = "Height (inches)",
         y = "Frequency") +
    scale_fill_discrete(name = "Genre") + 
    scale_x_continuous(breaks = seq(50, 90, 5)) + 
    theme_bw()

heights %>% 
  ggplot(aes(x = height,
             y =..density.., # new
             fill = sex)) +
    geom_histogram(binwidth = 1, 
                   col = "black", 
                   show.legend = FALSE) +
    labs(x = "Height (inches)",
         y = "Frequency") + 
    scale_x_continuous(breaks = seq(50, 85, 5)) + 
    facet_grid(sex ~ .) + 
    theme_bw()

# Ease comparisons by keeping axes the same when comparing data across multiple plots.
# Align plots vertically to see horizontal changes (histograms). 
# Align plots horizontally to see vertical changes (boxplots).
# Bar plots are useful for showing one number but not useful for showing distributions.

# Consider transformations ====

# The log transformation is useful for data with multiplicative changes. 
# The logistic transformation is useful for fold changes in odds. 
# The square root transformation is useful for count data.
# Ease comparisons: compared visual cues should be adjacent ====

# When two groups are to be compared, it is optimal to place them adjacent in the plot.
# Use color to encode groups to be compared.
# Consider using a color blind friendly palette like the one in this video.



## 5.3 ====

# Slope charts and Bland-Altman plot ====

data("gapminder")

west <- c("Western Europe", "Northern Europe", "Southern Europe", 
          "Northern America", "Australia and New Zealand") 

data <- gapminder %>% 
  filter(year %in% c(2010, 2015), 
         region %in% west, 
         !is.na(life_expectancy), 
         population > 10^7)

data %>% 
  mutate(location = ifelse(year == 2010, 1, 2), 
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"), location  + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x = year, 
             y = life_expectancy, 
             group = country)) + 
    geom_line(aes(color = country),
              show.legend = FALSE) + 
    geom_text(aes(x = location, 
                  label = country,
                  hjust = hjust), 
              show.legend = FALSE) + 
    labs(title = "Slope Chart",
         x = "Year", 
         y = "Life Expectancy") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) # new: justify title plot

# Scatter plot on this case

data %>% 
  select(country, year, life_expectancy) %>% 
  pivot_wider(names_from = year, 
              values_from = life_expectancy) %>% 
  ggplot(aes(x = `2010`, 
             y = `2015`, 
             label = country)) + 
    geom_point() + 
    geom_text_repel(size = 3.5) + 
    geom_abline(intercept = 0, 
                lty = 2) + 
    scale_x_continuous(limits = c(78, 83),
                       breaks = seq(78, 83, 1)) + 
    scale_y_continuous(limits = c(78, 83),
                       breaks = seq(78, 83, 1)) + 
    theme_bw()

# Bland-Altman plot also known as the Tukey Mean Different plot and also MA plot 

spread() = pivot_wider()
gather() = pivot_longer()

data %>% 
  mutate(year = paste0("life_expectancy_", year)) %>% 
  pivot_wider(country,
              names_from = year, 
              values_from = life_expectancy) %>% 
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2, 
         difference = life_expectancy_2015 - life_expectancy_2010) %>% 
  ggplot(aes(x = average,
             y = difference, 
             label = country)) + 
    geom_point() + 
    geom_text_repel() + 
    labs(title = "Bland-Altman Plot",
         x = "Average",
         y = "Difference") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

# Consider using a slope chart or Bland-Altman plot when comparing one variable
# at two different time points, specially for a small number of observations.

# The Bland-Altman plot (Tukey Mean Difference Plot, MA plot), graphs the difference between conditions on 
# y-axis and the mean between conditions on x-axis. It is appropriate for large number of observations than slope charts. 
# Encoding a Third Variable ====

# Encode a categorical third variable on a scatter plot using color or shape.
# Use the shape argument to control shape.
# Encode a continuous third variable on a using color intensity or size.

# Case study: Vaccines

data("us_contagious_diseases")
str(us_contagious_diseases)

data_us_contagious <- us_contagious_diseases %>% 
  filter(disease == "Measles", 
         !state %in% c("Alaska", "Hawaii")) %>% 
  mutate(rate = count/population*10^4) %>% 
  mutate(state = reorder(state, rate))

data_us_contagious %>% 
  filter(state == "California") %>% 
  ggplot(aes(x = year,
             y = rate)) + 
    geom_line() + 
    geom_vline(xintercept = 1963, 
               col = "blue") + 
    labs(x = "Year", 
         y = "Cases per 10 000") + 
    theme_bw()

display.brewer.all(type = "seq")
display.brewer.all(type = "div")

data_us_contagious %>% 
  ggplot(aes(x = year,
             y = state, 
             fill = rate)) +
    geom_tile(color = "grey50") + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_fill_gradientn(name = "Rate",
                         colors = brewer.pal(n = 9, 
                                             name = "Reds"), 
                        trans = "sqrt") + 
    geom_vline(xintercept = 1963, 
               color = "blue") + 
    theme_minimal() + 
    theme(panel.grid = element_blank(), 
          plot.title = element_text(hjust = 0.5)) + 
    labs(title = "Measles", 
         x = "", 
         y ="")

avg_USA <- us_contagious_diseases %>% 
  filter(disease == "Measles") %>% 
  group_by(year) %>% 
  summarize(us_rate = sum(count, na.rm = T)/sum(population, na.rm = T)*10^4)

data_us_contagious %>% 
  filter(!is.na(rate)) %>% 
  ggplot() + 
    geom_line(aes(x = year, 
                  y = rate, 
                  group = state), 
              col = "grey50", 
              alpha = 0.2, 
              size = 1, 
              show.legend = FALSE) + 
    geom_line(aes(x = year, 
                  y = us_rate), 
              data = avg_USA, 
              size = 1, 
              color = "black") + 
    scale_y_continuous(trans = "sqrt",
                       breaks = c(5, 25, 125, 300)) +
    labs(title = "Measles Cases per 10 000 by state",
         x = "Year", 
         y = "Rate") + 
    geom_text(aes(x = 1950,
                  y = 50, 
                  label = "US average"), 
              color = "black", 
              size = 3.5) + 
    geom_vline(xintercept = 1963, 
               color = "blue") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5))

# Position and length are stronger cues than color for numeric values, 
# but color can be appropriate sometimes.

# Avoid Pseudo and Gratuitous 3D Plots ====

# In general, pseudo 3D plots and gratuitous 3D plots only add confusion.
# Use regular 2D plots instead.
# Avoid too many significant digits ====

# In tables, avoid using too many significant digits. Too many digits can distract from the meaning of your data.

options(digits = n) # how changing how many significant digits displays


## Titanic Exercise ====

library(titanic)

titanic <- titanic_train %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>% 
  mutate(Survived = factor(Survived, 
                           labels = c("No", "Si")), 
         Sex = factor(Sex), 
         Pclass = factor(Pclass))

# Assignment 2

titanic %>% 
  filter(!is.na(Age)) %>% 
  ggplot(mapping = aes(x = Age, 
                       y = ..count.., 
                       fill = Sex, 
                       color = Sex)) + 
    geom_density(alpha = 0.2) + 
    #scale_fill_discrete(labels = c("Female", "Male")) + 
    #facet_grid(Sex ~ .) + 
    labs(y = "Density") + 
    theme_bw()

titanic %>% 
  group_by(Sex) %>% 
  summarize(total = n()) %>%
  mutate(count = titanic %>% 
                 filter(Age %in% c(18:35)) %>% 
                 group_by(Sex) %>% 
                 summarize(n()) %>% 
                 select(`n()`), 
         proportion = round(count/total, 3))

# Assignment 3

titanic %>% 
  filter(!is.na(Age)) %>% 
  ggplot(mapping = aes(sample = Age)) +
    geom_qq(dparams = list(mean(titanic$Age, na.rm = TRUE), 
                           sd(titanic$Age, na.rm = TRUE)), 
            size = 1) + 
    geom_abline() + 
    labs(x = "Theoretical", 
         y = "Sample") + 
    theme_bw()

# Assignment 4 

titanic %>%
  ggplot(mapping = aes(x = Survived, 
                       fill = Sex)) + 
    geom_bar(width = 0.7, 
             color = "black",
             position = "dodge") +
    scale_fill_discrete(labels = c("Female", "Male")) + 
    scale_y_continuous(breaks = seq(0, 600, 50)) + 
    labs(y = "Count") + 
    theme_bw()

titanic %>% 
  mutate(Sex = factor(Sex, 
                      labels = c("Female", "Male"))) %>% 
  ggplot(mapping = aes(x = Sex, 
                       fill = Survived)) + 
    geom_bar(color = "black", 
             position = position_dodge()) +
    labs(x = "Genre", 
         y = "Count") + 
    theme_bw()

# Assignment 5

titanic %>% 
  drop_na(Age) %>% 
  ggplot(mapping = aes(x = Age,
                       y = ..count..,
                       fill = Survived, 
                       color = Survived)) +
    geom_density(alpha = 0.2, 
                 show.legend = F) + 
    scale_x_continuous(breaks = c(0, 8, 10, 18, 30, 50, 70, 80)) +
    scale_fill_discrete(name = "Survived") +
    #scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac")) + 
    guides(color = FALSE) + # Remove duplicate legend
    labs(x = "Age", 
         y = "Density") + 
    facet_grid(Survived ~ .) + 
    theme_bw()

died <- titanic %>%
  drop_na(Age) %>% 
  filter(Survived == "No") %>% 
  mutate(rango = cut(Age, breaks = c(0, 8, 10, 18, 30, 50, 70, 80, 100))) %>% 
  group_by(rango) %>% 
  summarize(died = n())

inner_join(total, died, by = "rango") %>% 
  mutate(proportion = died/total)

# Assignment 6

titanic %>% 
  filter(!Fare == 0) %>% 
  ggplot(mapping = aes(x = Survived, 
                       y = Fare)) + 
    geom_boxplot() + 
    stat_summary(fun = mean) + 
    geom_jitter(alpha = 0.2) +
    scale_y_continuous(trans = "log2")

# Assignment 7

titanic %>% 
  ggplot(mapping = aes(x = Survived, 
                       fill = Pclass)) + 
    geom_bar(position = position_fill()) 

# Assignment 8

titanic %>% 
  drop_na(Age) %>% 
  ggplot(aes(x = Age,
             y = ..count..,
             fill = Survived)) +
    geom_density(alpha = 0.2, position = "stack") + 
    facet_grid(Sex ~ Pclass) + 
    theme_bw()
           