library(dslabs)
library(tidyverse)

data("heights")
head(heights)

table(heights$sex) # Contingency table, absolute frequencies
prop.table(table(heights$sex)) # Contingency table, relative frequencies

blues9[1:4] # colors palette

data("murders")
levels(murders$region)
murders$region <- factor(murders$region, # Change the order of levels
                         levels = c("North Central", "Northeast", "South", "West"))
levels(murders$region)

table(murders$region)

ggplot(data = murders, 
       aes(x  = region)) + 
  geom_bar(fill =  blues9[3:6], 
           color =  "black") +
  labs(title = "States by regions", 
       x = "US Regions", 
       y = "States") +
  theme_bw()

ggplot(data = heights, 
       mapping = aes(x = height, 
                     fill = sex)) + 
  geom_histogram(bins = 35, 
                 color = "black") + 
  labs(title = "Histogram", 
       x = "Height (in)",  
       y  = "Frequency") + 
  theme_bw()

ggplot(data = heights, 
       mapping = aes(x = height, 
                     fill = sex)) + 
  geom_density(color = "black") + 
  theme_bw()

y <- heights$height[heights$sex == "Male"] # Only male

x <- heights %>%  # Equal than above
  filter(sex == "Male") %>% 
  pull(height)

identical(y, x) # Compares declared objects

tapply(heights$height, heights$sex, mean)
tapply(heights$height, heights$sex, sd)

z <- scale(heights$height) # Standardized

mean(abs(z) < 2) # Operating with booleans, I can obtain proportions

pnorm(q = 70.5, # Distributions function
      mean = mean(x), 
      sd = sd(x), 
      lower.tail = F) 

quantile(x, 0.750)

q <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(x, q)
percentiles

percentiles[names(percentiles) == c("25%", "50%", "75%")] # Quartiles

summary(x) # Five characteristic numbers

qnorm(p = 0.5, mean = mean(x), sd = sd(x)) # Quantile function 
qnorm(p = 0.5, mean = mean(x), sd = sd(x), lower.tail = F)
pnorm(q =  69.31475, mean = mean(x), sd = sd(x))

teoricos <- qnorm(q, mean = 69, sd = 3)

plot(teoricos, percentiles) # QQ-plot
qqplot(teoricos, percentiles)

ggplot(data = NULL,
       aes(x = teoricos, 
           y = percentiles)) + 
  geom_point() +
  labs(title = "QQPLOT", 
       x = "Cuantiles Teóricos", 
       y = "Cuantiles Empíricos") +
  geom_abline() + 
  theme_bw()


