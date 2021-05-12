library(tidyverse)
library(dslabs)
library(ggthemes)
library(ggrepel)

data("murders")

murders$region <- factor(murders$region, # Change the order of levels
                         levels = c("North Central", "Northeast", "South", "West"))

r <- murders %>% # F
  summarize(rate = sum(total) / sum(population) * 10^6) %>% 
  .$rate # == pull()

# Tex, aesthetic
ggplot(data = murders, 
       mapping = aes(x = population/10^6, 
                     y = total, 
                     label = abb)) + 
  geom_point(size = 2) + 
  geom_label(nudge_x = 0.7) 

# Scale
ggplot(data = murders, 
       mapping = aes(x = population/10^6, 
                     y = total, 
                     label = abb)) + 
  geom_point(size = 2) + 
  geom_text(nudge_x = 0.03) + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10")

ggplot(data = murders, 
       mapping = aes(x = population/10^6, 
                     y = total, 
                     label = abb)) + 
  geom_point(size = 2) + 
  geom_text(nudge_x = 0.03) + 
  scale_x_log10() + 
  scale_y_log10()

# Labels, colors by categorical variable 
ggplot(data = murders, 
       mapping = aes(x = population/10^6, 
                     y = total, 
                     label = abb, 
                     col = region)) + 
  geom_point(size = 2) + 
  geom_text_repel() + # geom_text_(nudge_x = 0.03, col = "black")
  scale_x_log10() + 
  scale_y_log10() + 
  labs(title = "US Gun Murders in 2010", 
       x = "Population in millions (log scale)",  
       y = "Total number of murders (log scale)") + 
  geom_abline(intercept = log10(r), 
              lty = 2, 
              color = "darkgray") + 
  scale_color_discrete(name = "US Region") + 
  theme_economist_white()

# Exercises 

data("heights")

males <- heights %>% 
  filter(sex == "Male")

heights$height[heights$sex == "Male"]

# Histogram ====
ggplot(data = males, 
       mapping = aes(x = height)) + 
  geom_histogram(binwidth = 1, 
                 col = "black", 
                 fill = "blue") + 
  labs(title = "Males Height Histogram", 
       x = "Height (in)", 
       y = "Frequency") + 
  theme_bw()

# Density plot ====
ggplot(data = males, 
       mapping = aes(x = height)) +
  geom_density(fill = "Gray") +
  labs(title = "Males  Height Density Plot", 
       x = "Height (in)", 
       y = "Dendity") + 
  theme_bw()

# QQ-Plot ====
ggplot(data = males, 
       mapping = aes(sample = height)) +
  geom_qq(col = "blue", 
          dparams = c(mean(males$height), sd(males$height))) + 
  labs(title = "QQ-Plot Males Height", 
       x = "Theoretical", 
       y = "Sample") + 
  geom_abline() + 
  theme_bw()
