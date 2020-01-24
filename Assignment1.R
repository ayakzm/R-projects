#Task1
install.packages("dslabs")
install.packages("gapminder")
library(ggplot2)
library(dslabs)
ggplot(heights) + geom_density(aes(x=height, col=sex))
  
#Task2

library(ggplot2)
library(dplyr)
library(gapminder)
df <- gapminder::gapminder %>%
  filter(year == 1992)
ggplot(df, aes(x = gdpPercap, y = lifeExp, color = continent, size=pop)) +   
  geom_point() +   scale_x_log10()
  

  