## Working with colors in R

library(tidyverse)


## What different colors can you use in R ?


# start with a basic scatterplot....

cheese <- read_csv("data/cheese.csv")
head(cheese)

ggplot(cheese, aes(x=sat_fat, y=chol) ) + geom_point()


# 1. adding a name by color name
ggplot(cheese, aes(x=sat_fat, y=chol) ) + geom_point(color = "purple")


# you can look up color names here:
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
ggplot(cheese, aes(x=sat_fat, y=chol) ) + geom_point(color = "darkcyan")


# 2. you can enter a hex code.
# e.g. use google's color picker
# you need to include the #

ggplot(cheese, aes(x=sat_fat, y=chol) ) + geom_point(color = "#1ed427")





###  Manually Selecting Colors with groups.


## Let's use a dataset on penguins:

penguins <- read_csv("data/penguins.csv")

head(penguins)


ggplot(penguins, aes(x = body_mass_g,  y = flipper_length_mm, color = species) ) +
  geom_point()


# make sure you supply the same number of colors as categories

ggplot(penguins, aes(x = body_mass_g,  y = flipper_length_mm, color = species) ) +
  geom_point() +
  scale_color_manual(values = c("forestgreen", "deepskyblue", "coral"))




## Line Graph Example

covid.county <- read_csv("data/countycovid.csv")
head(covid.county)

ggplot(covid.county, aes(x=day, y=total, color=county)) + 
  geom_line()

# supply the same number of colors as categories...
ggplot(covid.county, aes(x=day, y=total, color=county)) + 
  geom_line() +
  scale_color_manual(values = c("red", "blueviolet", "darkorange2"))


# you can mix names and hexcodes...
ggplot(covid.county, aes(x=day, y=total, color=county)) + 
  geom_line() +
  scale_color_manual(values = c("red", "#888aeb", "darkorange2"))






####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.

# 1. For the cheese dataset, change the color of the points using a hexcode of your choice.

ggplot(cheese, aes(x=sat_fat, y=chol) ) + geom_point(color = ______)



# 2. For the penguins dataset, plot culmen_length_mm against culmen_depth_mm and color points by species 
# Pick your own colors

ggplot(penguins, aes(x = culmen_length_mm, y = culmen_depth_mm, color = species) ) +
  geom_point() + 
  scale_color_manual(values = c(____,  ____, ______))



