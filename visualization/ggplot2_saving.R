## Saving ggplot images

# let's make a plot

library(tidyverse)

# import the cheese dataset
cheese <- read_csv("data/cheese.csv")
head(cheese)

ggplot(cheese, aes(x = chol, y = kcal)) + geom_point() + theme_classic()


# Option 1...   just screenshot it...

# Option 2...   hit the 'export' tab on the plot viewer...

# Option 3...   you can use a function called 'ggsave'


# first, assign your plot as an object...
# just making it a bit different so that you can tell

head(cheese)

p <- ggplot(cheese, aes(x = chol, y = kcal)) + 
  geom_point(color='purple', size=2) + 
  theme_classic() +
  xlab("Cholesterol") +
  ylab("Calories in kcal") +
  ggtitle("Cheese")

p

ggsave("img/cheese_plot.png", p)  # save as a png

ggsave("img/cheese_plot.pdf", p)  # save as a pdf


## if you need to play around with the sizes, you can do that too...

ggsave("img/cheese_plot2.png", p, width = 10, height = 8)  #(in inches, though can be in cm)

