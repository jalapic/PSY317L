

## Linear Regression



library(tidyverse)

# import data
df <- read_csv("films2015.csv")

# look at data
head(df)

# visualize data
# add a line of best fit to plot
ggplot(df, aes(x=rottentomatoes, y=imdb)) + 
  geom_point() +
  stat_smooth(method="lm")


fit <- lm(imdb ~ rottentomatoes, data = df)
fit
summary(fit)
