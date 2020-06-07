
### Correlation Example


library(tidyverse)

# import data
df <- read_csv("films2015.csv")

# look at data
head(df)

# visualize data
ggplot(df, aes(x=rottentomatoes, y=imdb)) + geom_point()


# run a correlation test (Pearson's - parametric)
cor.test(df$rottentomatoes, df$imdb)

# run a correlation test (Spearman's - non-parametric)
cor.test(df$rottentomatoes, df$imdb, method="spearman")  #warning message is ok.


# add a line of best fit to plot
ggplot(df, aes(x=rottentomatoes, y=imdb)) + 
  geom_point() +
  stat_smooth(method="lm")
