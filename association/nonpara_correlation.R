#### Correlations - Non-Parametric


## Your data aren't always approximately normal.

# What if you still want to do a correlation?

# There are a family of "non-parametric correlations" that you can use.

# These measure association without requiring normality.




# Example 1.

library(tidyverse)

cheese <- read_csv("data/cheese.csv")

nrow(cheese)

head(cheese)

# let's make a scatterplot of saturated fat against cholesterol
ggplot(cheese, aes(x = sat_fat, y = chol)) + geom_point()


# looks like there's a pretty obvious relationship.

# let's do our checks for normality...

shapiro.test(cheese$sat_fat)  # P < 0.05, therefore can reject null that data is normal
shapiro.test(cheese$chol)  # P < 0.05, therefore cannot reject null that data is normal

qqnorm(cheese$sat_fat)
qqline(cheese$sat_fat, col = "steelblue", lwd = 2)

qqnorm(cheese$chol)
qqline(cheese$chol, col = "steelblue", lwd = 2)



## OK, so doing a Pearson correlation is not best...


# Option 1.  Spearman's Rank Test

ggplot(cheese, aes(x = sat_fat, y = chol)) + geom_point()

cor(cheese$sat_fat, cheese$chol, method = "spearman")      # rho = 0.87

cor.test(cheese$sat_fat, cheese$chol, method = "spearman") 

# p<.05 therefore reject null that rho = 0
# don't worry about the warning message.


# you can do 1-tailed tests as with Pearson

cor.test(cheese$sat_fat, cheese$chol, method = "spearman", alternative = "greater") 


## Option 2 - other types...

## There are other types of non-parametric correlation...
# Kendall's tau-b is commonly used also.

cor.test(cheese$sat_fat, cheese$chol, method = "kendall") 





########


# Example 2.


### Non-parametric tests should always be used if you have rank/ordinal data.
# (there are often used with very small sample sizes too)


# these data show the dominance rank, age, body size and testosterone levels
# for a group of 18 animals.

test <- read_csv("data/testosterone.csv")

head(test)

ggplot(test, aes(x = drank, y = testosterone)) + geom_point()


cor(test$drank, test$testosterone, method = "spearman") # rho = -0.91


cor.test(test$drank, test$testosterone, method = "spearman") # 2- tailed

cor.test(test$drank, test$testosterone, method = "spearman", alternative = "less") # 1- tailed

ggplot(test, aes(x = drank, y = testosterone)) + 
  geom_point() +
  stat_smooth(method = "lm", se=F) +
  xlab("Dominance Rank") +
  ylab("Testosterone Level") 




