#### Point Biserial Correlations


### loading libraries ----

library(tidyverse)



# Read in Data


# Do Male cats wander further from their house than Female cats ?

cats <- read_csv("data/cats.csv")  

nrow(cats)

head(cats)


#make a scatterplot
ggplot(cats, aes(x = sex, y = time)) + geom_point()

ggplot(cats, aes(x = sex, y = time)) + geom_jitter(width = .05) #easier to see


# adding a trendline 
ggplot(cats, aes(x = sex1, y = time)) + 
  geom_jitter(width = .05) +
  stat_smooth(method='lm',se=F)

# just tidying up the x-axis
ggplot(cats, aes(sex1,time)) + 
  geom_jitter(width = .05) +
  scale_x_continuous(breaks=c(0,1))+
  stat_smooth(method='lm',se=F)


## We can get a quick point biserial correlation using 'cor.test'

# using pearson (works if dichotomous variable is coded 0/1)

cor.test(cats$time, cats$sex1) # r = 0.38, p<.05
cor.test(cats$sex1, cats$time) # r = 0.38, p<.05


# suggests that there is a positive relationship 
# and male cats do move more than female cats.


# you can also do for non-parametric tests...

cor.test(cats$time, cats$sex1, method="spearman") # rho = 0.36, p<.05



## "better" versions of this test are available, 
## e.g. using library(polycor), but this is ok for now.


