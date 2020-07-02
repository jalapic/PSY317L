#### Pearson Correlation Introduction


### loading libraries ----

library(tidyverse)


### loading data ----

jays <- read_csv("data/BlueJays.csv")

jayM <- jays %>% filter(KnownSex == "M") # we'll just look at Males

nrow(jayM) # 63 observations

head(jayM)



## First let's make Scatterplots ----

# e.g. Body Mass against Head Size

ggplot(jayM, aes(x=Mass, y=Head)) + 
  geom_point(shape = 21, color = "navy", fill = "dodgerblue")


# it looks like there is an association.
# we can directly measure the Pearson's 'r' with "cor"


# remember that df$colname gives you all the data in that column

jayM$Mass

cor(jayM$Mass, jayM$Head)  # r = 0.58,  a strong positive correlation.


# and of course it doesn't matter which way round you write it

cor(jayM$Head, jayM$Mass)   # same result.



# just a note - if you have missing data NA... use this instead...

cor(jayM$Head, jayM$Mass, use = "everything") 





# If you wish to you can add a line of best fit to your data like this:

ggplot(jayM, aes(x=Mass, y=Head)) + 
  geom_point(shape = 21, colour = "navy", fill = "dodgerblue") +
  stat_smooth(method="lm", se=F)



## To do a Pearson Correlation, the data should be approximately normal
# this can be tested using various tests e.g.


## Test for normality using Shapiro-Wilks tests and QQ Plots ----

shapiro.test(jayM$Mass)  # P > 0.05, therefore cannot reject null that data is not normal
shapiro.test(jayM$Head)  # P > 0.05, therefore cannot reject null that data is not normal

qqnorm(jayM$Mass)
qqline(jayM$Mass, col = "steelblue", lwd = 2)

qqnorm(jayM$Head)
qqline(jayM$Head, col = "steelblue", lwd = 2)





### Significance testing----

# the default is to run a 2-tailed test.

cor.test(jayM$Head, jayM$Mass)  

cor.test(jayM$Head, jayM$Mass, conf.level = 0.99)  #note that you can specify confidence intervals




## you can do 1-tailed tests also...

# testing null hypothesis that r is not greater than 0
# i.e. is my correlation meaningfully positive ?

cor.test(jayM$Head, jayM$Mass, alternative = "greater") 



# testing null hypothesis that r is not smaller than 0
# i.e. is my correlation meaningfully negative ?

cor.test(jayM$Head, jayM$Mass, alternative = "less") 




## Example 2....

ggplot(jayM, aes(x = Mass, y = BillDepth)) + geom_point()

shapiro.test(jayM$BillDepth)

cor.test(jayM$Mass, jayM$BillDepth)  # p>0.05

cor.test(jayM$Mass, jayM$BillDepth, alternative = "greater") # p>0.05

# you should have an a priori reasoning whether you need to do a 1-tailed or 2-tailed test.




## Example 3 - negative correlation..

# These data looks at the number of hours spent playing an online game, 
# the top score achieved and a self-report measure of tiredness.
# lower tiredness score = more tired

gs <- read_csv("data/gamescore.csv")

head(gs)

nrow(gs)  # 50 subjects
 
ggplot(gs, aes(x = hours, y = tiredness)) + geom_point()

shapiro.test(gs$hours)     #p>.05
shapiro.test(gs$tiredness) #p>.05

cor(gs$hours, gs$tiredness) # negative r,  -0.29

cor.test(gs$hours, gs$tiredness) # p<.05, therefore suggests there is a relationship

ggplot(gs, aes(x = hours, y = tiredness)) + 
  geom_point() +
  stat_smooth(method = "lm", se = F)





####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.

# 1. For the jayM data, plot Head against Skull

ggplot(jayM, aes(x = ______,  y= ______)) + geom_point()


# 2. For the jayM data, check if the variable 'Skull' is approximately normal using a Shapiro test.
# what does the p-value tell you?

______(jayM$Skull)


# 3. For the jayM data, conduct a correlation test (2-tailed) testing 
# if there is a meaningful association between Mass and Skull
# What do the 'r' and 'p-values' suggest?

______(jayM$Mass, _______)


# 4. Read in the 'gamescore.csv' data and make the plot
# this looks at the number of hours spent playing an online game, and the top score achieved
# the third column is a self-report measure of tiredness.

gs <- read_csv("data/gamescore.csv")
ggplot(gs, aes(x = hours, y = score)) + geom_point()


# think about whether you would do a 1-tailed or 2-tailed correlation test
# when testing the association between 'hours' and 'score'.
# complete the code below to do both types of test
# how do you interpret these results?

cor.test(gs$hours, __________)  # 2-tailed
cor.test(gs$hours, __________, alternative = ______________) #1-tailed



