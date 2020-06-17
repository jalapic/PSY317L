#### Partial Correlations


### loading libraries ----

library(tidyverse)

library(gridExtra) # to put plots next to each other

library(ppcor) # for partial correlation functions
# you'll get a warning message - that's ok.


### loading data ----

gs <- read_csv("data/gamescore.csv")


nrow(gs)

head(gs)


## Partial Correlation----



# it's possible that 'score' is affected by 'hours' and 'altertness'.

# but we really care about the relationship between 'hours' and 'score'
# controlling for 'altertness'.


ggplot(gs, aes(x=hours, y=score)) + 
  geom_point() +
  stat_smooth(method="lm",se=F)


cor(gs$hours, gs$score)  # r = 0.24

cor.test(gs$hours, gs$score)   # p=0.098

cor.test(gs$hours, gs$score, alternative = "greater")  # p=0.049




# Let's look at each plot and correlation in turn:

p1 <- ggplot(gs, aes(x=hours, y=score)) + 
  geom_point() +
  stat_smooth(method="lm",se=F)
p1
cor(gs$hours, gs$score)  # r = 0.24


p2 <- ggplot(gs, aes(x=alertness, y=score)) + 
  geom_point() +
  stat_smooth(method="lm",se=F)
p2
cor(gs$alertness, gs$score) # r = -0.21


p3 <- ggplot(gs, aes(x=hours, y=alertness)) + 
  geom_point() +
  stat_smooth(method="lm",se=F)
p3
cor(gs$hours, gs$alertness) # r = -0.29


# There's a way of plotting all next to each other...


grid.arrange(p1,p2,p3,nrow=1) # need library(gridExtra) for this




# Just an FYI:
# for partial correlations we need complete observations - no missing data allowed.


# we use 'pcor.test' from the ppcor package.

pcor.test(x=gs$hours, y=gs$score, z=gs$alertness)

cor.test(x=gs$hours, y=gs$score, z=gs$alertness)





### Example 2...

exams <- read_csv("data/exams1.csv")

nrow(exams)

head(exams)

# note these data are not approximately normal:
# all p<0.05, therefore reject null that they are approximately normal

shapiro.test(exams$revise)
shapiro.test(exams$exam)
shapiro.test(exams$anxiety)




# Let's look at how 'revise', 'exam' and 'anxiety' relate to each other:
# in particular, how revising predicts exams

p1 <- ggplot(exams, aes(x=revise, y=exam)) + 
  geom_point() +
  stat_smooth(method="lm",se=F)
p1
cor(exams$revise, exams$exam, method = "spearman")  # rho = 0.33

cor.test(exams$revise, exams$exam, method = "spearman")  # p<.05 therefore appears is relationship


p2 <- ggplot(exams, aes(x=revise, y=anxiety)) + 
  geom_point() +
  stat_smooth(method="lm",se=F)
p2
cor(exams$revise, exams$anxiety, method = "spearman") # r = -0.61


p3 <- ggplot(exams, aes(x=anxiety, y=exam)) + 
  geom_point() +
  stat_smooth(method="lm",se=F)
p3
cor(exams$anxiety, exams$exam, method= "spearman") # r = -0.39



grid.arrange(p1,p2,p3,nrow=1) # need library(gridExtra) for this




# we use 'pcor.test' from the ppcor package.
# because this is non-parametric, we set the method to spearman

pcor.test(x=exams$revise, y=exams$exam, z=exams$anxiety, method = "spearman")
 cor.test(x=exams$revise, y=exams$exam, z=exams$anxiety, method = "spearman")

# appears that the relationship between revision and exam score becomes much less
# meaningful when you account for anxiety.

