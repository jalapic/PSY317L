
### Overview of how to do 2 sample t-tests in R


# this tutorial is not about theory - just about doing.

library(tidyverse)


## Let's look at the data:

anastasia <- c(65, 74, 73, 83, 76, 65, 86, 70, 80, 55, 78, 78, 90, 77, 68)
bernadette <- c(72, 66, 71, 66, 76, 69, 79, 73, 62, 69, 68, 60, 73, 68, 67, 74, 56, 74)


mean(anastasia)            # 74.5
sd(anastasia)              # 9.0
nA<-length(anastasia)      # 15
nA

mean(bernadette)           # 69.1
sd(bernadette)             # 5.8
nB <- length(bernadette)   # 18
nB


# plot the data:
dd <- data.frame(values = c(anastasia, bernadette),
                 group = c(rep("Anastasia",15), rep("Bernadette", 18))
)

dd  # the data are in long format.


# boxplot
ggplot(dd, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha=.3, outlier.shape = NA) +
  geom_jitter(width=.1, size=2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))




## Before we go on, we should check two things:


##  1. One assumption is that are data are approximately normal:

shapiro.test(anastasia)  # p>.05 so data approximately normal
shapiro.test(bernadette) # p>.05 so data approximately normal

# you could also do QQ plots...

qqnorm(anastasia)
qqline(anastasia, col = "steelblue", lwd = 2) # looks ok

qqnorm(bernadette)
qqline(bernadette, col = "steelblue", lwd = 2) # looks ok



##  2. Another assumption is that we have homogeneity of variance:
# equality of variance between groups

sd(anastasia)
sd(bernadette)

var(anastasia)
var(bernadette)  # are these close enough???

# you can do a Levene's test to test for this, from the package 'car':
library(car)
leveneTest(y = dd$values, group = dd$group)

# a p-value of >.05 indicates that there is not sufficient evidence to suggest that the groups have different variances
# i.e. p>.05, we can assume the groups have the same variance.



### Doing the Student's t-test 

# OK let's do the test  - first 2-tailed

# this is the Student's t-test:
t.test(anastasia, bernadette, var.equal = T)


# you can also use data from long format:
head(dd)
t.test(values ~ group, data=dd, var.equal = T)



### What about one-tailed tests?

# this would be the case if you had a priori prediction:


# these are the same
t.test(anastasia, bernadette, var.equal = T, alternative = "greater")

t.test(values ~ group, data=dd, var.equal = T, alternative ="greater")


# if you were predicting group A would have a mean lower than group B
t.test(anastasia, bernadette, var.equal = T, alternative = "less")




## The Big Elephant in the Room......

# We are doing Student's t-tests here.  
# They assume equal variances between groups.

# There is actually an independent 2 sample t-test you can run that doesn't
# It applies a correction - it's called the Welch's t-test

# There is no reason not to use the Welch's test (I recommend you do):
# you don't have to worry about equal variances:

# just drop the "var.equal" thing:


t.test(anastasia, bernadette)  # notice the changes...

t.test(anastasia, bernadette, var.equal = T) # this is the Student's for comparison



t.test(anastasia, bernadette, alternative = "greater") 




## Effect Size for Independent two sample t-tests:

# the difference between the means of each group divided by the pooled standard deviation

library(lsr)
cohensD(values ~ group, data = dd)  # d = 0.74





##### Try For Yourself Examples ----

# load in the BlueJays

jays <- read_csv("data/BlueJays.csv")
head(jays)

maleJays <- jays %>% filter(KnownSex=="M")
femaleJays <- jays %>% filter(KnownSex=="F")

#plot

ggplot(jays, aes(x=KnownSex, y=BillDepth)) +
  geom_boxplot()+
  geom_jitter(width=.1)+
  theme_classic()


# 1. perform a Shapiro-Wilk test to determine if BillDepth is normally distributed for male and female jays

shapiro.test(maleJays$BillDepth)
shapiro.test(femaleJays$BillDepth)


# 2. Use the long-form data to perform a two-tailed Welch's t-test to see if there are differences in the means between male and females in BillDepth

t.test(BillDepth ~ KnownSex, data = jays)


# 3. Do a 1-tailed test to determine if male Blue Jays have heavier skulls than female jays.

t.test(maleJays$Skull, femaleJays$Skull, alternative="greater")


# 4. Compute the effect size for our BillDepth t-test using 'cohensD'
library(lsr)
cohensD(BillDepth ~ KnownSex, data = jays)  
