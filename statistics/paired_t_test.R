
### Comparing Means with Paired Data


# Paired Data are when you have 2 observations per individual

# e.g. may be a measurement that is 'before' and 'after' for same subject




### First let's look at and visualize Paired Data


library(tidyverse)


#### Read data

chico <- read_csv("data/chico.csv")
head(chico)

chico  # the two scores we're interested in are in separate columns at the moment
       # this is 'wide format' data


## We can summarize the data in each column with 'describe()'

library(psych)
describe(chico) #ignore the warnings


# This summarizes 'between group' differences - it doesn't show within subject changes...




### Visualizations ---

# traditional summaries of the data also struggle to show individual changes, only looking at the between group effects




### e.g. boxplot graph


# but first, to make the data into a format that we can plot in ggplot,
# we need to turn the data in long format

head(chico)



# need to reorder the data:

chico.long <- chico %>% pivot_longer(2:3) 

chico.long


# boxplot
ggplot(chico.long, aes(x=name, y=value, fill=name))+
  geom_boxplot(outlier.shape = NA, alpha=.5) +
  geom_jitter(width=.1, size=1) +
  theme_classic() +
  scale_fill_manual(values=c("lightseagreen","darkseagreen"))



### Better method is to do one of these two plots:

## Scatterplot
head(chico)

ggplot(chico, aes(x=grade_test1, y=grade_test2)) +
  geom_point() +
  theme_classic()+
  geom_abline(intercept =0 , slope = 1)


## Slope Graph
head(chico.long)

ggplot(chico.long, aes(x=name, y=value, group=id))+
  geom_point(alpha=.6, size=2)+
  geom_line(color="gray50")+
  theme_classic()




### Fundamentally, what a 'paired t-test' cares about are the differences in scores


## First, let's run the paired t-test command in R, then we'll break down what's going on (in another video):


t.test(chico$grade_test1, chico$grade_test2, paired=T)  # this is 2-tailed

t.test(chico$grade_test2, chico$grade_test1, paired=T)  # the only thing that changes is the sign of t


t.test(chico$grade_test2, chico$grade_test1, paired=T, alternative = "greater")  # this is 1-tailed



## The major assumption of the test is that your data are normal:

shapiro.test(chico$grade_test1)

shapiro.test(chico$grade_test2)


# you could also do QQ plots...

qqnorm(chico$grade_test1)
qqline(chico$grade_test1, col = "steelblue", lwd = 2) # hmm, not great.

qqnorm(chico$grade_test2)
qqline(chico$grade_test2, col = "steelblue", lwd = 2) # hmm, not great.



## In actual fact, what the paired t-test is doing is working on difference scores,
# so really, we need those to be approximately normally distributed:


# Calculate the difference between the two tests per subject:

chico$improvement <- chico$grade_test2 - chico$grade_test1 
head(chico)
chico

# descriptives of the difference scores
mean(chico$improvement)
sd(chico$improvement)


# check normality of difference scores
qqnorm(chico$improvement)
qqline(chico$improvement, col = "steelblue", lwd = 2) # bit better

shapiro.test(chico$improvement)







### Calculating effect sizes

# With a paired t-test, we calculate Cohen's d
# by dividing the mean difference score by the SD in difference scores



# Effect Size is equal to:
mean(chico$improvement) / sd(chico$improvement)  # 1.45






##### Try For Yourself Examples ----------------

# take a look at these before and after scores

ba <- read_csv("data/beforeafter.csv")

head(ba)

# make a scatterplot with the x being 'before' and y being 'after'
ggplot(ba, aes(x=before, y=after)) +
  geom_point() +
  theme_classic()+
  geom_abline(intercept =0 , slope = 1)


# run a Shapiro test on the before and after columns to check normality

shapiro.test(ba$before)
shapiro.test(ba$after)

# create a difference column for the difference between before and after

ba$difference <- ba$before - ba$after

head(ba)


# run a Shapiro test on the difference column

shapiro.test(ba$difference)


# run a paired t-test to determine if there is a difference in scores 
# between the before and after columns

t.test(ba$before, ba$after, paired = T)

# what is the CI?