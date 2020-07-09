

### The following is Digging Deeper into the Theory of the Paired Test:


### Quick Recap of The Data:



library(tidyverse)


#### Read data

chico <- read_csv("data/chico.csv")
head(chico)

chico  # the two scores we're interested in are in separate columns at the moment
# this is 'wide format' data


## Scatterplot
head(chico)

ggplot(chico, aes(x=grade_test1, y=grade_test2)) +
  geom_point() +
  theme_classic()+
  geom_abline(intercept =0 , slope = 1)



## If we just ran the test we'd do this:

t.test(chico$grade_test2, chico$grade_test1, paired=T)



#### Difference Scores

# Create a new column that is the difference between our two columns:
chico$improvement <- chico$grade_test2 - chico$grade_test1 
head(chico)
chico


# histogram of difference scores
ggplot(chico, aes(x=improvement)) +
  geom_histogram(color='black', fill='green', alpha=.4, boundary=0, binwidth = .5) +
  theme_classic()+
  geom_vline(xintercept = 0, lty=2, color='black')


# descriptives of the difference scores
mean(chico$improvement)
sd(chico$improvement)


# check normality of difference scores
qqnorm(chico$improvement)
qqline(chico$improvement, col = "steelblue", lwd = 2) # bit better

shapiro.test(chico$improvement)




### What we can essentially do now is a one-sample t-test on the difference scores.



mean(chico$improvement)


t.test(chico$improvement, mu=0) # this is a one-sample t-test

t.test(chico$grade_test2, chico$grade_test1, paired=T)  # notice it's the same as this






##### The below is the 'theory' regarding the above.

# It's a recap of the one-sample t-test stuff.

# you wouldn't ever do it in R like this, but this might help you follow where the numbers came from.


## To break it down, how does it get the confidence intervals ?


# we assume our one sample comes from a sampling dist of 
# shape t with d.f. = n-1

nrow(chico) - 1  #19

# we assume for the CI, that the mean of that sampling dist is our estimate of the mean

mean(chico$improvement)  # 1.405


# the standard dev of that sampling dist (the sem), is approx equal to  s/sqrt(n)

sd(chico$improvement) / sqrt(nrow(chico))  # 0.2169738


# now we need to know the value of 't' for d.f. = 19, that demarks 2.5% in each tail of the distribution:

qt(.975, df=19)  # 2.093024


# We calculate the 'error' by multiplying 't' by the sem
2.093024 * 0.2169738  # 0.454

qt(.975, df=19)*(sd(chico$improvement) / sqrt(nrow(chico)))  #0.454 (this is just a bit hard to read)

err <- qt(.975, df=19)*(sd(chico$improvement) / sqrt(nrow(chico))) # save it as 'err'

err

# upper bound:
mean(chico$improvement)  + err   #1.859

# lower bound:
mean(chico$improvement)  - err   #0.951

# check we agree with t.test():
t.test(chico$improvement, mu=0) #yes!




## Signifcance Testing: ----

#### To caculate our observed value of 't' and associated p-value:


# here we again assume that our sample of observed differences is one sample from a sampling dist
# that sampling dist is t-shaped and has d.f. of n-1 = 19

# this time our null hypothesis is that the mean of that distribution is 0 (no overall difference)
# therefore, we ask, how surprising was our one observed mean of sample differences?

# we calculate how usual or unusual our one observed sample mean was in units of 't'.
# to calculate 't' (observed t), we need the SD of the sampling distribution, which is estimated by s/sqrt(n)


## Let's actually plot this t distribution:

## don't worry about this code.... just look at the picture

t <- seq(-9,9,by=.01)
Density <- dt(t, df=19)
df <- data.frame(t,Density)
ggplot(df, aes(x=t,y=Density))+
  theme_classic()+
  geom_line(color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black',lwd=1) +
  ggtitle("t-distribution with d.f.=19")


# The SD of this distribution is

sd(chico$improvement) / sqrt(nrow(chico))  # 0.2169738

sem <- sd(chico$improvement) / sqrt(nrow(chico))

sem


## calculate our observed value of t:

mean(chico$improvement)  / sem  # 6.475

tobs <- mean(chico$improvement)  / sem

tobs


# replot:

ggplot(df, aes(x=t,y=Density))+
  theme_classic()+
  geom_line(color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black',lwd=1) +
  ggtitle("t-distribution with d.f.=19") +
  geom_vline(xintercept=tobs, color='orange', lty=2)


# What proportion of curve is to left of our observed t ?
pt(tobs, df=19) # 0.9999983

# Our p value for a one-tailed test would be, what proportion of samples have a value higher than our observed:
1 - pt(tobs, df=19) #0.0000017

# To get a 2-tailed probability we just multiply this by 2
0.0000017 * 2   #0.000034


# let's check:

t.test(chico$grade_test2, chico$grade_test1, paired=T)


