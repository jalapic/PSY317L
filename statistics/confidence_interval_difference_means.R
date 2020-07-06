#### Confidence Interval for Difference in Means


## This tutorial is a bit more theory..


library(tidyverse) # just for plotting


## An example:

# A researcher compares the reaction times of two groups on a motor test.
# individuals are different in each group
# groupA took some stimulant,  groupB is the control group

groupA <- c(5.5, 5.1, 3.5, 5.1, 4.6, 4.6, 5.9, 4.0, 3.1, 3.8)
mean(groupA)     # 4.52
sd(groupA)       # 0.91
nA<-length(groupA)   # 10
nA

groupB <- c(5.7, 5.3, 5.9, 5.0, 5.0, 4.3, 4.1, 5.9, 5.9, 5.8, 5.4, 5.2, 4.9, 5.5)
mean(groupB)     # 5.28
sd(groupB)       # 0.58
nB <- length(groupB)   # 14
nB


# plot the data:
d <- data.frame(values = c(groupA, groupB),
                group = c(rep("A",10), rep("B", 14))
)

d

ggplot(d, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha=.3) +
  geom_jitter(width=.1, size=2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))




# what's the difference in means?

mean(groupA)  - mean(groupB)   # -0.76

# group B is slower by 0.76 seconds on average.


## But how meaningful is this difference?





## We can imagine that this observed difference in sample means came from a 
# sampling distribution of the difference in sample means
# our observed value was just one possible sample.

# that sampling distribution is has a mean, and a standard deviation

# if we assume that our observed value of the difference in means 
# is the mean of that sampling distribution
# then we can create a 95% confidence interval around it.


# CI =  sample_dif +/-  t * SEDM 




# If we assume equal variances between the population of group A and B, we can calculate the
# standard deviation of this sampling distribution.


# it's equal to:
# the pooled estimate of the common standard devation * sqrt(1/n1 + 1/n2)

sd(groupA)  #0.91

sd(groupB)  #0.58


# the formula for the pooled estimate of the common standard deviation is long-winded:
# we discuss it in more detail in another video

# from first principles, calculating deviations from each group mean
difA2 <- (groupA - mean(groupA))^2
difB2 <- (groupB - mean(groupB))^2
sumsq <- sum(difA2) + sum(difB2)
n <- nA + nB #24
sd.pool <- sqrt(sumsq/(n-2))

sd.pool # 0.73  this is the estimated pooled s.d.

sedm <-  sd.pool * sqrt( (1/nA) + (1/nB))

sedm



## So now we have the mean and the sd of our sampling distribution of difference in sample means.


# For CIs, we assume the mean to be our estimate of the mean differences:

mean(groupA)  - mean(groupB)  #-0.76


# Because our sampling distribution approximates to a t-distribution of d.f. n-2
# we calculate the value of 't' which leaves 2.5% in the tails.

tval <- qt(.975, df = n-2)
tval


-0.76 +  (tval*sedm)  # upper bound = -0.13
-0.76 -  (tval*sedm)  # lower bound = -1.39


t.test(groupA, groupB, var.equal = T)  # this is the t-test function




### Visualizing this:

# Don't worry about the gross loooking code... just using it to make the plot:

m <- -0.76 # mean
v <- sedm^2 # variance, sedm squared
df <- 22
vals <- rt(n=500000, df=df)*sqrt(v * (df-2)/df) + m
df1 <- data.frame(val = vals)


ggplot(df1, aes(x=val)) +
  geom_histogram(aes(y = ..density..), color='black', fill='#1930FF', alpha=.3)+
  theme_classic()+
  geom_density(alpha = 0.7, fill = "white") + 
  geom_vline(xintercept = -0.13, lwd=1, color="red") + 
  geom_vline(xintercept = -1.39, lwd=1, color="red") + 
  geom_vline(xintercept = -0.76, lwd=1,lty=2, color='black')+
  xlab("Difference in Sample Means") +
  ylab("") +
  ggtitle("Sampling Distribution of Differences in Sample Means",
          subtitle = "Red Lines Represent 95% CIs")










###########


### Example 2:  Harpo Data...

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

dd

ggplot(dd, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha=.3, outlier.shape = NA) +
  geom_jitter(width=.1, size=2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))


## assume mean of sampling distribution is

meandif <- mean(anastasia)  - mean(bernadette)   # 5.48
meandif # this is our observed difference in sample means



meandif  # we assume that this is the mean of the sampling distribution


## to get standard deviation of this sampling distribution:

# 1. get pooled standard variation:
# this is in the way of calculating average squared deviations.

difA2 <- (anastasia - mean(anastasia))^2
difB2 <- (bernadette - mean(bernadette))^2
sumsq <- sum(difA2) + sum(difB2)
n <- nA + nB #33
sd.pool <- sqrt(sumsq/(n-2))
sd.pool  #7.41

# 2. the standard deviation is equal to the pooled standard devation * sqrt(1/n1 + 1/n2)

sedm <-  sd.pool * sqrt( (1/nA) + (1/nB))

sedm  # 2.59   # our standard deviation of the sampling dist.


# now we get our value of t for df n-2 that leaves 2.5% in each tail:
tval <- qt(.975, df = nA + nB -2)
tval  #2.04

meandif

meandif +  (tval*sedm) # 10.76
meandif -  (tval*sedm) # 0.20

t.test(anastasia, bernadette, var.equal = T)


## plotting confidence intervals

dci <- data.frame(group = "", 
                  mean = meandif, 
                  lower = meandif -  (tval*sedm), 
                  upper = meandif +  (tval*sedm)
)

dci

ggplot(dci, aes(x=group, y=mean, ymax=upper, ymin=lower)) +
  geom_errorbar(width=0.2, size=1, color="blue") +
  geom_point(size=4, shape=21, fill="white") +
  theme_minimal() +
  coord_flip() +
  xlab("") +
  ylab("Estimate of Difference in Sample Means")
  
   
