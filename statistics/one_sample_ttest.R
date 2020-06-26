### One-sample t-test

# Dr Zeppo example data.

# Psychology Student scores:

zeppo <- c(50,60,60,64,66,66,67,69,70,74,76,76,77,79,79,79,81,82,82,89)

zeppo

length(zeppo)  # 20 - there are 20 students in the sample.

mean(zeppo)  # the mean of the sample is 72.3

sd(zeppo)  # the sample SD is 9.52

n <- length(zeppo)

n 

## 95% confidence interval of the population mean...

#  sample.mean +/-  t * (sampleSD / sqrt(n))

n-1   # degrees of freedom

qt(c(.025, .975), df=n-1)  #95% CI requires 2.5% in each tail

qt(c(.025, .975), df=19)  #

qt(.975, df=19)  # Bit easier to type

tval <- qt(.975, df=19)

tval


# So, CI is:
mean(zeppo) + tval * ( sd(zeppo) / sqrt(n)) # 76.76
mean(zeppo) - tval * ( sd(zeppo) / sqrt(n)) # 67.84



#### To run a one-sample t-test, comparing to some hypothesized mean

zeppo

t.test(zeppo, mu = 65.0)  # Default is a 2-tailed test.
# use this one to get the 95% CI


# you can do one-tailed test, with a predicted direction 
t.test(zeppo, mu = 65.0, alternative = "greater")  

t.test(zeppo, mu = 65.0, alternative = "less")  


shapiro.test(zeppo) # p>.05, so assume data approx normal





#### Example 2:

# On their second birthday, children know a mean of 50 words, and this is normally distributed.

# Researchers collected data from 12 children who were read to for >two hours per day.

# Did this sample of 12 children have a mean of greater than 50?


x <- c(45, 53, 71, 35, 51, 59, 49, 55, 78, 27, 66, 59)

x

shapiro.test(x) # p>.05, so assume approximately normal data.


t.test(x, mu = 50)  # two sided test

t.test(x, mu = 50, alternative = "greater")  # two sided test










#### Example 3:

# Data imported ...

library(tidyverse)

xt <- read_csv("data/crosstimes.csv")
xt



# one-tailed
# compare sample mean to hypothesized mean of 16.0
# for time3

xt$time3  # our sample data

mean(xt$time3)  # sample mean = 15.1

t.test(xt$time3, mu = 16.0) # this is 2-tailed - but has CI

t.test(xt$time3, mu = 16.0, alternative = "less") # this is 1-tailed


# assumption is that the data is normally distributed
shapiro.test(xt$time3)  # borderline



#### Worked Example - try for yourself...--------------

### Read in the Data BlueJays
jays <- read_csv("data/BlueJays.csv")

# just look at the females.
jaysF <- jays %>% filter(KnownSex == "F")
head(jaysF)

# 1. Is the variable "BillLength" approximately normal? Use a Shapiro-Test.

shapiro.test(jaysF$BillLength)


# 2. Use t-test, to 
# i)  Calculate a 95% CI of the female population BillLength mean
# ii) Test if the sample mean is meaningfully larger than 23.5mm

t.test(jaysF$BillLength, mu = 23.5)   # Confidence interval

t.test(jaysF$BillLength, mu = 23.5, alternative = "greater")   #t-test


