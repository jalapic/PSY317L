## How to calculate z-scores in R.


# z =  (x - mean(x)) / sd(x)

# z is a measure of how many standard deviations each point is from the mean of the distribution.


library(tidyverse)


### Example 1:

# I weighed 10 pineapples (in grams):

pineapple <- c(985, 1005, 1075, 977, 991, 1004, 1010, 1001, 952, 1035)

pineapple

mean(pineapple)

sd(pineapple)

pineapple - mean(pineapple)


dif <- pineapple - mean(pineapple)

dif 

dif / sd(pineapple)  # these are the Z-scores.


z <- dif / sd(pineapple)  # these are the Z-scores.

z


## What does the distribution of z-scores look like?

# that depends on your original distribution.

# let's just focus on what happens when your data are normally distributed.




### OK, Let's assume that the weights of pineapples are normally distributed.
# let's say they have a mean of 1003.5g and a standard deviation of 35g
# let's get the weights of 100000 pineapples.

set.seed(10) # this just makes sure we all get the same results

x <- rnorm(n= 100000, mean = 1003.5, sd = 35)

x # the weights of 100000 pineapples.

mean(x) # pretty close to what we wanted

sd(x) # pretty close to what we wanted


x - mean(x)  # difference of each data point from the mean.

dif <- x - mean(x)

dif / sd(x)  # these are the z scores.

z <- dif / sd(x)

z


# what do you think the mean and standard deviation of z-scores are ?


mean(z)

sd(z)


df <- data.frame(z) # making them into a dataframe

head(df)


# histogram
ggplot(df, aes(x = z)) + 
  geom_histogram(color = "black", fill = "cornflowerblue", alpha=0.5) +
  theme_classic()



# Overlay the mean:

ggplot(df, aes(x = z)) + 
  geom_histogram(color = "black", fill = "cornflowerblue", alpha=0.5) +
  theme_classic() +
  geom_vline(xintercept = mean(z), lwd=1, color = "darkorange")


# adding +/- 1sd and 2sd

ggplot(df, aes(x = z)) + 
  geom_histogram(color = "black", fill = "cornflowerblue", alpha=0.5) +
  theme_classic() +
  geom_vline(xintercept = mean(z), lwd=1, color = "darkorange") +
  geom_vline(xintercept = mean(z)+sd(z), lwd=1, color = "firebrick", lty=2) +
  geom_vline(xintercept = mean(z)-sd(z), lwd=1, color = "firebrick", lty=2) +
  geom_vline(xintercept = mean(z)+sd(z)+sd(z), lwd=1, color = "firebrick", lty=2) +
  geom_vline(xintercept = mean(z)-sd(z)-sd(z), lwd=1, color = "firebrick", lty=2) 
  


#### I bought a pineapple that was 940g and was upset.
# what proportion of pineapples are smaller than 940g?

# what z-score is a 940g pineapple?

# we'll use the population data to calculate it:

940 - 1003.5

(940 - 1003.5) / 35

zscore <- (940 - 1003.5) / 35

zscore

ggplot(df, aes(x = z)) + 
  geom_histogram(color = "black", fill = "cornflowerblue", alpha=0.5) +
  theme_classic() +
  geom_vline(xintercept = zscore, lwd=1, color="black")


# We can directly calculate what proportion is to the left of that black line:

pnorm(-1.814)
pnorm(zscore) #0.035  - there are 3.5% of pineapples smaller than our observed value.


# what should you get with these (you can estimate)?

pnorm(0)

pnorm(-1)

pnorm(1)

pnorm(-1.81)

pnorm(1.81)


### Reminder: all of this requires our data to come from a normal distribution.


#### Part 2:

# What is the probability of getting a pineapple between 990g and 1010g  ?


# convert to z scores:

(990 - 1003.5)  / 35   #-0.39
(1010 - 1003.5)  / 35  #0.19

z1 <- (990 - 1003.5)  / 35
z2 <- (1010 - 1003.5)  / 35

z1
z2


# Visualize:
ggplot(df, aes(x = z)) + 
  geom_histogram(color = "black", fill = "cornflowerblue", alpha=0.5) +
  theme_classic() +
  geom_vline(xintercept = 0, lwd=1, color="darkorange") +
  geom_vline(xintercept = z1, lwd=1, color="red", lty=2) +
  geom_vline(xintercept = z2, lwd=1, color="red", lty=2)


## get proportion of curve to left of each z-score

pnorm(z1) # 0.35

pnorm(z2) # 0.57

# so the area between them is:

pnorm(z2) - pnorm(z1)  #0.22



### Quick last question:

# what probability is there of getting a pineapple of greater than 1100g ?

(1100 - 1003.5) / 35  # z = 2.76

pnorm(2.76)  # proportion = 0.997 to left

# because we want greater than, we have to do this:

1 - pnorm(2.76)   #0.003  (so 0.3% chance)








##### Try for yourself -------------------------------------------  ##################


# What is the probability of a pineapple weighing more than 1014g?



# What is the probability of a pineapple weighing between 950g and 975g?




### This is a different/harder example. It's looking at real data that is not normally distributed.

## Load the pga dataset and filter the year 2015, and select name and driveavg:

pga <- read_csv("data/pga.csv")
head(pga)

pga15 <- pga %>% filter(year == 2015) %>% select(name, driveavg) %>% as.data.frame()
head(pga15)
tail(pga15)

# caclulate the z-scores for the "driveavg" column and make a histogram of them.


vals <- pga15$driveavg # store the values in the column as 'vals'

z.vals <- (vals - _____(vals) ) / ____(vals) # make the z-scores

df.vals <- data.frame(z = z.vals) # put the zscores into a dataframe

head(df.vals)

# make histogram

ggplot(_____, aes(x = _______)) + 
  geom_histogram(color = "black", fill = "mistyrose", alpha=0.5) +
  theme_classic()



# what do you notice ? 

# this last bit just helps us find who has a zscore above 2.5
pga15$z <- z.vals
pga15 %>% arrange(-z) %>% filter(z>2.5)
