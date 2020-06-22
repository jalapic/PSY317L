##### Skewness and Kurtosis

library(tidyverse)
source("descriptives/mode_function.R") #run this line so can calc mode.
library(moments)# we use two functions from this package to measure skewness and kurtosis



# A Classical Normal Distribution should have
# skewness of close to 0
# kurtosis close to 3


## This following line just creates 100,000 data points
# that have a mean of 100 and sd of 5 and 
# come from a normal distribution

set.seed(1)

x <- rnorm(n = 100000, mean = 100, sd = 5.0)

x

mean(x) #99.99

sd(x) #5.02


# skewness and kurtosis


skewness(x)  # should be very close to 0

kurtosis(x)  # should be very close to 3

# do it a few times


# Note with a smaller sample size, our distribution will
# not be as normal, i.e. more skewed and not mesokurtic

x1 <- rnorm(n = 10, mean = 100, sd = 5.0)
x1

mean(x1)
sd(x1)

skewness(x1)
kurtosis(x1)


## back to the 100,000 data points
x


#Histogram

df <- data.frame(x)
head(df)


ggplot(df, aes(x = x)) + 
  geom_histogram(aes(y = ..density..), 
                 color = "black", 
                 fill = "mistyrose", 
                 alpha = 0.8,
                 binwidth = 1) + 
  geom_density(color = 'dodgerblue', lwd=1) 


p <- ggplot(df, aes(x = x)) + 
  geom_histogram(aes(y = ..density..), 
                 color = "black", 
                 fill = "mistyrose", 
                 alpha = 0.8,
                 binwidth = 1) + 
  geom_density(color = 'dodgerblue', lwd=1) 

p



### Let's look at the median, mean and mode of this distribution of numbers.

median(x)
mean(x)
estimate_mode(x)

# let's add to our plot

p + 
  geom_vline(xintercept = median(x), color = "red", lwd=1)+ 
  geom_vline(xintercept = mean(x), color = "blue", lwd=1)+ 
  geom_vline(xintercept = estimate_mode(x), color = "darkorange", lwd=1)




### Let's look at some data with skew to see what happens...


# mlb batting averages....

bats <- read_csv("data/batting.csv")

head(bats)

nrow(bats)

#histogram
p1 <- ggplot(bats, aes(x = avg)) + 
  geom_histogram(aes(y = ..density..), 
                 color = "black", 
                 fill = "lightseagreen", 
                 alpha = 0.2,
                 binwidth = .005) + 
  geom_density(colour = 'black', lwd=1) +
  theme_classic()

p1

#  where do you think the mode, median & mean will be?

median(bats$avg)
mean(bats$avg)
estimate_mode(bats$avg)

p1 + 
  geom_vline(xintercept = median(bats$avg), color = "red", lwd=1)+ 
  geom_vline(xintercept = mean(bats$avg), color = "blue", lwd=1)+ 
  geom_vline(xintercept = estimate_mode(bats$avg), color = "darkorange", lwd=1)


# what will the skewness and kurtosis be?




skewness(bats$avg)  # < 0 = negative skew

kurtosis(bats$avg)   # >3 = 'peaky' less in shoulders of tails



#### Final example - 'right skew'.


head(bats)

# let's look at at bats.

#histogram
p2 <- ggplot(bats, aes(x = totalAB)) + 
  geom_histogram(aes(y = ..density..), 
                 color = "black", 
                 fill = "plum", 
                 alpha = 0.2,
                 binwidth = 200) + 
  geom_density(colour = 'black', lwd=1) +
  theme_classic()

p2


bats %>% 
  filter(totalAB > 11000)

#  where do you think the mode, median & mean will be?

median(bats$totalAB)
mean(bats$totalAB)
estimate_mode(bats$totalAB)

p2 + 
  geom_vline(xintercept = median(bats$totalAB), color = "red", lwd=1)+ 
  geom_vline(xintercept = mean(bats$totalAB), color = "blue", lwd=1)+ 
  geom_vline(xintercept = estimate_mode(bats$totalAB), color = "darkorange", lwd=1)


# skewness and kurtosis

skewness(bats$totalAB)  # > 0 = positive skew

kurtosis(bats$totalAB)   # >3 = 'peaky' less in shoulders of tails


