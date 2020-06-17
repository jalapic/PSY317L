### Demonstration of the Mean in R.

# Calculating Means.

# let's make an object x which is a distribution of numbers

x <- c(5, 10, 15, 30)

x

sum(x) # this calculates the sum or total

length(x) # this tells you the 'n'


# therfore the mean is:

sum(x) / length(x)

# which you can get with the following 
mean(x)



##### Example 2...

# here we will read in data...

library(tidyverse)

skit <- read_csv("data/skittles.csv")

skit

# the numerical data
skit$number

# the total
sum(skit$number)

# the n
length(skit$number)

# the mean is total/n
sum(skit$number) / length(skit$number)

# which you can also get by:
mean(skit$number)


