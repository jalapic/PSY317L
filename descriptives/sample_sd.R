
### Standard Deviation in R...

# showing that 'sd' is the sample standard deviation

library(tidyverse)

# Import the Data
cheese <- read_csv("data/cheese.csv")


head(cheese)

# Plot the Data
ggplot(cheese, aes(x=chol)) + 
  geom_histogram(color="black", fill="lightseagreen", binwidth = 10)+
  theme_classic()



cheese$chol

sd(cheese$chol)  # sample SD = 34.53

mean(cheese$chol) #69.4



### Let's do it "by hand"

x <- cheese$chol  # let's call our distribution of numbers x

x # our numbers are now called 'x'

sd(x)  # 34.53
mean(x) # 69.4  - these are still the same


# step 1.  Deviation of each x from the mean of x

x

x - mean(x)

dif <- x - mean(x)  # let's call these differences 'dif'

dif

# step 2 - square the differences.

dif^2

dif2 <- dif^2

dif2  # these are the differences squared.


# Step 3. Sum up these squared differences
sum(dif2)


# Step 4. We now divide by n-1 to get an estimated 'average squared deviation'

nrow(cheese) #73
length(x) #73 observations.

sum(dif2) / (length(x) - 1)
sum(dif2) / 72

asd <- sum(dif2) / (length(x) - 1)

asd

# Step 5. Square root to stanardize back to interpretable units.

sqrt(asd)  #34.53008



### All together...

x                                  # our data
dif <- x - mean(x)                 # difference of each x from mean of x
dif2 <- dif^2                      # squared differences
sum(dif2)                          # the sum of the squared diffrences
asd <- sum(dif2) / (length(x) - 1) # the average squared difference using n-1
sqrt(asd)                          # the sample standard deviation

sd(x)


## We could have done this in one line..
# but until you're familiar with R, this can be hard to look at:

sqrt(sum((x - mean(x))^2)/(length(x)-1)) # sample SD

sqrt(sum((x - mean(x))^2)/(length(x)))  # population SD


