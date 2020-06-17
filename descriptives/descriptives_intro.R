### Descriptive Statistics

## There are several ways of getting descriptive statistics.
# I'll introduce you to some that may be useful for getting quick summaries

library(tidyverse)

# Import the cheese dataset

# this dataset has no 'groups'

cheese <- read_csv("data/cheese.csv")

head(cheese)


# let's focus on the cholesterol column for now

ggplot(cheese, aes(x=chol)) + 
  geom_histogram(color="black", fill="lightseagreen", binwidth = 10)+
  theme_classic()

cheese$chol



### Basic Statistics Data Summary functions

range(cheese$chol) # range

min(cheese$chol) # minimum

max(cheese$chol) # maximum

mean(cheese$chol)  # mean

sd(cheese$chol) # this is the sample standard deviation.



median(cheese$chol) # median

IQR(cheese$chol) # the interquartile range
 
quantile(cheese$chol, .25) # lower quartile

quantile(cheese$chol, .75) # upper quartile


# median = 79, lqr = 46, uqr = 94
# min = 3, max = 123


# these quartiles may be slightly different to what you'd get by hand.

ggplot(cheese, aes(y=chol)) + 
  geom_boxplot(color='black', fill='lightseagreen')


# nb we get those numbers on the x-axis because there is no 'group'
# we can get rid of them like this:

ggplot(cheese, aes(y=chol)) + 
  geom_boxplot(color='black', fill='lightseagreen') + 
  scale_x_discrete(breaks = NULL)





#### Missing Data....

# Descriptives are straightfoward but there are traps
# particularly missing data...

# import data
# these data show times for 10 competitors at a crossword competition
# they are how long in minutes (converted to decimals)
# it took to complete 5 different puzzles

# notice one time is missing
xt <- read_csv("data/crosstimes.csv")
xt

mean(xt$time1)  # this works

mean(xt$time5)  # this doesn't work

sd(xt$time5)  # this doesn't work


# you have to do this if you have missing data

mean(xt$time5, na.rm = T)

min(xt$time5, na.rm = T)

sd(xt$time5, na.rm = T)

IQR(xt$time5, na.rm = T)

median(xt$time5, na.rm = T)

quantile(xt$time5, 0.25, na.rm = T)



##### What if you wish to get results from all columns at once...


### All at once...

######

summary(cheese) #some summary stats

summary(xt)


# Some ways from other packages
# warning messages are ok.

library(psych)

describe(cheese)

describe(xt)



################----------------------#############################

### Try for yourself examples.  

# Most are free form, except one fill in the blank....
# Copy and Paste and Modify from above 


#1. Calculate the IQR for time3 of the xt data


#2. Calculate the standard deviation for kcal from the cheese data



# Load in the "bmi.csv" dataset
# this gives various health measurements of 20 subjects
# but it contains missing data

bmi <- read_csv("data/bmi.csv")

head(bmi)
tail(bmi)

#3. Make a boxplot of the bmi column.

ggplot(______, aes(y=______)) + 
  geom_boxplot(color='black', fill='lightseagreen') + 
  scale_x_discrete(breaks = NULL)



# 4. Use 'mean()' and 'sd()' functions to calculate the mean and standard deviation of the bmi column.


# 5. Use either 'summary()' or 'describe()' from the Psych package to get an overall summary of the bmi dataset.

