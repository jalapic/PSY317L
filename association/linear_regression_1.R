#### Introduction to Linear Regression


## Ordinary Least Squares Regression.



library(tidyverse)


# Import Data

df <- read_csv("data/parenthood.csv")

head(df)

tail(df)

nrow(df)

# Scatterplot
ggplot(df, aes(x = dan.sleep, y = dan.grump)) + 
  geom_point() + 
  stat_smooth(method='lm', se=F) 


# We could do a correlation
cor.test(df$dan.sleep, df$dan.grump)



# The line of best fit is really the regression line:

# this line has the equation   y' = a + bx
# y' is the predicted value of y (sometimes y-hat)

# when x = 0;  a is the y-intercept (where line crosses x-axis at 0)

# positive b = positive slope
# negative b = negative slope

# y is the 'outcome' variable
# x is the 'predictor' variable

# you'll also sometimes see b0 for a, and b1 for b.....



# The actual equation is:

# y' = a + bx + e 

# e is the error, or:
# y - y'  (the difference of each data point from the predicted value =  a residual)


# The estimated regression coefficients,  a & b, 
#  are those that minimise the sum of the squared residuals 






## Running a Regression Model in R.


mod1 <- lm(dan.grump ~ dan.sleep, data=df)  # build regression model

mod1  

mod1$coefficients

# intercept = a = 125.96
# dan.sleep  = b = -8.94

# therefore:   y' = 125.96 + -9.84x


## Just a small technical detail.....
# these regression coefficients are 'estimates'
# we are trying to estimate the 'parameters' that are true for a population




### OK.  These are just estimates... Are they any good ?



summary(mod1)


# Finally, we have a confidence interval around 'b'
confint(object = mod1, level = .95)


# In brief:

# Residuals:  broad descriptive info of variability

# Coefficients:  
# dan.sleep = b = estimate and standard error of the estimate
# t-value and p-value for how meaningful that estimate is different from 0.

# Residual Standard Error = Standard Error of the Estimate = Sigma_est
# a standardized measure of how far each residual is on average from the regression line.

# d.f. = n - number of parameters being estimated, = 100 - 2 = 98


# Multiple R2  =  measure of how much overall variation in dan.grump is attributable to dan.sleep


# F-statistic =  A goodness of fit of overall model.




##### Try for yourself examples --------------------------


## read in the data BlueJays.csv

jays <- read_csv("data/BlueJays.csv")
head(jays)

# make the scatterplot
ggplot(jays, aes(x= Mass, y = Head)) + 
  geom_point() +
  stat_smooth(method='lm', se=F)


# perform a linear regression, examining if mass predicts head size.

mod <- lm(Head ~ Mass, data = jays)

# examine the output - what are 'a' and 'b' for the regression?
mod


# get the estimates and other linear model output summary:

summary(mod)

# what is the R2?
# what is the standard error of the estimate?
# what are the degrees of freedom?
# Is the model a good fit?

# is the slope significantly different from 0 ?

# get the confidence intervals for the slope b?

confint(mod)
