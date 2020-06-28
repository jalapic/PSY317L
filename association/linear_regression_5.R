
### Examining individual predictor estimates


##  The standard error of b and the t-statistic


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


# Run the model
mod1 <- lm(dan.grump ~ dan.sleep, data = df)

mod1

mod1$coefficients

b <- mod1$coefficients[2]

b

summary(mod1)


### What we are ultimately interested in is whether our estimate of b could potentially be 0
# our null hypothesis is that b = 0.


## Let's understand what std.error of 'b' really is first....


# - we could imagine that our estimate of 'b' is just one possible estimate from a sampling distribution of b
# - that is our 100 data points were used to get an estimate of 'b'
# - but we could have collected 100 different points and got lots of different 'b's...
# - CLT tells us that the sampling distribution for 'b' should be normal.
# - we're testing that the mean of that sampling distribution is 0.
# - then, if we know the standard error of that distribution, we could calculate a 't'

# t = b / se(b)

# that would be a measure of how typical/atypical our obsereved estimate of 'b' was....

# it turns out that the t-shaped sampling distribution has d.f. n - number of predictors - 1
# (a bit different to e.g. a one-sample-t-test)


## It turns out that calculating the standard error of 'b' is a bit annoying....


# 1. you could just look at the summary in R:

summary(mod1)


## 2. you can also use a shortcut formula to calculate it, using the correlation coefficient

r <- cor(df$dan.sleep, df$dan.grump)

r

n <- nrow(df)

n

(r * sqrt(n-2)) / (sqrt(1-r^2))  # -20.85




## 3. Here is the annoying way how to calculate it manually:

x_dif <- df$dan.sleep - mean(df$dan.sleep) # difference of each x from mean of x
x_dif

sum(x_dif^2) # sum of these squared differences 

ssx <- sqrt(sum(x_dif^2))  # square root this sum
ssx

summary(mod1) # we need the standard error of the estimate
sest <- summary(mod1)[6]$`sigma`
sest

sb <- sest/ssx
sb   # this is the standard error of b

summary(mod1)


###  Once you have sb, you calculate 't' in the following way

b  #-8.94
sb #0.4285

t <- b/sb
t  #-20.854

pt(t, df=98) # gives the exact p-value



### Finally, we can calculate a confidence interval around 'b'.

# remember, 'b' is just an estimate of a population parameter (the real b)
# so we should put a CI around it.


# we make the CI in the usual way:

# CI(b)  = b +/-  t * SE(b)

# We've already calculated b and se(b)
# t is the critical value of t for a 2-tailed test
# for a 95% CI, we get the value at 97.5% of the t distribution for df = n - 2

# easy way - gives us the confidence interval:
confint(object = mod1, level = .95)


# by hand - for d.f = 98:

tval <- qt(.975, df = 98)
tval

b + (tval * sb)

b - (tval * sb)


