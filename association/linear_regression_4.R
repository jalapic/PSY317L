
### Goodness of Fit Test - F-ratio



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


##### Add in the residuals

X <- df$dan.sleep  # the predictor
Y <- df$dan.grump  # the outcome
Y.pred <-  125.97   +   (-8.94 * X)
df$residuals <- df$dan.grump - Y.pred
df$residuals2 <- df$residuals^2


head(df)

tail(df)

### Visualize the residuals


p1 <- ggplot(df, aes(x = dan.sleep, y = dan.grump)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +   
  geom_segment(aes(xend = dan.sleep, yend = dan.grump-residuals), alpha = .5, color='red') + 
  # geom_point(aes(y = dan.grump+residuals), shape = 1) +
  theme_classic() +
  ggtitle("OLSR best fit trendline")

p1



## Add in the residuals from the mean of Y

df$Ymean <- mean(df$dan.grump)
df$resid_Ymean <- df$dan.grump - df$Ymean  #residual from Ymean
df$resid_Ymean2 <- df$resid_Ymean ^ 2           # total residuals


head(df)


## visualize this:

p2 <- ggplot(df, aes(x = dan.sleep, y = dan.grump)) +
  geom_point() +
  geom_hline(yintercept = mean(df$dan.grump), color='blue') +
  geom_segment(aes(xend = dan.sleep, yend = dan.grump-resid_Ymean), alpha = .5, color='red') + 
  # geom_point(aes(y = dan.grump+residuals), shape = 1) +
  theme_classic() +
  ggtitle("Residuals to the mean Y")

p2



library(gridExtra)
grid.arrange(p1,p2,nrow=1)


### Adding up the Residuals


SS.resid <- sum(df$residuals2)
SS.resid  #1838.722

SS.tot <- sum(df$resid_Ymean2)
SS.tot    #9998.59



### The F-ratio is essentially a method of determining the proportion of 
# residual variance compared to total variance.


#The F-test is useful for checking that the model as a whole is performing better than chance

# To calculate:

SS.mod <- SS.tot - SS.resid
SS.mod  #8159.868


# we convert SS.mod and SS.resid to Mean Squares values by dividing by their degrees of freedom

# The d.f. for SS.mod  is the number of predictors in the model ( = 1 = dan.sleep)

# The d.f. for SS.resid is the number of datapoints, minus the number of predictors, minus 1.
# = 100 - 1 (dan.sleep) - 1 = 98

MS.resid <- SS.resid / 98
MS.resid  #18.76

MS.mod <- SS.mod / 1
MS.mod    #8159.868
 
Fval <-  MS.mod / MS.resid
Fval  #434.9

summary(mod1)


# The observed value of F is compared to
# the sampling distribution for  F if the null hypothesis is true.
# the null is that our model (trendline) is no better than random in fitting the datapoints
