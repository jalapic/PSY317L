
### Standard Error of the Estimate, sigma_est, S.



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



### Add in the residuals

X <- df$dan.sleep  # the predictor
Y <- df$dan.grump  # the outcome
df$Y.pred <-  125.96   +   (-8.94 * X)  # add predicted Y
df$residuals <- df$dan.grump - df$Y.pred  # add residuals
df$residuals2 <- df$residuals^2  # residuals squared



head(df)

tail(df)



### Let's visualize them.

p1 <- ggplot(df, aes(x = dan.sleep, y = dan.grump)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +   
  geom_segment(aes(xend = dan.sleep, yend = dan.grump-residuals), alpha = .5, color='red') + 
  # geom_point(aes(y = dan.grump+residuals), shape = 1) +
  theme_classic() +
  ggtitle("OLSR best fit trendline")

p1






###  Standard Error of the Estimate.


# We sum the squared residuals, divide by N-2, and square root this number.

nrow(df)

sum(df$residuals2) # 1838.722

sum(df$residuals2) / 98   # 18.76

sum(df$residuals2) / (nrow(df)-2)  #18.76

sqrt(sum(df$residuals2) / (nrow(df)-2))  #4.33

s_est <- sqrt(sum(df$residuals2) / (nrow(df)-2))  #4.332

s_est #4.332


# this value is given in the summary to the lm:

summary(mod1) # the s_est is given as 'residual standard error'


## What to do with the Standard Error of the Estimate ?


# smaller numbers indicate a better model (but what's small?)
# s_est is in the original units of the Y-axis


# As a rule of thumb:
#  Approximately 95% of the observations should fall within 
#  plus/minus 2*standard error of the estimates from the regression line

mod1

ggplot(df, aes(x = dan.sleep, y = dan.grump)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  geom_abline(intercept = 125.956+s_est+s_est, slope = -8.937, color = 'red', lty=2)+
  geom_abline(intercept = 125.956-s_est-s_est, slope = -8.937, color = 'red', lty=2) +
  ggtitle("Regression with 2 x Standard Error of the Estimate")



