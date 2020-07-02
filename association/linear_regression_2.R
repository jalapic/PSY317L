
### Residuals & R2



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




### How to calculate the residuals:


X <- df$dan.sleep  # the predictor
Y <- df$dan.grump  # the outcome

Y.pred <-  125.96   +   (-8.94 * X)


Y.pred

df$Y.pred <- Y.pred


head(df)

tail(df)


# so to get the residual,  y - y'
df$dan.grump - Y.pred

df$residuals <- df$dan.grump - Y.pred

head(df)

tail(df)


## These residuals are called 'ordinary residuals' or 'raw residuals'



### Let's visualize them.

p1 <- ggplot(df, aes(x = dan.sleep, y = dan.grump)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +   
  geom_segment(aes(xend = dan.sleep, yend = dan.grump-residuals), alpha = .5, color='red') + 
 # geom_point(aes(y = dan.grump+residuals), shape = 1) +
  theme_classic() +
  ggtitle("OLSR best fit trendline")

p1



###  What about other possible trendlines?  

# What would the size of these red residual lines be for those?


# how we do this, is to compare our residuals to a flat trendline at the Y mean.

mean(df$dan.grump)

ggplot(df, aes(x = dan.sleep, y = dan.grump)) + 
  geom_point() + 
  geom_hline(yintercept = mean(df$dan.grump), color='blue') 

# we can work out what the 'residuals' would be for this trendline:

head(df)

df$Ymean <- mean(df$dan.grump)

head(df)

df$resid_Ymean <- df$dan.grump - df$Ymean  #residual from Ymean


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


### Instead of using 'raw residuals', one way to measure how far away residuals are is to square them

# Squaring the residuals:

head(df)

df$residuals2 <- df$residuals ^ 2               # raw residuals

df$resid_Ymean2 <- df$resid_Ymean ^ 2           # total residuals

head(df)

# we want the raw residuals to be as small a fraction as possible of the total residuals



SS.resid <- sum(df$residuals2)
SS.resid  #1838.722

SS.tot <- sum(df$resid_Ymean2)
SS.tot    #9998.59


# to make these numbers interpretable, we convert them to R2.

# if the trendline is absolutely useless at predicting the y values,
# then the trendline would have residuals as high as the total residuals.

# if the trendline is perfect at predicting the y values, then the residual SS total would be 0.

1 -  (SS.resid/SS.tot)   # 0.816   (this is R2)


# remember our linear model:
mod1

summary(mod1)  # the R2 matches



## Remember, a short hand way of getting R2 is to square the Pearson r:
cor(df$dan.sleep, df$dan.grump)

r <- cor(df$dan.sleep, df$dan.grump)

r

r^2



