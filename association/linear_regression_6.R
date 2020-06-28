
#### Assumptions of Linear Regression


# 1. Normality (specifically the residuals)
# 2. Linearity
# 3. Homogeneity of Variance (homoscedasticity)
# 4. Uncorrelated Predictors (only relevant if doing more than one predictor)
# 5. No 'bad' outliers



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

summary(mod1)



### 1. Normality of Residuals ---

residuals(mod1)  # these are standardized residuals (but ok for now)

df$resids <- residuals(mod1)

#a) histogram plot
ggplot(df, aes(x=resids)) + geom_histogram(color='white') # possibly ok

#b) Shapiro-Wilk test
shapiro.test(df$resids)  # shapiro test says normal.

#c) QQ plot
qqnorm(df$resids)
qqline(df$resids, col = "steelblue", lwd = 2)  # it's ok

plot( x = mod1, which = 2 )  # fast way of getting same plot




### 2. Linearity ---

# X and Y should have a linear relationship

# a) scatterplot - especially if just one predictor:

ggplot(df, aes(x = dan.sleep, y = dan.grump)) + 
  geom_point() + 
  stat_smooth(method='lm', se=F) 



# b) There should also be a linear relationship between the actual Y values and the predicted Y' values

# we calculated these like this before:
coefficients(mod1)
df$Y.pred <- 125.956292 + (df$dan.sleep * -8.936756)
head(df)


# but you can get them directly like this:
fitted.values(mod1)

df$Y.fitted <- fitted.values(mod1)

head(df)

# plot this relationship
ggplot(df, aes(x = Y.fitted, y = dan.grump)) + 
  geom_point() + 
  stat_smooth(method='lm', se=F) 





### 3. Homogeneity of Variance / Homoscedasticity ---

# The model should be equally good at predicting Y's across all values of X.

head(df)

# if true, this should be a straight line
ggplot(df, aes(x = Y.fitted, y = resids)) + 
  geom_point() + 
  stat_smooth(method='lm', se=F) 


# an alternative plot of this is as follows:

plot(mod1, which = 3)

# there are some more formal tests of this... but we don't need to worry about them.





