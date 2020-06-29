
#### Unusual Datapoints


# Generally linear regression models should not be overly affected by individual data points

# but there are different types of 'unusual' observations


# i. outliers
# ii. high leverage
# iii. high influence



##  i. Outliers --

# typically highly unusual in terms of outcome (y) but not in terms of predictor (x)


x <- c(4.1,4.2,5,5.5,6,6.1,6.15,6.4,7,7.2,7.7,8,9.7)
y <- c(3.9,4.3,6,5.9,6.1,11.9,6.3,5.8,7.9,6.4,7.8,8,9.1)
y1 <- c(3.9,4.3,6,5.9,6.1,NA,6.3,5.8,7.9,6.4,7.8,8,9.1)

df <- data.frame(x,y,y1)
df

mod <- lm(y~x, data = df)
mod


mod1 <- lm(y1~x, data = df)
mod1

coms <- coefficients(mod1)
coms

x6y <- coms[1] + (x[6]*coms[2]) # to get the predicted value for x=6.1


ggplot(df, aes(x = x, y = y)) + 
  geom_point(size=2) +
  stat_smooth(method="lm",se=F) +
  theme_classic() +
  geom_abline(intercept = coms[1], slope = coms[2], color = 'red', lty=2)+
  geom_segment(aes(x = x[6], y = y[6], xend = x[6], yend = x6y), lty=2, alpha=.4)




##  ii. High Leverage --

# a datapoint is very different to the others
# it doesn't necessarily have to have a high residual
# an example might be a point that has unusually large x and y.

# high leverage points are those that have a high influence on the regression line's trajectory
# not necessarily affecting the angle of the slope

# it's measured by a value called 'hat value' or h.

hatvalues(mod)  # the outlier doesn't actually have a large leverage 
                # it's not overly influencing the trajectory of the regression line
                # point 13, x=9.1, y=9.1, has a higher leverage.


# roughly speaking, a large hatvalue is one which is 2-3 times the average hat value

mean(hatvalues(mod))


hatvalues(mod1)
mean(hatvalues(mod1))




## iii. High Influence --

# essentially a high leverage point that is an outlier.


x <- c(4.1,4.2,5,5.5,6,6.1,6.15,6.4,7,7.2,7.7,8,9.7)
y2 <- c(3.9,4.3,6,5.9,6.1,NA,6.3,5.8,7.9,6.4,7.8,8,6.1)
y3 <- c(3.9,4.3,6,5.9,6.1,NA,6.3,5.8,7.9,6.4,7.8,8,NA)

df1 <- data.frame(x,y2,y3)
df1

mod2 <- lm(y2~x, data = df1)
mod2

mod3 <- lm(y3~x, data = df1)
mod3

coms3 <- coefficients(mod3)
coms3

x13y <- coms3[1] + (x[13]*coms3[2]) # to get the predicted value for x=9.7


#scatterplot

ggplot(df1, aes(x = x, y = y2)) + 
  geom_point(size=2) +
  stat_smooth(method="lm",se=F) +
  theme_classic() +
  geom_abline(intercept = coms3[1], slope = coms3[2], color = 'red', lty=2)+
  geom_segment(aes(x = x[13], y = y2[13], xend = x[13], yend = x13y), lty=2, alpha=.4)


hatvalues(mod2) # datapoint 13 has high leverage, it's also an outlier.

cooks.distance(mod2) # measures influence - biggest concern to regression models

# typically a cook's distance > 1 requires more consideration.


# also plot like this

plot(mod2, which = 4)



### Let's look at the Sleep vs Grumpiness Data:

newdf <- read_csv("data/parenthood.csv")

head(newdf)


# Scatterplot
ggplot(newdf, aes(x = dan.sleep, y = dan.grump)) + 
  geom_point() + 
  stat_smooth(method='lm', se=F) 


new.mod <- lm(dan.grump ~ dan.sleep, data = newdf)
new.mod

hatvalues(new.mod)


cooks.distance(new.mod)
plot(new.mod, which = 4)
