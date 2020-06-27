
### Ordinary Least Squares Regression:

## Visualizing Residuals....

## & Understanding R2.



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




### Let's visualize the residuals in more detail:

head(df)

tail(df)



### But let me show you how they come about:


X <- df$dan.sleep  # the predictor
Y <- df$dan.grump  # the outcome

Y.pred <-  125.97   +   (-8.94 * X)


Y.pred

df$Y.pred <- Y.pred


head(df)

tail(df)


# so to get the residual,  y' - y
Y.pred - df$dan.grump

df$residuals <- Y.pred - df$dan.grump 

head(df)

tail(df)


## These residuals are called 'ordinary residuals' or 'raw residuals'



### Let's visualize them.

p1 <- ggplot(df, aes(x = dan.sleep, y = dan.grump)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +   
  geom_segment(aes(xend = dan.sleep, yend = dan.grump+residuals), alpha = .2, color='red') + 
 # geom_point(aes(y = dan.grump+residuals), shape = 1) +
  theme_classic() +
  ggtitle("OLSR best fit trendline")

p1



###  What about other possible trendlines?  

# What would the size of these red residual lines be for those?


# how we do this is to compare our residuals to a flat trendline at Y mean.


ggplot(df, aes(x = dan.sleep, y = dan.grump)) + 
  geom_point() + 
  geom_hline(yintercept = mean(df$dan.grump), color='blue') 

# we can work out what the 'residuals' would be for this trendline:

head(df)




SS.resid <- sum( (Y - Y.pred)^2 )

SS.tot <- sum( (Y - mean(Y))^2 )





# But what are these:

#you can actually get them like this:

mod1$residuals

df$residuals <- mod1$residuals


https://learningstatisticswithr.com/book/regression.html#regressionassumptions


https://rstudio-pubs-static.s3.amazonaws.com/295773_71ccdfba50b84d48a870c4b83c2cd96d.html#

https://training-course-material.com/training/Standard_Error_of_the_Estimate#:~:text=The%20standard%20error%20of%20the%20estimate%20is%20a%20measure%20of,of%20the%20average%20squared%20deviation.

https://bookdown.org/jefftemplewebb/IS-6489/linear-regression.html


