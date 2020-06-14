
### How Pearson Correlations are Calculated.


# this tutorial shows you how to calculate 'r' by hand...

# really just so you see what's going on.

# you would never do this to calculate r,
# this is just to help you understand it.



### Example 1.   Positive Correlation.

x <- c(1.1, 1.5, 2.1, 3.5, 3.6, 3.5, 2.6, 5.6, 4.4, 3.9)
y <- c(2.8, 2.9, 1.6, 5.5, 4.7, 8.1, 3.3, 7.7, 7.1, 5.8)

df <- data.frame(x, y)

df

ggplot(df, aes(x = x, y = y)) + geom_point(size=2)

cor.test(x,y)
cor.test(df$x,df$y)


# step 1. Calculate mean and sd of each column.

mean(x)
sd(x)

mean(y)
sd(y)


# step 2. Calculate z-scores of x, and z-scores of y.

(x - mean(x)) / sd(x)
(y - mean(y)) / sd(y)

df$zx <- (x - mean(x)) / sd(x)
df$zy <- (y - mean(y)) / sd(y)

df

# step 3. Calculate the cross-product:  zx * zy

df$zxzy <- df$zx * df$zy

df

# step 4.  Sum up the cross products.

sum(df$zxzy) # 7.58


# step 5- calculate 'r' by dividing by N-1. (for a sample)

sum(df$zxzy) / 9
sum(df$zxzy) / (nrow(df) - 1)

# r=0.84

cor.test(x,y)



###################

### Example 2.   Negative Correlation.

x <- c(1.1, 1.5, 2.1, 3.5, 3.6, 3.5, 2.6, 5.6, 4.4, 3.9)
y <- c(10.4, 10.0, 8.4, 8.5, 8.4, 6.3, 7.1, 6.2, 8.1, 10.0)

df <- data.frame(x, y)

ggplot(df, aes(x = x, y = y)) + geom_point(size=2)

cor.test(x,y)
cor.test(df$x,df$y) # r = -0.61

# Calculate z-scores for each x and each y

df$zx <- (x - mean(x)) / sd(x)
df$zy <- (y - mean(y)) / sd(y)

df

# Calculate the cross-product:  zx * zy

df$zxzy <- df$zx * df$zy

df

# Sum up the cross products.

sum(df$zxzy) # -5.5


# Calculate 'r' by dividing by N-1.

sum(df$zxzy) / 9
sum(df$zxzy) / (nrow(df) - 1)

# r = -0.61

cor.test(x,y)
