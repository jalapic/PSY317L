### One-sample t-test - bit more on the theory


# We want to test if our data sample could have come from a population with a specific hypothesized mean, or if it was unlikely to have come from that population.

# We need to imagine that our data sample is just one data sample that we could have theoretically collected from the population with that hypothesized mean.

# Our sample mean is therefore one sample mean we could have got from a sampling distribution of sample means.

# that sampling distribution has a t-distribution shape

# the mean of that sampling distribution is the hypothesized population mean.

# we calculate how unlikely our sample mean was to get from that sampling distribution by calculating the t-statistic - a measure of how many SD our sample mean is from the mean of the sampling distribution.



# e.g. The population mean number of words spoken by two year olds by their 2nd birthday is 50 words and is normally distributed.  

# A researcher wanted to investigate if reading to children increases their word knowledge. They collected data from 12 children who were read to for at least two hours every day. These are the number of words spoken by the 12 children:

x <- c(45, 53, 71, 35, 51, 59, 49, 55, 78, 27, 66, 59)

x

mean(x)  # 54, which is higher than 50.
# but is it meaningfully higher?


## Null H0:  mu <= 50
## Alternative H1:  mu > 50


# Our sample mean is one sample mean that could have come from the sampling distribution of sample means.

# What shape is that sampling distribution?

# t-distribution with df  n-1
# mean of the distribution is hypothesized mean 50.
# sd of the distribution is sampleSD/sqrt(n)


# We can standardize this sampling distribution to a t-distribution with mean 0.

mean(x)  #54
sd(x)  #14.4

n <- length(x)
n


t <-  (mean(x) - 50) / (sd(x) / sqrt(n))  #50 is hypothesized mean

t #0.96



#visualize
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dt, args = list(df = 11)) +
  xlab("t") +
  ggtitle("t-distribution for df = 11") +
  geom_vline(xintercept = t, color="orange", lwd=1)


# what proportion of curve is above t value ?

pt(t, df=11, lower.tail = FALSE)  # p value 1 tailed test - proportion above t
# 0.178

pt(t, df=11, lower.tail = TRUE) # proportion below t
# 0.822


t.test(x, mu = 50)  # two sided test

t.test(x, mu = 50, alternative = "greater")  # two sided test
