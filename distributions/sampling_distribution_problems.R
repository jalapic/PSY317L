### Sampling distribution problem.

# The weight of chicken eggs is normally distributed with mean 60g and standard deviation of 3g.

# What is the probability of getting a batch of a dozen eggs that have a mean of less than 58g ?

set.seed(1) # so you get the same values as my script


### First, I'll make some plots of the 'population' and 'sampling distribution'


## Population
x <- rnorm(1000000, mean = 60, sd = 3)

ggplot(data.frame(x), aes(x = x)) +
  geom_histogram(color='black', fill='mistyrose', alpha=.4)+
  geom_vline(xintercept = 60, lwd=1)


## Sampling Distribution of sample means with Sample size of 12 (a dozen).

results<-vector('list',10000)
for(i in 1:10000){
  results[[i]] <- mean(sample(x, 12, replace = T))
}

res <- unlist(results)

ggplot(data.frame(res), aes(x=res)) +
  geom_histogram(color="black", fill='lightseagreen', alpha=.4)+
  geom_vline(xintercept = mean(res),lwd=1) +
  ggtitle("Sampling Distribution of Sample Means") 


# What proportion of samples of size 12, would have a mean of < 58g?

ggplot(data.frame(res), aes(x=res)) +
  geom_histogram(color="black", fill='lightseagreen', alpha=.4)+
  geom_vline(xintercept = mean(res),lwd=1) +
  ggtitle("Sampling Distribution of Sample Means") +
  geom_vline(xintercept = 58, color = 'red', lty=2, lwd=1)


# Well, I could just look at how many out of 10,000 simulated samples had means of less than 58g
sum(res<58)/10000  # that's 0.011 or 1.1% of samples.


## However, we can assume that the sampling distribution is Normally Distributed.
# This is because of Central Limit Theorem.

# So, we can convert to z-scores.

# what is 58g as a z-score in the sampling distribution?
# We need to know the mean, and the SD of the sampling distribution.

# these are the values from our simulated data:
mean(res)
sd(res)

# but we can also get these without simulating:

mean(x) # we assume that the sampling distribution mean is the same as the population mean (pop mean = 60)

sd(x) # the pop standard deviation is 3.0

3 / sqrt(12)  # this is the SD of the sampling dist of sample means (also called standard error)

sem <- 3 / sqrt(12) 
sem

# so, 58g as a z-score is...

(58 - 60) / sem  # -2.31

z <- (58 - 60) / sem  # -2.31

z

pnorm(z)  # prob = 0.010

pnorm(-2.31)

