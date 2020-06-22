### What is the Sampling Distribution ?



### Say this is our population...

# It has 1 million individuals with a mean of 100 and sd of 15

# e.g. adult Archerfish are 100mm long, with sd of 15mm.

x <- rnorm(n = 1000000, mean = 100, sd = 15)

x

# histogram of the population:

ggplot(data.frame(x), aes(x = x)) + 
  geom_histogram(color='black', fill='#f584ed', alpha=.5, binwidth = 5) +
  theme_classic() +
  xlab("Body Length mm") +
  ylab("Frequency") +
  ggtitle("Body Sizes of 1 million Archerfish")



## What is the population mean body length?

mean(x)


## If you took a sample of 10 fish, what would the mean be?

sample(x, 10, replace = T)  # do this a few times.

samp <- sample(x, 10, replace = T)  # do this a few times.
samp
mean(samp)

# written more succinctly:
mean(sample(x, 10, replace = T))  # do this a few times.


### What if we were to collect 10,000 samples
# and for each one get the mean

results<-vector('list',10000)
for(i in 1:10000){
results[[i]]  <- mean(sample(x, 10, replace = T))  
}

unlist(results)

res <- unlist(results)

mean(res)
sd(res) #standard deviation of the sampling distribution, aka standard error



### Let's Draw this as a histogram.

p1 <- ggplot(data.frame(res), aes(x = res)) + 
  geom_histogram(color='black', fill='#4adbe0', alpha=.5, binwidth = 1) +
  theme_classic() +
  xlab("Mean Body Length of each sample - mm") +
  ylab("Frequency") +
  ggtitle("Sampling Distribution of Sample Means for n=10")

p1

# the sampling distribution mean == population mean

# this SD of the sampling distribution, is equal to
# the population SD divided by square root of the sample size.

mean(res)
sd(res)

15/sqrt(10)   # pop st dev divided by square root of sample size





####  What happens to this distribution when you increase the Sample Size?

x

sample(x, 50, replace = T)

mean(sample(x, 50, replace = T))

# ok, get 10,000 samples of size 50
results1<-vector('list',10000)
for(i in 1:10000){
  results1[[i]]  <- mean(sample(x, 50, replace = T))
}

res1 <- unlist(results1)

mean(res1)  # still same as population mean

sd(res1)    # smaller with larger sample size
15/sqrt(50) # smaller because sample size higher

# plot histogram
p2 <- ggplot(data.frame(res1), aes(x = res1)) + 
  geom_histogram(color='black', fill='#4adbe0', alpha=.5, binwidth = .5) +
  theme_classic() +
  xlab("Mean Body Length of each sample - mm") +
  ylab("Frequency") +
  ggtitle("Sampling Distribution of Sample Means for n=50")

p2

library(gridExtra)
grid.arrange(p1,p2,nrow=1)

# notice that the sampling distribution is "tighter" for higher sample sizes.

