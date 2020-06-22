### Central Limit Theorem.

## Basically, no matter what our starting distribution,
# when we take a bunch of samples and calculate the sample mean for each
# the resulting distribution will be normal-ish.

# this is more true for larger samples.


### Example 1.

# here is a uniform distribution.
# it's 1 million numbers between 0 and 75

# e.g. trees distance from the center of a forest in km.

x <- runif(1000000, min = 0, max = 75)
x

# histogram
ggplot(data.frame(x), aes(x = x)) + 
  geom_histogram(color='black', 
                 fill = "#894ae0", 
                 alpha=.3, 
                 binwidth = 5,
                 boundary = 0) +
  theme_classic()


### Population Mean & SD
mean(x)  #37.5

sd(x) #21.66



### OK. Take a sample of 30.

sample(x, 30, replace = T) # do this a few times.

# you can calculate the mean for each sample
mean(sample(x, 30, replace = T)) # do this a few times.

# notice that they are sometimes a bit bigger than 37.5, 
# and sometimes a bit smaller.


## Let's get the mean of 10,000 samples---

results<-vector('list',10000)
for(i in 1:10000){
  results[[i]]  <- mean(sample(x, 30, replace = T))  
}

unlist(results)

res <- unlist(results)

mean(res)  # the mean of the sample means is close to 37.5, the population mean

sd(res)  #3.9 - this is a lot smaller than the population SD





## The sampling distribution of sample means ....

### Let's Draw this as a histogram.

ggplot(data.frame(res), aes(x = res)) + 
  geom_histogram(color='black', fill='#894ae0', alpha=.5, binwidth = 1) +
  theme_classic() +
  geom_vline(xintercept = mean(res), lwd=1) + 
  xlab("Mean of each sample") +
  ylab("Frequency") +
  ggtitle("Sampling Distribution of Sample Means for n=30")

  # This is the sampling distribution.

sd(x)/sqrt(30)  # standard error.
sd(res)






###### Let's try a different distribution.....

### e.g. a skewed distribution:

# e.g. Donations to a charity (in 10s of $s)

q <- rnbinom(1000000, 5, .4)

q

# histogram
ggplot(data.frame(q), aes(x = q)) + 
  geom_histogram(color='black', 
                 fill = "#1ad665", 
                 alpha=.3, 
                 binwidth = 1,
                 boundary = 0) +
  theme_classic()


### Population Mean
mean(q)  #7.5

sd(q)  #4.33


### OK. Take a sample of 30.

sample(q, 30, replace = T) # do this a few times.

# you can calculate the mean for each sample
mean(sample(q, 30, replace = T)) # do this a few times.

# notice that they are sometimes a bit bigger than 7.5, 
# and sometimes a bit smaller.


## Let's get the mean of 10,000 samples---

results<-vector('list',10000)
for(i in 1:10000){
  results[[i]]  <- mean(sample(q, 30, replace = T))  
}

unlist(results)

res <- unlist(results)

mean(res)  # the mean of the sample means is close to 7.5

sd(res)  #0.8

sd(q)/sqrt(30)  # standard error.


## The sampling distribution of sample means ....

### Let's Draw this as a histogram.

ggplot(data.frame(res), aes(x = res)) + 
  geom_histogram(color='black', fill='#31e8d0', alpha=.5, binwidth = .1) +
  theme_classic() +
  geom_vline(xintercept = mean(res), lwd=1) + 
  xlab("Mean of each sample") +
  ylab("Frequency") +
  ggtitle("Sampling Distribution of Sample Means for n=30")



