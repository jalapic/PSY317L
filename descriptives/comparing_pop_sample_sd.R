### Why we need the Sample Standard Deviation.

source("descriptives/population_sd.R") # to get the populationSD function

# start with pop vs sample SD formulas.


# the sample SD is an estimate of the population SD

x <- c(14, 11, 5, 3, 8, 10, 9, 15)

x

pop.sd(x)  # population SD = 3.839

sd(x) # sample SD = 4.104




### Would the difference be as big with a bigger sample ?

set.seed(1) # just so we all get the same results
x <- rnorm(100, mean = 8) #100 random numbers with mean of 8.
x

pop.sd(x)  # population SD = 8.894

sd(x)      # sample SD = 0.898

# Sample SD is larger than population SD
# but this difference gets smaller as sample sizes increase.




#### Visual Proof that using the population SD underestimates for samples

# Say we have a population of 1000.
# Here are all the values
# The population has a mean of 199.91 and population SD of 8.28.

set.seed(1)
population <- rnorm(1000, mean = 200, sd = 8)

population

mean(population)  #199.91
pop.sd(population)    #8.28


# Say you did not know the real population SD.
# And you were going to take samples of size 15 
# to try and estimate the population mean and SD

s1 <- sample(population, 15, replace = T)

s1

mean(s1) #204.4
pop.sd(s1)  #11.9
sd(s1)   # 12.3


# Let's try another sample:

s2 <- sample(population, 15, replace = T)

s2

mean(s2) #195.0
pop.sd(s2)  #7.2
sd(s2)   # 7.5

# So, in one sample the sample SD was closer to the real pop SD
# in the other, the popSD was closer to the real pop SD

# the sample mean is closish to the population mean.


## Let's do this for 10,000 samples:
# and for each, calculate the sample mean, sample SD, and popSD

results.means<- vector('list',10000)
results.popSD<- vector('list',10000)
results.sampSD<- vector('list',10000)

for(i in 1:10000){
  s <- sample(population, 15, replace = T)
  results.means[[i]] <- mean(s)
  results.popSD[[i]] <- pop.sd(s)
  results.sampSD[[i]] <- sd(s)
}


# remember the actual population has 
# population mean = 199.91
# population sd = 8.28

means <- unlist(results.means) # sample means
means
mean(means) #199.9

popSDs <- unlist(results.popSD) # SDs using popSD
popSDs
mean(popSDs) #7.84

sampSDs <- unlist(results.sampSD) # sample SDs
sampSDs
mean(sampSDs) #8.12


#### Let's visualize:


#### Means
ggplot(data.frame(means), aes(x=means)) +
  geom_histogram(color='black', fill='blue', alpha=.2)+
  theme_classic() +
  geom_vline(xintercept = mean(means), lwd=1, color="black") +
  geom_vline(xintercept = 199.91, lwd=1, color="orange", lty=2) +
  ggtitle("Sample Means Distribution")
  
# so, the sample mean is a good estimate of the population mean, on average


#### Population SD
ggplot(data.frame(popSDs), aes(x=popSDs)) +
  geom_histogram(color='black', fill='blue', alpha=.2)+
  theme_classic() +
  geom_vline(xintercept = mean(popSDs), lwd=1, color="black") +
  geom_vline(xintercept = 8.28, lwd=1, color="orange", lty=2) +
  ggtitle("SD Distribution when using Population SD")

# using the population SD formula with samples, leads us to underestimating the real population SD - on average
# there is a lot of variation though.


#### Sample SD
ggplot(data.frame(sampSDs), aes(x=sampSDs)) +
  geom_histogram(color='black', fill='blue', alpha=.2)+
  theme_classic() +
  geom_vline(xintercept = mean(sampSDs), lwd=1, color="black") +
  geom_vline(xintercept = 8.28, lwd=1, color="orange", lty=2) +
  ggtitle("SD Distribution when using Sample SD")

# using the sample SD formula with samples, provides a better estimate of the population SD on average

# this is why we divided by n-1 for sample SD.




