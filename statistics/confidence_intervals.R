

### z- and t- distribution Confidence Intervals.

# Imagine with have a population of butterflies
# the wingspan has a population mean, mu, of 7.8cm
# the population sd, sigma, is 0.3cm

# we take one sample of butterflies of size n=15.
# our sample mean is 7.72



# 1. Construct 95% Z-distribution confidence interval around this estimate.

#  CI  =  x_bar +/-  z * sem
#  sem = sigma/sqrt(n)


# i) x_bar = 7.72

# we assume our one sample mean, was one possible sample mean from a sampling distribution
# we assume that sampling distribution is normally distributed
# for our CI we assume it's mean is 7.72 (our sample mean)
# the SD of that sampling distribution of sample means = standard error = sigma/sqrt(n)

sem <- 0.3 / sqrt(15)
sem


# Because our sampling distribution is normal, 
# we can use the standard normal distribution to work out how many SD 
# either side would give us boundaries that contain 95% of the data within them.


qnorm(.975) #1.96   # this leaves 2.5% in the tail.


# our margin of error is:
1.96 * sem

# therefore our CI is:

7.72 + (1.96 * sem)
7.72 - (1.96 * sem)


# 7.72 +/- 0.15
# 7.72 [7.57, 7.87]  # this contains our true population mean.


# What if our sample mean had been 7.95 ?

7.95 + (1.96 * sem)
7.95 - (1.96 * sem)  

# 7.95 [7.798, 8.102]  # this contains our true population mean.


### A 95% Confidence Interval refers to - 
# 95% of samples will have the true population mean in their Confidence Interval.



## What happens to CI with larger sample size? e.g. n=50

# our standard error gets smaller.
sem1 <- 0.3 / sqrt(50)
sem1

# so our margin of error gets smaller:
1.96 * sem1

# our confidence interval gets tighter:
7.72 + (1.96 * sem1)
7.72 - (1.96 * sem1)

# 7.72 +/- 0.083
# 7.72 [7.637, 7.803]  # this contains our true population mean - just.


# Would this tightening of CI cause us to 'miss' the population mean more with our CI?
# No - because as the sample size increases, 
#  the estimate of the sample means gets closer to the true pop mean.


## What about other Confidence Interval %s?

# e.g. a 99% CI would mean that you capture the true mean in 99% of samples with their CI.
# here, you would use a value of 'z' that leaves 0.5% in each tail, i.e. 99% in the middle.
# how many SD away from the mean do you need to be, to make sure you have covered 99% of the distribution
# (49.5% on each side of the mean)

qnorm(.995) #2.576

# so our margin of error gets bigger:
2.576 * sem  # 0.20  (bigger than 0.15 for 95% CI)

# our confidence interval gets tighter:
7.72 + (2.576 * sem) #7.92
7.72 - (2.576 * sem) #7.52

# 7.72 [7.52, 7.92]  99% CI

# 7.72 [7.57, 7.87]  95% CI





### t-distribution Confidence Intervals.

# all the above has relied on us 'knowing' the population SD sigma.
# however, we rarely do.

# we use t-distribution to calculate CIs when we do not know sigma.


# In this case, our sampling distribution is not quite normal, but t-shaped
# it is t-shaped with d.f.  of n-1.

# The difference betwen t-shaped and normal is that the tails of the t-distribution are heavier.

# this means, that to have e.g. 2.5% in each tail, you need to be more SDs away from the mean.
# this will ensure 95% of the distribution in the middle.

# so, consequently, values of 't' are higher than their corresponding values of 'z'.

# these values of 't' change according to the sample size (and degrees of freedom).

## Formula:


#  CI  =  x_bar +/-  t * sem
#  sem = s/sqrt(n)


## Example 1.

# 95% CI for the following:

# sample mean = 7.72
# sample sd = 0.285
# n = 15


# i. what value of 't' leaves 95% in middle of distribution for d.f.=14

qt(.975, df=14) # 2.144787  (notice it's a bit bigger than 1.96)

sem2 <- 0.285 / sqrt(15)

# margin of error
2.144787 * sem2

# upper bound
7.72 + (2.144787 * sem2)

# lower bound
7.72 - (2.144787 * sem2)

# 7.72 +/- 0.158
# 7.72 [7.56, 7.88]  # this contains our true population mean.


# compare to the z - the t-dist based CI is a bit wider.
# 7.72 +/- 0.15
# 7.72 [7.57, 7.87]  # this contains our true population mean.




## Example 2:

# 95% CI for the following:
# sample mean = 115.5
# sample sd = 11.1
# n = 34

qt(.975, df=33) # 2.034515

sem3 <- 11.1 / sqrt(34)

# margin of error
2.034515 * sem3

# upper bound
115.5 + (2.034515 * sem3)

# lower bound
115.5 - (2.034515 * sem3)

# 115.5 +/- 3.87
# 115.5 [111.63, 119.37]  





## Example 3:

# 80% CI for the following:
# sample mean = 115.5
# sample sd = 11.1
# n = 34

qt(.90, df=33) # 1.307737

sem3 <- 11.1 / sqrt(34)

# margin of error
1.307737 * sem3

# upper bound
115.5 + (1.307737 * sem3)

# lower bound
115.5 - (1.307737 * sem3)

# 115.5 +/- 2.49
# 115.5 [113.01, 117.99]  # an 80% CI has a much tighter CI

# a tighter CI here means more chance of misssing the true population mean though.





#### Let me just show you how we do this in R usually:

vals <-
c(98.34456, 119.51556, 106.89230, 123.18029, 118.94992, 114.65976, 
  108.79470, 119.06786, 107.77041, 108.45470, 140.06593, 113.18642, 
  137.11072, 110.20815,  99.15518, 120.20461, 131.97015, 110.39190, 
  103.14745, 129.12144, 111.11100, 122.10201, 100.38841, 107.83489, 
  102.92158, 100.15357, 125.24278, 117.46616, 106.33658, 122.47705, 
  131.18588, 111.12131, 127.48950, 120.97727)


vals

mean(vals)
sd(vals)
length(vals)

# we just did this 'by hand' and got:
# 115.5 [111.63, 119.37]  
# for a 95% CI using the t-distribution.

t.test(vals)

# 95 percent confidence interval:
#   111.627 119.373