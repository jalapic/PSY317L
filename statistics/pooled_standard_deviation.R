#### Pooled Standard Deviation

# In Student's t-test, we assume that our two samples come from populations
# that have "equal variance". 

# i.e. their variance/standard deviations are the same.


# this enables us to use this estimate 
#  in calculating the Standard Deviation of the Sampling Distribution of Differences in Sample Means




## When we have two samples, they each have sample standard deviations, that are usually not equal:

# e.g.

samp1 <-  c(23, 25, 33, 19, 21, 27, 26, 31, 20)
samp1
mean(samp1)  # 25
sd(samp1)    # 4.82
length(samp1) # 9

samp2 <-  c(21, 22, 23, 19, 20, 24, 25, 21, 23, 22)
samp2
mean(samp2)  # 22
sd(samp2)    # 1.83
length(samp2) #10

# So, assuming they both come from populations with the same SD, how do we estimate that value?


## We have to "pool the standard devation"

# Essentially, get a value that is likely somewhere in between the two sample SDs.



#### Method 1.

# this way makes most sense to me:

# Standard Deviations are essentially the square root of the average squared deviation away from the mean...

# so, just recalculate from first principles.

# get the difference of every score from it's group mean, 
# then square, then add them all up, then divide by N-2, then square root


# I'm putting them into a dataframe to make it easier to look at:

df <- data.frame(samp1=c(samp1, NA), samp2)
df


# get the differences:


df$samp1_dif <- df$samp1 - mean(df$samp1, na.rm = T)
df$samp2_dif <- df$samp2 - mean(df$samp2, na.rm = T)

df


# now square them all:

df$samp1_dif2 <- df$samp1_dif^2
df$samp2_dif2 <- df$samp2_dif^2

df


# get the sum of all the squares:

sum(df$samp1_dif2, na.rm = T) 
sum(df$samp2_dif2)

ssq <- sum(df$samp1_dif2, na.rm = T) + sum(df$samp2_dif2)
ssq


# now get the 'average' squared deviation by dividing by n-2
# we do this to not underestimate the population SD
# but n-1 is not sufficient as we have two samples (and two sample means), so it's n-2

n2 <- length(samp1) + length(samp2) - 2
n2  #17

asd <- ssq/n2
asd

# get the pooled standard devation by squarerooting this:

sqrt(asd)  # 3.56


# Our estimate of the pooled standard deviation is 3.56

sd(samp1)
sd(samp2)




### There is a second way of calculating the Pooled SD.
# You can just do it from the summaries of means and standard deviations:

# it's just a weighted average of each sample's SD.


s1sd <- sd(samp1)
s2sd <- sd(samp2)

w1 <- length(samp1) - 1
w2 <- length(samp2) - 1

numerator <- (w1*s1sd^2) + (w2*s2sd^2)
denominator <- w1+w2

sqrt(numerator/denominator) #3.56








