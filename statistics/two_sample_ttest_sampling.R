#### Visualizing Sampling Distribution in 2 Sample t-test



# Say we have two normally distributed populations, A and B.  

# Each has 500,000 subjects.

# A has a mean of 15, and a SD of 2.5
# B has a mean of 12, and a SD of 2.5


set.seed(1)

A <- rnorm(n = 500000, mean = 15, sd = 2.5)
B <- rnorm(n = 500000, mean = 12, sd = 2.5)


# let's look at our populations
A

B


# and their summary descriptive stats
mean(A)   #15
sd(A)     #2.5

mean(B)   #12
sd(B)     #2.5



## OK, the true difference between these groups in means is 

mean(A) - mean(B) #2.997


## But what if we took 1 sample of size 17 for group A, and 1 sample of size 20 for group B?

a1 <- sample(A, 17)
a1

b1 <- sample(B,20)
b1

mean(a1)
mean(b1)

mean(a1) - mean(b1)  # 3.33 (which is pretty close to the real difference !)



## Let's do it again.....

a2 <- sample(A, 17)
a2

b2 <- sample(B,20)
b2


mean(a2)
mean(b2)

mean(a2) - mean(b2)  # 4.39 (that's a bit bigger than the real difference)



## We can write this script more succinctly like this:
 
mean(sample(A,17)) - mean(sample(B,20))

# and if you do this many times, you'll get lots of differences in means between the samples


# If we did that 1000s of times, we'd get a "sampling distribution"
# it would be a "sampling distribution of the differences in sample means"

# we'll do it 10000 times:
# this might take 5-10 seconds...

difs <- vector('list', 10000)

for(i in 1:10000){
  difs[[i]] <-  mean(sample(A, 17)) - mean(sample(B, 20))
}
unlist(difs)   # our sampling distribution


# what's the average difference in sample means?

mean(unlist(difs))  # 3.0

# more or less the same as the real difference in the populations...



## let's plot them in a histogram:

df <- data.frame(dif = unlist(difs))

ggplot(df, aes(x=dif)) +
  geom_histogram(color='black', fill='dodgerblue', alpha=.5)+
  theme_classic() +
  ggtitle("Sampling Distribution of Difference in Sample Means") +
  geom_vline(xintercept = mean(unlist(difs)), lwd=1, color="red")


## So, we have a symmetrical sampling distribution of difference in sample means...
## with mean = 3.0



## What is the Standard Deviation of this distribution?

# ?!&*(&)@&&***!! aaggghhh (see separate video about this)

# in short, for our example we can work it out directly:
sd(unlist(difs))  # 0.82


# there is also something called "variance sum law" that gives us a shortcut:
# essentially, if you want to know the variance of the difference of two variables
# then it's equal to the sum of the variance of those two variables.
# the variance of the sampling dist of means of A is var(A)/17
# the variance of the sampling dist of means of B is var(A)/20


sqrt((var(A)/17) + (var(B)/20)) #0.82


 


##### Example 2: No differences in Population Means


# Say we have two normally distributed populations, C and D.  

# Each has 500,000 subjects.

# C has a mean of 10, and a SD of 3
# D has a mean of 10, and a SD of 3


set.seed(1)

C <- rnorm(n = 500000, mean = 10, sd = 3)
D <- rnorm(n = 500000, mean = 10, sd = 3)


# let's look at our populations
C

D


# and their summary descriptive stats
mean(C)   #10
sd(C)     #3

mean(D)   #10
sd(D)     #3



## OK, the true difference between these groups in means is 
mean(C) - mean(D) #0


# Let's look at one sample of size 11 for population C 
# and one sample of size 14 for population D

c1 <- sample(C, 11)
d1 <- sample(D, 14)

c1
d1

mean(c1)

mean(d1)

mean(c1) - mean(d1)  # 1.56


# if we did this lots of times, we'd sometimes get sample means that were larger in C and 
# sometimes they would be larger in D.


### Let's grab 10,000 samples and calculate mean differences

difs1 <- vector('list', 10000)

for(i in 1:10000){
  difs1[[i]] <-  mean(sample(C, 11)) - mean(sample(D, 14))
}
unlist(difs1)   # our sampling distribution


# what's the average difference in sample means?

mean(unlist(difs1))  #0.01

# more or less the same as the real difference in the populations...



## let's plot them in a histogram:

df1 <- data.frame(dif = unlist(difs1))

ggplot(df1, aes(x=dif)) +
  geom_histogram(color='black', fill='mistyrose', alpha=.5)+
  theme_classic() +
  ggtitle("Sampling Distribution of Difference in Sample Means") +
  geom_vline(xintercept = mean(unlist(difs1)), lwd=1, color="red")


## So, we have a symmetrical sampling distribution of difference in sample means...
## with mean = 0.0



## What is the Standard Deviation of this distribution?

sd(unlist(difs1))  # 1.2

# using our shortcut
sqrt((var(C)/11) + (var(D)/14)) #1.2




## So, theoretically these "sampling distributions of the difference in sample means" exist.

# The question becomes, what if you only have one sample of A & B, or one sample of C & D
# Would you be able to infer if there was really a population difference in means or not?














