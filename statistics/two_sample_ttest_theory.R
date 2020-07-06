

### Explaining the Student t-test


## Let's look at our data:

anastasia <- c(65, 74, 73, 83, 76, 65, 86, 70, 80, 55, 78, 78, 90, 77, 68)
bernadette <- c(72, 66, 71, 66, 76, 69, 79, 73, 62, 69, 68, 60, 73, 68, 67, 74, 56, 74)


mean(anastasia)            # 74.5
sd(anastasia)              # 9.0
nA<-length(anastasia)      # 15
nA

mean(bernadette)           # 69.1
sd(bernadette)             # 5.8
nB <- length(bernadette)   # 18
nB


# plot the data:
d <- data.frame(values = c(anastasia, bernadette),
                group = c(rep("anastasia",15), rep("bernadette", 18))
)

d

ggplot(d, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha=.3) +
  geom_jitter(width=.1, size=2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))




# what's the difference in means?

mean(anastasia)  - mean(bernadette)   # 5.48

# anastasia students have on average 5.48 higher scores.


## But how meaningful is this difference?




#### Student's t-test approach:

## we're going to work out how usual/unusual our one observed sample mean difference is.

# we need to construct the sampling distribution of differences in sample means.

# we hypothesise that its mean is 0 (no difference between groups)

# we have to work out the standard deviation of the sampling dist....


# If we assume equal variances between the population of group A and B, we can calculate the
# standard deviation of this sampling distribution as

# the pooled estimate of the common standard devation * sqrt(1/n1 + 1/n2)



## step 1:  calculate the pooled SD between the two samples...

# from first principles, calculating deviations from each group mean
difA2 <- (anastasia - mean(anastasia))^2
difB2 <- (bernadette - mean(bernadette))^2
sumsq <- sum(difA2) + sum(difB2)
n <- nA + nB #33
sd.pool <- sqrt(sumsq/(n-2))

sd.pool # 7.41  this is the estimated pooled s.d.


sd(anastasia) #8.999
sd(bernadette) #5.775



## step 2: use the pooled SD to calculate the S.D. of the Sampling Dist.

sedm <-  sd.pool * sqrt( (1/nA) + (1/nB))

sedm  # this is the Standard Deviation of the Sampling Distribution of differences in sample means




### We can now visualize this theoretical Sampling Distribution:



##  Plotting our Sampling Distribution of Differences in Sample Means

# Don't worry about the gross loooking code... just using it to make the plot:

m <- 0 # mean
v <- sedm^2 # variance, sedm squared
df <- 31
vals <- rt(n=500000, df=df)*sqrt(v * (df-2)/df) + m
df1 <- data.frame(val = vals)


ggplot(df1, aes(x=val)) +
  geom_histogram(aes(y = ..density..), color='black', fill='purple', alpha=.3)+
  theme_classic()+
  geom_density(alpha = 0.7, fill = "white") + 
  geom_vline(xintercept = 5.48, lwd=1, color="red") + 
  geom_vline(xintercept = 0, lwd=1,lty=2, color='black')+
  xlab("Difference in Sample Means") +
  ylab("") +
  ggtitle("Sampling Distribution of Differences in Sample Means")






## Step 3... Calculate our observed t.

# the observed value of t, is how (un)expected our observed sample difference in means is...

# essentially we say how many SDs is our one observed sample mean difference from the mean?

tobs <- (mean(anastasia)  - mean(bernadette))  / sedm  

tobs # t = 2.1154


## Calculate the p-value

# we are concerned with knowning how much of the t distribution is greater than our observed t.

pt(tobs, df=n-2) # 0.9787353  - this is the proportion to the left.

1 - pt(tobs, df=n-2)   # 0.0213 # this is the one-tailed p-value


(1 - pt(tobs, df=n-2)) * 2  # p = 0.04253  # the two-tailed p-value


### Let's check with R's function:

t.test(anastasia, bernadette, var.equal = T)  # yes! t=2.1154, df=31, p=0.04253




#### We can visualize this sampling distribution in terms of t:


### make a t-distribution

t <- seq(-3,3,by=.01)
Density <- dt(t, df=31)
df <- data.frame(t,Density)
ggplot(df, aes(x=t,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=2.12, color='red',lwd=1 )











