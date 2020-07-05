#### t-test Permutation


### The principle of permutation tests can be applied to t-tests also


library(tidyverse)

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
dd <- data.frame(values = c(anastasia, bernadette),
                 group = c(rep("Anastasia",15), rep("Bernadette", 18))
)

dd

ggplot(dd, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha=.3, outlier.shape = NA) +
  geom_jitter(width=.1, size=2) +
  theme_classic() +
  scale_fill_manual(values = c("firebrick", "dodgerblue"))


## assume mean of sampling distribution is

meandif <- mean(anastasia)  - mean(bernadette)   # 5.48
meandif



### If we ran a traditional Student t-test, we'd get the following:

t.test(anastasia, bernadette, var.equal = T)  # t = 2.114, p=0.04253



### With a permutation test, we could imagine...
# how likely were the scores to fall in the way they did by chance?

length(anastasia)
length(bernadette)

c(anastasia, bernadette)  # we have 33 scores in total, with 15 in one group and 18 in another.


# what if we shuffled these scores into different groups of 15 and 18. What would the difference in means be?

allscores <- c(anastasia, bernadette)

allscores

split(allscores, rep(1:2, c(15,18))) # this line splits the scores into groups of 15 and 18....but....

#.... notice, that it's still ordered in the same way.


# So we shuffle:

sample(allscores)  # do this a few times...

split(sample(allscores), rep(1:2, c(15,18))) # do this a few times....


# We're now getting different samples of 15 and 18, using our original data...



## Let's grab one of these shuffles:

set.seed(1)  # this just means if you do this you get the same results as me

x <- split(sample(allscores), rep(1:2, c(15,18)))

x

x[[1]] # this is our shuffled sample of size 15
x[[2]] # this is our shuffled sample of size 18

# what's the difference in their means?

mean(x[[1]]) - mean(x[[2]])  #1.567  - smaller than our obseved value of 5.48



### Let's do it again!

x <- split(sample(allscores), rep(1:2, c(15,18)))
mean(x[[1]]) - mean(x[[2]])  #-4.056  - much smaller than our obseved value of 5.48 !


## Keep doing it...

x <- split(sample(allscores), rep(1:2, c(15,18)))
mean(x[[1]]) - mean(x[[2]])  



# In fact, we can do it, say, 10000 times...

results<-vector('list',10000)
for(i in 1:10000){
  x <- split(sample(allscores), rep(1:2, c(15,18)))
  results[[i]]<-mean(x[[1]]) - mean(x[[2]])  
}

unlist(results) # these are all our mean differences from 10,000 shuffles of the data.



## We can graph this:

df <- data.frame(difs = unlist(results))

ggplot(df, aes(x=difs)) +
  geom_histogram(color="black", fill="green", alpha=.4) +
  geom_vline(color="navy",lwd=1,lty=2,xintercept = 5.48) +
  theme_classic()+
  ggtitle("Mean Differences from \n 10000 Permutations of Raw Data")


# so how many times did we get a difference in means that was higher than 5.48?

sum(unlist(results) > 5.48)  # 202 times out of 10000

sum(unlist(results) > 5.48) /10000  # which is 0.0202 proportion of the time

# this is sort of equivalent to a 1-tailed p-value ....

# we could say p=0.020





