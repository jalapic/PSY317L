### Correlation - Permutation Test

# You can generate significance for correlation in other ways

# let's use a small dataset as an example


library(tidyverse)

# just some data looking at 15 subjects
# they complete a task
# those that spent longer on it tend to get higher scores

df <-  read_csv("data/timescore.csv")

head(df)

# scatterplot
ggplot(df, aes(x = time, y = score)) + 
  geom_point() +
  stat_smooth(method = "lm", se=F)
 
# regular significance test
cor.test(df$time,df$score) #r=0.55, p=0.031

# data is normal (though n=15 is pretty small)
shapiro.test(df$time)
shapiro.test(df$score)


##### METHOD 2: MONTE CARLO PERMUTATION


head(df)

# perform one shuffle of 'score' and create new column

set.seed(1) # just doing this so all our results look same


df$score  # actual data in order

sample(df$score)  # actual data but order shuffled

df$shuffle1 <- sample(df$score) #create a new column with shuffled data

df

# this is what that new column looks like:
ggplot(df, aes(x = time, y = shuffle1)) + 
  geom_point() +
  stat_smooth(method = "lm", se=F)

cor.test(df$time, df$shuffle1) # now relationship is a bit negative

cor(df$time, df$score) # original is r = 0.56
cor(df$time, df$shuffle1) # shuffle 1 is r = -0.12

# we can do this many times
cor(df$time, sample(df$score)) # r = 0.35
cor(df$time, sample(df$score)) # r = 0.04
cor(df$time, sample(df$score)) # r = -0.06
cor(df$time, sample(df$score)) # r = 0.15

# etc etc

# This next bit of code does the above 10,000 times 
# and stores the results

results <- vector('list',10000)
for(i in 1:10000){
  results[[i]] <- cor(df$time, sample(df$score))
}

unlist(results) # this are the correlations for 10,000 shuffles


cor(df$time, df$score) # original is r = 0.56

# and we can plot the results...

results.df <- data.frame(x = unlist(results))
head(results.df)

ggplot(results.df, aes(x)) + 
  geom_histogram(color="darkgreen",fill="lightseagreen") +
  geom_vline(xintercept = 0.56, lwd=1, lty=2) +
  xlab("r") 

# how many of the shuffles were greater than our observed value ?

sum(unlist(results) > 0.56) #130 were greater.

# what proportion is that ?

sum(unlist(results) > 0.56)  / 10000  #0.013

# so our 'p-value' is p = 0.013.
# this is actually smaller than the original p-value of 0.031





