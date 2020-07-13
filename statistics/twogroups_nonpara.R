## Non-parametric Alternatives to the Two Sample t-tests


# This is just how to implement the tests
# I'm not dicussing the theory here


library(tidyverse)


### Example Data

# Anxiety Scores 1hour after taking placebo or drug

placebo <- c(15, 16, 19, 19, 17, 20, 18, 14, 18, 20, 20, 20, 13, 11, 16, 19, 19, 16, 10) 

drug <- c(15, 15, 16, 13, 11, 19, 17, 17, 11, 14, 10, 18, 19, 14, 13, 16, 16, 17, 14, 10, 14)



# put into dataframe - long format
df <- data.frame(anxiety = c(placebo, drug),
           group = c(rep("placebo", length(placebo)), 
                     rep("drug", length(drug))
                     )
           )

df


## Plot Data of anxiety by group

ggplot(df, aes(x=group, y=anxiety, fill=group)) + 
  geom_boxplot(outlier.shape = NA, alpha=.4) +
  geom_jitter(width=.1) +
  theme_classic() +
  scale_fill_manual(values=c("orange", "brown"))
  



# test if each group is approximately normally distributed

shapiro.test(drug)    # approximately normal as p>.05
shapiro.test(placebo) # not enough evidence to be normal as p<.05



## If we shouldn't proceed with a 2 sample t-test, what should we do?


# (we could do a permutation test).....





#### Non-parametric Tests....

## Wilcoxon Rank Sum Test  (aka Mann-Whitney U test)

## Examines difference in Medians for 2 independent populations


wilcox.test(placebo, drug) # 2-tailed

# W is the test-statistic,  p = 0.0191


wilcox.test(placebo, drug, conf.int = T) # if want confidence intervals and estimate of difference.


wilcox.test(placebo, drug, conf.int = T, exact=F) # this gets rid of obnoxious warnings, but not a big deal





# we can also do it with this syntax when data is in long format:
wilcox.test(anxiety ~ group, data=df) # 2-tailed


wilcox.test(anxiety ~ group, data=df, conf.int=T) # 2-tailed with confidence intervals



# should you need a 1-tailed test you can do it this way:

wilcox.test(placebo, drug, alternative = "greater") # 1-tailed


wilcox.test(anxiety ~ group, data=df, alternative = "less") # 1-tailed
wilcox.test(anxiety ~ group, data=df, alternative = "greater") # 1-tailed






### Effect Size for Wilcoxon Rank Sum / Mann Whitney Test

# for this test, it's effectively the degree to which one group has higher ranks of data than the other group

# there are many possible measures.

# here is one example 'r' (not the same as the correlation coefficient)

# this ranges from 0 to 1, with boundaries at 0.3, 0.5

# it's equal to the Z value calculated in the formula 
# divided by the total number of observations.

wilcox.test(anxiety ~ group, data=df, conf.int=T) 


library(rcompanion)

wilcoxonR(x  = df$anxiety,
          g  = df$group,
          ci = T)






######  Paired Data

## Likewise if Paired Data are not normal, you should not perform a paired t-test


## Instead, there is a Wilcoxon Signed Rank Test




#e.g. compare the values of immuncount to immuncount2

df1 <- read_csv("data/bloodwork.csv")
head(df1)

# both are highly non-normal
shapiro.test(df1$immuncount)
shapiro.test(df1$immuncount2)


# scatterplot
ggplot(df1, aes(x = immuncount, y=immuncount2 )) +
  geom_point() +
  theme_classic() +
  geom_abline(intercept =0 , slope = 1)


# to make slope graph
# we need long data to visualize:

df2 <- df1 %>% 
  select(ids, immuncount, immuncount2) %>%
  pivot_longer(2:3)



# better plot showing individual lines
ggplot(df2, aes(x=name, y=value, group=ids)) + 
  geom_point() + 
  geom_line() +
  theme_classic()



#### Now for the actual testing - 

median(df1$immuncount)
median(df1$immuncount2)



## this is essentiall looking at the overall number of difference scores that are negative vs positive



## Wilcoxon Signed Rank Test
wilcox.test(df1$immuncount, df1$immuncount2,  paired=T) #2-tailed

wilcox.test(df1$immuncount, df1$immuncount2,  paired=T, conf.int = T) #with CIs


# should you need a 1-tailed test you can do it this way:
wilcox.test(df1$immuncount, df1$immuncount2,  paired=T, alternative = "less") # 1-tailed
wilcox.test(df1$immuncount, df1$immuncount2,  paired=T, alternative = "greater") # 1-tailed



### Effect Size for Paired Wilcoxon Signed Rank Tests

# data need to be in long format



library(rcompanion)

# this is a bit annoying as it requires the data to be ordered specifically
# the top half of the data are the values for group1
# the bottom half of the data are the values for group2
# then the id variable needs to be in the same order for both groups

df2 <- df2 %>% arrange(name,ids)
df2


wilcoxonPairedR(x  = df2$value,
          g  = df2$name,
          ci = T)




