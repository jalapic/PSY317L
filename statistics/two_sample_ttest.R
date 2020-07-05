levene test for equal variances




# you can write your formula differently depending on if you have wide or long data:



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

dd
t.test(values ~ group, data = dd)




### Comparing Medians or Means

library(tidyverse)

# some example data:

df1 <- read_csv("https://raw.githubusercontent.com/jalapic/IntroR/master/bloodwork.csv")
head(df1)



## COMPARING BETWEEN TWO INDEPENDENT GROUPS




## Plot Data of heart rate of smokers vs non-smokers

ggplot(df1, aes(x=smoker, y=hrate)) + geom_boxplot()




## Independent t-test

t.test(hrate ~ smoker, data=df1) # 2-tailed

# should you need a 1-tailed test you can do it this way:
t.test(hrate ~ smoker, data=df1, alternative = "less") # 1-tailed
t.test(hrate ~ smoker, data=df1, alternative = "greater") # 1-tailed




## Wilcoxon Ranked Sum Test

wilcox.test(hrate ~ smoker, data=df1) # 2-tailed

# should you need a 1-tailed test you can do it this way:

wilcox.test(hrate ~ smoker, data=df1, alternative = "less") # 1-tailed
wilcox.test(hrate ~ smoker, data=df1, alternative = "greater") # 1-tailed


## test normality of data:
# you use this to test if your data are normal or not
# if not normal, you should use the Wilcoxon Ranked Sum Test

shapiro.test(df1$hrate)  # if p-value lower than 0.05 then data not normal.






### COMPARING PAIRED DATA.  


head(df1)

#e.g. compare the values of immuncount to immuncount2


## Paired t-test
t.test(df1$immuncount, df1$immuncount2,  paired=TRUE) #2-tailed

# should you need a 1-tailed test you can do it this way:
t.test(df1$immuncount, df1$immuncount2,  paired=TRUE, alternative = "less") # 1-tailed
t.test(df1$immuncount, df1$immuncount2,  paired=TRUE, alternative = "greater") # 1-tailed



## Wilcoxon Signed Rank Test
wilcox.test(df1$immuncount, df1$immuncount2,  paired=TRUE) #2-tailed

# should you need a 1-tailed test you can do it this way:
wilcox.test(df1$immuncount, df1$immuncount2,  paired=TRUE, alternative = "less") # 1-tailed
wilcox.test(df1$immuncount, df1$immuncount2,  paired=TRUE, alternative = "greater") # 1-tailed



### To visualize data like this is a bit harder - 
# you need to bring the data from the two columns into one column...

df2 <- df1 %>% gather(key,value,10:11) %>% select(ids, key,value)
head(df2)
tail(df2)  # notice our data are now in long-format.

#visualize if cell counts change from time1 to time2 
ggplot(df2, aes(x=key, y=value)) + geom_boxplot()

# better plot showing individual lines
ggplot(df2, aes(x=key, y=value, group=ids)) + geom_point() + geom_line()