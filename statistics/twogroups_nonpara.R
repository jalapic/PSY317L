


## confidence intervals  paired data


## you could do a CI of the differences


t <- seq(-3,3,by=.01)
Density <- dt(t, df=6)
df <- data.frame(t,Density)
ggplot(df, aes(x=t,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black',lwd=1) +
  ggtitle("t-distribution with d.f.=6") +
  geom_vline(xintercept=-2.45, color='orange', lty=2)+
  geom_vline(xintercept=2.45, color='orange', lty=2)


qt(.975, df = 6)

difs <- c(3,1,8,1,-1,6,5)
difs
shapiro.test(difs)

t.test(difs, mu=0)


t <- qt(.975, df = 6)
s  <- sd(difs)
n <- length(difs)
err <- t * (s/sqrt(n))

mean(difs) + err
mean(difs) - err

d<-data.frame(
  electronic = c(40,31,44,42,30,26,29),
  classical = c(37,30,36,41,31,20,24)
)
d

t.test(d$electronic, d$classical, paired=T, var.equal = T)


#### Read data

chico <- read_csv("data/chico.csv")
head(chico)

chico

library(psych)
describe(chico)


head(chico)


### boxplot graph

# need to reorder the data:

chico.long <- chico %>% pivot_longer(2:3) 

chico.long


ggplot(chico.long, aes(x=name, y=value, fill=name))+
  geom_boxplot(outlier.shape = NA, alpha=.5) +
  geom_jitter(width=.1, size=1) +
  theme_classic() +
  scale_fill_manual(values=c("lightseagreen","darkseagreen"))



## 95% Confidence Intervals

test1.ci <-t.test(chico$grade_test1, mu=0)[4][[1]]
test2.ci <-t.test(chico$grade_test2, mu=0)[4][[1]]

chico.summary <- data.frame(
  means = c(mean(chico$grade_test1), mean(chico$grade_test2)),
  lower = c(test1.ci[1], test2.ci[1]),
  upper = c(test1.ci[2], test2.ci[2]),
  group = c("test1", "test2")
)

chico.summary

ggplot(chico.summary, aes(x=group, y = means,  ymax=upper, ymin=lower))+
  geom_point(pch=19, size=3) +
  geom_line(group=1, lwd=1) +
  geom_errorbar(width=0.2, size=1, color="blue") +
  theme_classic()



### Scatterplot

ggplot(chico, aes(x=grade_test1, y=grade_test2)) +
  geom_point() +
  theme_classic()+
  geom_abline(intercept =0 , slope = 1)


### Slope Graph
head(chico.long)

ggplot(chico.long, aes(x=name, y=value, group=id))+
  geom_point(alpha=.6, size=2)+
  geom_line(color="gray48", alpha=.8)+
  theme_classic()



#### Difference Scores

chico$improvement <- chico$grade_test2 - chico$grade_test1 
head(chico)
chico


# histogram of difference scores

ggplot(chico, aes(x=improvement)) +
  geom_histogram(color='black', fill='green', alpha=.4, boundary=0, binwidth = .5) +
  theme_classic()+
  geom_vline(xintercept = 0, lty=2, color='black')

mean(chico$improvement)
sd(chico$improvement)


## 95% CI

qt(.975, df=19)

t <- seq(-3,3,by=.01)
Density <- dt(t, df=19)
df <- data.frame(t,Density)
ggplot(df, aes(x=t,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black',lwd=1) +
  ggtitle("t-distribution with d.f.=19") +
  geom_vline(xintercept=-2.09, color='orange', lty=2)+
  geom_vline(xintercept=2.09, color='orange', lty=2)

t.test(chico$improvement, mu=0, var.equal = T)

t <- seq(-7,7,by=.01)
Density <- dt(t, df=19)
df <- data.frame(t,Density)
ggplot(df, aes(x=t,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black',lwd=1) +
  ggtitle("t-distribution with d.f.=19") +
  geom_vline(xintercept=6.4754, color='orange', lty=2)


mean(chico$improvement) / (sd(chico$improvement)/sqrt(20))

pt(6.475, df=19)


t.test(chico$improvement, mu=0)

t.test(chico$grade_test2, chico$grade_test1, paired = T)


# you can do 1-tailed tests too



# effect sizes

mean(chico$improvement) / sd(chico$improvement)


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