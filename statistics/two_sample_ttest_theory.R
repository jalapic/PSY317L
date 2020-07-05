

### explaining t-test

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


5.48/2.59

t.test(anastasia, bernadette, var.equal = T)

pt(2.115, df = 31)

1 -  pt(2.115, df = 31)

t.test(anastasia, bernadette, var.equal = T, alternative = "greater")


###
#this should actually be a t-distribution:
df <- data.frame(val = rnorm(n = 500000, mean = 0, sd = 2.59))

ggplot(df, aes(x=val)) +
  geom_histogram(aes(y = ..density..), color='black', fill='purple', alpha=.3)+
  theme_classic()+
  geom_density(alpha = 0.7, fill = "white") + 
  geom_vline(xintercept = 5.48, lwd=1, color="red") + 
  xlab("Difference in Sample Means") +
  ylab("") +
  ggtitle("Sampling Distribution of Differences in Sample Means")




### make a t-distribution

t <- seq(-5,5,by=.01)
Density <- dt(t, df=31)
df <- data.frame(t,Density)
ggplot(df, aes(x=t,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=2.12, color='red',lwd=1 )


#### Example 2

jays <- read_csv("data/BlueJays.csv")
head(jays)
nrow(jays)

headf <- jays %>% filter(KnownSex=='F') %>% .$Head
headf

headm <- jays %>% filter(KnownSex=='M') %>% .$Head
headm


ggplot(jays, aes(x=KnownSex, y=Head, fill=KnownSex)) +
  geom_boxplot(alpha=.3, outlier.shape = NA) +
  geom_jitter(width=.1,size=2, alpha=.5) +
  scale_fill_manual(values=c("orange","purple"))+
  theme_classic()+
  xlab("Sex")+
  ylab("Head Size mm")

mean(headf)
sd(headf)
length(headf)

mean(headm)
sd(headm)
length(headm)


spsd<-sqrt((sum((headf - mean(headf))^2)+sum((headm - mean(headm))^2))/(length(headf)+length(headm)-2))
ANOVAreplication::pooled.sd(jays %>% select(Head,KnownSex))

sedm <- spsd * sqrt((1/63) + (1/60) )

dm <- mean(headm) - mean(headf)
dm

dm/sedm

t.test(headm, headf, var.equal = T)
2.05/0.215

dm/spsd

(1 - (pt(9.520152, df=121))) * 2

qt(.975, df = 121)

qt(.95, df = 121)
