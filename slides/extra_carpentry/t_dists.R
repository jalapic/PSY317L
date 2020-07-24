library(tidyverse)

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



tdist <- seq(-5,5,by=.01)
Density <- dt(tdist, df=31)
dfx <- data.frame(tdist,Density)

p1=ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=2.115, color='red',lwd=1,lty=2 ) +
  ggtitle("One-tailed test") +
  xlab("t")


p2=ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=2.115, color='red',lwd=1,lty=2 ) +
  geom_vline(xintercept=-2.115, color='red',lwd=1,lty=2 ) +
  ggtitle("Two-tailed test") +
  xlab("t")


library(gridExtra)
grid.arrange(p1,p2,nrow=1)
