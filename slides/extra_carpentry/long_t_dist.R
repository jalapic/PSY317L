library(tidyverse)

tdist <- seq(-21,21,by=.01)
Density <- dt(tdist, df=98)
dfx <- data.frame(tdist,Density)


ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=-20.854, color='blue',lwd=1,lty=2 ) +
  ggtitle("t-distribution with d.f.=98") +
  xlab("t")





library(tidyverse)
tdist <- seq(-3,3,by=.01)
Density <- dt(tdist, df=24)
dfx <- data.frame(tdist,Density)

qt(.90, df = 24)

ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_hline(yintercept=0, lty=1, color='black')+
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=-1.3178, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=1.3178, color='red',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=24") +
  xlab("t")





dist <- seq(-3,3,by=.01)
Density <- dt(tdist, df=9)
dfx <- data.frame(tdist,Density)

qt(.975, df = 9)

p1 = ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_hline(yintercept=0, lty=1, color='black')+
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=-2.26, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=2.26, color='red',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=9") +
  xlab("t") +
  xlim(-3,3)


dist <- seq(-3,3,by=.01)
Density <- dt(tdist, df=19)
dfx <- data.frame(tdist,Density)

qt(.975, df = 19)

p2 = ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_hline(yintercept=0, lty=1, color='black')+
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=-2.09, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=2.09, color='red',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=19") +
  xlab("t")



dist <- seq(-3,3,by=.01)
Density <- dt(tdist, df=29)
dfx <- data.frame(tdist,Density)

qt(.975, df = 29)

p3 = ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_hline(yintercept=0, lty=1, color='black')+
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=-2.05, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=2.05, color='red',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=29") +
  xlab("t")

library(gridExtra)
grid.arrange(p1,p2,p3,nrow=1)


########
N<-1000000
df<-11
sdev<-4.143268
v <- sdev^2
m <- 50

vals <- rt(N, df=df)*sqrt(v * (df-2)/df) + m

library(tidyverse)
r1 = ggplot(data.frame(vals), aes(x = vals))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 1) + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  xlim(30,70)+
  theme_classic() +
  xlab("Sample Means") +
  ggtitle("Sampling Distribution \n t-distribution df=11")+
  geom_vline(xintercept=50, lty=2, lwd=1) +
  geom_vline(color="red", xintercept = 54, lwd=1)

r1


tdist <- seq(-5,5,by=.01)
Density <- dt(tdist, df=11)
dfx <- data.frame(tdist,Density)

r2 =ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=0.965, color='red',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=11") +
  xlab("t")+
  xlim(-5,5)

library(gridExtra)
grid.arrange(r1,r2,nrow=1)

(30-50)/4.14




N<-1000000
df<-19
sdev<-2.128874
v <- sdev^2
m <- 65

vals <- rt(N, df=df)*sqrt(v * (df-2)/df) + m

library(tidyverse)
r1 = ggplot(data.frame(vals), aes(x = vals))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 1) + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  xlim(55,75)+
  theme_classic() +
  xlab("Sample Means") +
  ggtitle("Sampling Distribution \n t-distribution df=19")+
  geom_vline(xintercept=65, lty=2, lwd=1) +
  geom_vline(color="red", xintercept = 72.3, lwd=1)

r1


tdist <- seq(-5,5,by=.01)
Density <- dt(tdist, df=19)
dfx <- data.frame(tdist,Density)

r2 =ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=3.43, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=-3.43, color='red',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=19") +
  xlab("t")+
  xlim(-5,5)

library(gridExtra)
grid.arrange(r1,r2,nrow=1)




tdist <- seq(-5,5,by=.01)
Density <- dt(tdist, df=9)
dfx <- data.frame(tdist,Density)

ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=-1.83, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=-0.99, color='purple',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=9") +
  xlab("t")+
  xlim(-5,5)





tdist <- seq(-5,5,by=.01)
Density <- dt(tdist, df=21)
dfx <- data.frame(tdist,Density)

qt(df=21, .025)
qt(df=21, .975)

ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=-2.08, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=2.08, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=1.75, color='purple',lwd=1,lty=1 ) +
  ggtitle("t-distribution with d.f.=21") +
  xlab("t")+
  xlim(-5,5)





tdist <- seq(-5,5,by=.01)
Density <- dt(tdist, df=22)
dfx <- data.frame(tdist,Density)

ggplot(dfx, aes(x=tdist,y=Density))+
  theme_classic()+
  geom_line(intercept=0, color='firebrick',lwd=1) +
  geom_vline(xintercept=0, lty=2, color='black', lwd=1)+
  geom_vline(xintercept=2.073873, color='red',lwd=1,lty=1 ) +
  geom_vline(xintercept=-2.073873, color='red',lwd=1,lty=1 ) +
  ggtitle("Generating a 95% CI") +
  xlab("t")

qt(.975, df=22)





m <- -0.76 # mean
v <- sedm^2 # variance, sedm squared
df <- 22
vals <- rt(n=500000, df=df)*sqrt(v * (df-2)/df) + m
df1 <- data.frame(val = vals)


ggplot(df1, aes(x=val)) +
  geom_histogram(aes(y = ..density..), color='black', fill='#1930FF', alpha=.3)+
  theme_classic()+
  geom_density(alpha = 0.7, fill = "white") + 
  geom_vline(xintercept = -0.13, lwd=1, color="red") + 
  geom_vline(xintercept = -1.39, lwd=1, color="red") + 
  geom_vline(xintercept = -0.76, lwd=1,lty=2, color='black')+
  xlab("Difference in Sample Means") +
  ylab("") +
  ggtitle("Sampling Distribution of Differences in Sample Means",
          subtitle = "Red Lines Represent 95% CIs")