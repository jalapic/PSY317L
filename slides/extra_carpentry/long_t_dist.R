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
