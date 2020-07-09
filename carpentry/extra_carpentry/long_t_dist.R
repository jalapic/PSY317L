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


