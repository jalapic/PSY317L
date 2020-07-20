library(tidyverse)

set.seed(1)

x1 <- rnorm(n = 1000000, mean = 100, sd = 15.0)

dfnorm <- data.frame(x1)
            
p1=
ggplot(dfnorm, aes(x = x1))  + 
geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 2) + 
geom_density(alpha = 0.7, fill = "mistyrose") + 
theme_classic() +
ggtitle("Population mean = 100, SD = 15")+
geom_vline(xintercept=100, lty=2) +
xlab("IQ score")
            
p1

results<-vector('list',100000)
for(i in 1:100000){results[[i]]<-mean(sample(x1,25,T))}
vals<-unlist(results)


p2 = ggplot(data.frame(vals), aes(x = vals))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "cyan", alpha=.8, binwidth = .5) + 
  geom_density(alpha = 0.5, fill = "dodgerblue") + 
  theme_classic() +
  ggtitle("Sampling Distribution of Sample Means for n=25")+
  geom_vline(xintercept=100, lty=2) +
  xlab("Sample Mean")



p2

p2b <- p2 + geom_vline(xintercept = 102.5, color="red",lwd=1)
p2b

dfnorm$z <- (x1-mean(x1))/sd(x1)

p3=ggplot(dfnorm, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept = 0.833, color="red", lty=1, lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) 

p3

library(gridExtra)
grid.arrange(p2b,p3,nrow=1)



ggplot(dfnorm, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept = 1.9, color="red", lty=1, lwd=1) +
  geom_vline(xintercept = -1.9, color="red", lty=1, lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3))




q1=ggplot(dfnorm, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution \n One-tailed tests")+
  geom_vline(xintercept = 1.645, color="red", lty=1, lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) 


q2=ggplot(dfnorm, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution \n One-tailed tests")+
  geom_vline(xintercept = -1.645, color="red", lty=1, lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) 


grid.arrange(q1,q2,nrow=1)

q1 + geom_vline(xintercept = 1.77, color="purple", lwd=1)


ggplot(dfnorm, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept = 1.96, color="red", lwd=1)+
  geom_vline(xintercept = -1.96, color="red", lwd=1)+
  ggtitle("Standard Normal Distribution \n Two-tailed tests")+
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) 



ggplot(dfnorm, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept = 1.96, color="red", lwd=1)+
  geom_vline(xintercept = -1.96, color="red", lwd=1)+
  ggtitle("Standard Normal Distribution \n Two-tailed tests")+
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  geom_vline(xintercept = -1.69, color="purple", lwd=1)

