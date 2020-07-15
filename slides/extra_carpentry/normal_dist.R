library(tidyverse)

set.seed(1)

x1 <- rnorm(n = 1000000, mean = 100, sd = 5.0)
x2 <- rnorm(n = 1000000, mean = 17, sd = 3.5)
x3 <- rnorm(n = 1000000, mean = 17, sd = 7.0)
x4 <- rnorm(n = 1000000, mean = 100, sd = 15.0)



dfnorm <- data.frame(x1,x2,x3,x4)

p1=
ggplot(dfnorm, aes(x = x1))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  theme_classic() +
  xlab("values") +
  ggtitle("mean = 100, SD = 5")+
  geom_vline(xintercept=100, lty=2)

p2=
  ggplot(dfnorm, aes(x = x4))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  theme_classic() +
  xlab("values")+
  ggtitle("mean = 100, SD = 15")+
  geom_vline(xintercept=100, lty=2)

p3=
  ggplot(dfnorm, aes(x = x2))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 1) + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  theme_classic() +
  xlab("values")+
  ggtitle("mean = 17, SD = 3.5") +
  geom_vline(xintercept=17, lty=2)

p4=
  ggplot(dfnorm, aes(x = x3))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 1) + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  theme_classic() +
  xlab("values")+
  ggtitle("mean = 17, SD = 7")+
  geom_vline(xintercept=17, lty=2)

library(gridExtra)

grid.arrange(p1,p2,p3,p4,nrow=2)


dfnorm$z <- (dfnorm$x1 - mean(dfnorm$x1))/sd(dfnorm$x1)

ggplot(dfnorm, aes(x = z))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "dodgerblue", alpha=.4, binwidth = .1) + 
  geom_density(alpha = 0.1, fill = "navy") + 
  theme_classic() +
  xlab("z")+
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept=0, lty=2, lwd=1)



x <- rnorm(1000000, 0, 1)

ggplot(data.frame(x), aes(x=x))  + 
#  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.7, fill = "white", color="red", lwd=1) + 
  theme_minimal() +
  xlab("z") +
  ylab("") +
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept = -2.31, color="darkorange", lty=2, lwd=1) +
  theme_classic()




ggplot(data.frame(x), aes(x=x))  + 
  #  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  theme_minimal() +
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept = 0.875, color="red", lty=1, lwd=1) +
  theme_classic()




#### pineapples

# histogram
set.seed(10) # this just makes sure we all get the same results
x <- rnorm(n= 100000, mean = 1003.5, sd = 35)


p1=ggplot(data.frame(x), aes(x = x)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "cornflowerblue", alpha=0.9) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1)+
  theme_classic() +
  geom_vline(xintercept = 1003.5, lwd=1, lty=2) +
  geom_vline(xintercept=940, lty=1, color='red', lwd=1)+
  ggtitle("Population Distribution of Pineapples")
p1


df <- data.frame(x)
df$z <- (x-mean(x))/sd(x)

p2=ggplot(df, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept = -1.814286, color="red", lty=1, lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) 

p2

library(gridExtra)
grid.arrange(p1,p2,nrow=1)








p1=ggplot(data.frame(x), aes(x = x)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "cornflowerblue", alpha=0.9) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1)+
  theme_classic() +
  geom_vline(xintercept = 1003.5, lwd=1, lty=2) +
  geom_vline(xintercept=1050, lty=1, color='red', lwd=1)+
  ggtitle("Population Distribution of Pineapples")
p1


df <- data.frame(x)
df$z <- (x-mean(x))/sd(x)

p2=ggplot(df, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept = 1.33, color="red", lty=1, lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) 

p2

library(gridExtra)
grid.arrange(p1,p2,nrow=1)

(1050-1003.5)/35






p1=ggplot(data.frame(x), aes(x = x)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "cornflowerblue", alpha=0.9) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1)+
  theme_classic() +
  geom_vline(xintercept = 1003.5, lwd=1, lty=2) +
  geom_vline(xintercept=950, lty=1, color='red', lwd=1)+
  geom_vline(xintercept=1040, lty=1, color='red', lwd=1)+
  ggtitle("Population Distribution of Pineapples")
p1


df <- data.frame(x)
df$z <- (x-mean(x))/sd(x)

p2=ggplot(df, aes(x = z)) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1) + 
  xlab("z") +
  ylab("") +
  geom_vline(xintercept=0, color='black',lwd=1,lty=2)+
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept = -1.53, color="red", lty=1, lwd=1) +
  geom_vline(xintercept = 1.19, color="red", lty=1, lwd=1) +
  theme_classic() +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3)) 

p2

library(gridExtra)
grid.arrange(p1,p2,nrow=1)

(1050-1003.5)/35

