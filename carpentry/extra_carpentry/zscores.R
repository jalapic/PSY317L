### skewness
library(tidyverse)
library(fGarch)
x <- rsnorm(1000000, mean = 100, sd = 20, xi = 1.7)
skewness(x)
p1=ggplot(data.frame(x), aes(x)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightseagreen", 
                 alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "white") + 
  theme_classic() +
  xlab("values") +
  ggtitle("Skewness = 0.68") 


x1 <- rsnorm(1000000, mean = 100, sd = 20, xi = -1.7)
skewness(x1)
p2=ggplot(data.frame(x1), aes(x1)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightseagreen", 
                 alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "white") + 
  theme_classic() +
  xlab("values") +
  ggtitle("Skewness = -0.68")




x3 <- rnorm(1000000, mean = 100, sd = 20)
skewness(x3)
p3=ggplot(data.frame(x3), aes(x3)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightseagreen", 
                 alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "white") + 
  theme_classic() +
  xlab("values") +
  ggtitle("Skewness = 0")


library(gridExtra)
grid.arrange(p1,p3,p2,nrow=1)


p4=p1 + geom_vline(xintercept = median(x), color = "blue", lwd=1)+ 
  geom_vline(xintercept = mean(x), color = "red", lwd=1)+ 
  geom_vline(xintercept = estimate_mode(x), color = "darkorange", lwd=1)

p5=p2 + geom_vline(xintercept = median(x1), color = "blue", lwd=1)+ 
  geom_vline(xintercept = mean(x1), color = "red", lwd=1)+ 
  geom_vline(xintercept = estimate_mode(x1), color = "darkorange", lwd=1)

p6=p3 + geom_vline(xintercept = median(x3), color = "blue", lwd=1)+ 
  geom_vline(xintercept = mean(x3), color = "red", lwd=1)+ 
  geom_vline(xintercept = estimate_mode(x3), color = "darkorange", lwd=1)


grid.arrange(p4,p6,p5,nrow=1)







x5 <- rnorm(1000000, mean = 60, sd = 6)

p10=ggplot(data.frame(x5), aes(x5)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "orange", 
                 alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "white") + 
  theme_classic() +
  xlab("values") +
  geom_vline(xintercept=60, lty=2,lwd=1)+
  ggtitle("Airedale Terriers") +
  xlab("Weight - lbs") +
  geom_vline(color="red", lwd=1, xintercept=65)



x6 <- rnorm(1000000, mean = 20, sd = 0.4)
p11=ggplot(data.frame(x6), aes(x6)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", 
                 alpha=.4, binwidth = 0.1) + 
  geom_density(alpha = 0.7, fill = "white") + 
  theme_classic() +
  xlab("values") +
  geom_vline(xintercept=20, lty=2,lwd=1)+
  ggtitle("Scottish Terriers")+
  xlab("Weight - lbs")+
  geom_vline(color="red", lwd=1, xintercept=20.5)


x7 <- rnorm(1000000, mean = 0, sd = 1)

p12=ggplot(data.frame(x7), aes(x7)) +
 # geom_histogram(aes(y = ..density..), color = "black", fill = "orange", 
  #               alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "white") + 
  theme_classic() +
  xlab("values") +
  geom_vline(xintercept=0, lty=2,lwd=1)+
  ggtitle("z = 0.83") +
  xlab("z") +
  geom_vline(color="red", lwd=1, xintercept=0.83)


p13=ggplot(data.frame(x7), aes(x7)) +
 # geom_histogram(aes(y = ..density..), color = "black", fill = "purple", 
   #              alpha=.4, binwidth = 0.1) + 
  geom_density(alpha = 0.7, fill = "white") + 
  theme_classic() +
  xlab("values") +
  geom_vline(xintercept=0, lty=2,lwd=1)+
  ggtitle("z = 1.25")+
  xlab("z")+
  geom_vline(color="red", lwd=1, xintercept=1.25)



grid.arrange(p10,p11,p12,p13,nrow=2)
