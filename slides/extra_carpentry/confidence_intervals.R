### Confidence intervals.

library(tidyverse)


x <- rnorm(100000, mean = 7.8, sd = 0.3)
library(tidyverse)
p1 <- ggplot(data.frame(vals=x),aes(vals))+
  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 0.05) + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  theme_classic() +
  xlab("Wingspan cm")+
  geom_vline(xintercept = 7.8, color='black',lwd=1)
p1


set.seed(1)
samp1 <- sample(x, size = 15, replace = T)
samp1
mean(samp1)



samp2 <- sample(x, size = 15, replace = T)
samp2
mean(samp2)



0.3 / sqrt(15)



#get sample means for sampling distribution
results<-vector('list',100000)
for(i in 1:100000){
  results[[i]]  <- mean(sample(x, 15, replace = T))  
}

res <- unlist(results)

p2 <- ggplot(data.frame(res), aes(x = res)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "#4adbe0", alpha=.4, binwidth = 0.01) + 
  geom_density(alpha = 0.7, fill = "ghostwhite") + 
  theme_classic() +
  xlab("Sample Mean") +
  ylab("Frequency") +
  ggtitle("Sampling Distribution of Sample Means for n=15") +
  geom_vline(xintercept = mean(res), lwd=1)

p2




z <- rnorm(100000, 0, 1)

ggplot(data.frame(z), aes(x = z))  + 
#  geom_histogram(aes(y = ..density..), color = "black", fill = "purple", alpha=.4, binwidth = 2) + 
  geom_density(alpha = 0.7, fill = "ghostwhite", color="red") + 
  theme_classic() +
  xlab("z") +
  ggtitle("Standard Normal Distribution")+
  geom_vline(xintercept=0, lty=2, lwd=1) +
  geom_vline(xintercept = 1.96, color="red", lty=1)+
  geom_vline(xintercept = -1.96, color="red", lty=1)

qnorm(c(0.025, 0.975))


7.8 +  (1.96 * 0.077)

7.8 -  (1.96 * 0.077)


p2 + 
  geom_vline(xintercept=7.65, color='red', lty=1,lwd=1) + 
  geom_vline(xintercept=7.95, color='red', lty=1, lwd=1)




### Confidence Intervals Summary:

dfa <- data.frame(sample = "samp1", mean = 7.72, lower = 7.56773, upper = 7.871372)

ggplot(dfa) + 
  geom_errorbar(aes(x=sample, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=sample, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red")


dfb <- data.frame(sample = c("samp1", "samp2"),
                  mean = c(7.72, 7.95), 
                  lower = c(7.56773,7.798179),
                  upper = c(7.871372, 8.101821)
)

ggplot(dfb) + 
  geom_errorbar(aes(x=sample, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=sample, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red")



# for 25 ...
results<-vector('list',25)
for(i in 1:25){
results[[i]]<-mean(sample(x, 15, T))
}
xmeans <- unlist(results)
lowers <-  xmeans - (1.96 * 0.07745967)
uppers <-  xmeans + (1.96 * 0.07745967)

dfc<-
  data.frame(sample = 1:25,
           mean = xmeans,
           lower = lowers,
           upper = uppers)

ggplot(dfc) + 
  geom_errorbar(aes(x=sample, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=sample, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red")




#### Other percentages from standard normal dist.

qnorm(.975)


q1 <- ggplot(data.frame(z), aes(x = z))  + 
  geom_density(alpha = 0.7, fill = "ghostwhite", color="red") + 
  theme_classic() +
  xlab("z") +
  ggtitle("80% CI")+
  geom_vline(xintercept=0, lty=2, lwd=1) +
  geom_vline(xintercept = qnorm(.9), color="red", lty=1)+
  geom_vline(xintercept = -qnorm(.9), color="red", lty=1)


q2 <- ggplot(data.frame(z), aes(x = z))  + 
  geom_density(alpha = 0.7, fill = "ghostwhite", color="red") + 
  theme_classic() +
  xlab("z") +
  ggtitle("90% CI")+
  geom_vline(xintercept=0, lty=2, lwd=1) +
  geom_vline(xintercept = qnorm(.95), color="red", lty=1)+
  geom_vline(xintercept = -qnorm(.95), color="red", lty=1)


q3 <- ggplot(data.frame(z), aes(x = z))  + 
  geom_density(alpha = 0.7, fill = "ghostwhite", color="red") + 
  theme_classic() +
  xlab("z") +
  ggtitle("95% CI")+
  geom_vline(xintercept=0, lty=2, lwd=1) +
  geom_vline(xintercept = qnorm(.975), color="red", lty=1)+
  geom_vline(xintercept = -qnorm(.975), color="red", lty=1)


q4 <- ggplot(data.frame(z), aes(x = z))  + 
  geom_density(alpha = 0.7, fill = "ghostwhite", color="red") + 
  theme_classic() +
  xlab("z") +
  ggtitle("99% CI")+
  geom_vline(xintercept=0, lty=2, lwd=1) +
  geom_vline(xintercept = qnorm(.995), color="red", lty=1)+
  geom_vline(xintercept = -qnorm(.995), color="red", lty=1)


library(gridExtra)
grid.arrange(q1,q2,q3,q4,nrow=2)



qnorm(.9) * (0.3 / sqrt(15))
qnorm(.95) *(0.3 / sqrt(15))
qnorm(.975) *(0.3 / sqrt(15))
qnorm(.995) *(0.3 / sqrt(15))


c(7.72 + qnorm(.9) * (0.3 / sqrt(15)),
  7.72 + qnorm(.95) *(0.3 / sqrt(15)),
  7.72 + qnorm(.975) *(0.3 / sqrt(15)),
  7.72 + qnorm(.995) *(0.3 / sqrt(15))
)


c(7.72 - qnorm(.9) * (0.3 / sqrt(15)),
  7.72 - qnorm(.95) *(0.3 / sqrt(15)),
  7.72 - qnorm(.975) *(0.3 / sqrt(15)),
  7.72 - qnorm(.995) *(0.3 / sqrt(15))
)



dfd <- data.frame(CI = c("80%", "90%", "95%", "99%"),
                  mean = 7.72, 
                  lower = c(7.72 - qnorm(.9) * (0.3 / sqrt(15)),
                            7.72 - qnorm(.95) *(0.3 / sqrt(15)),
                            7.72 - qnorm(.975) *(0.3 / sqrt(15)),
                            7.72 - qnorm(.995) *(0.3 / sqrt(15))
                  )
                  ,
                  upper = c(7.72 + qnorm(.9) * (0.3 / sqrt(15)),
                            7.72 + qnorm(.95) *(0.3 / sqrt(15)),
                            7.72 + qnorm(.975) *(0.3 / sqrt(15)),
                            7.72 + qnorm(.995) *(0.3 / sqrt(15))
                  )
)

ggplot(dfd) + 
  geom_errorbar(aes(x=CI, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=CI, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red")







dfe <- data.frame(CI = c("80%", "90%", "95%", "99%"),
                  mean = 7.95, 
                  lower = c(7.95 - qnorm(.9) * (0.3 / sqrt(15)),
                            7.95 - qnorm(.95) *(0.3 / sqrt(15)),
                            7.95 - qnorm(.975) *(0.3 / sqrt(15)),
                            7.95 - qnorm(.995) *(0.3 / sqrt(15))
                  )
                  ,
                  upper = c(7.95 + qnorm(.9) * (0.3 / sqrt(15)),
                            7.95 + qnorm(.95) *(0.3 / sqrt(15)),
                            7.95 + qnorm(.975) *(0.3 / sqrt(15)),
                            7.95 + qnorm(.995) *(0.3 / sqrt(15))
                  )
)

ggplot(dfe) + 
  geom_errorbar(aes(x=CI, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=CI, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red")




dff <- data.frame(n = c("5","10","15","25","50"),
                  mean = 7.72, 
                  lower = c(7.72 - qnorm(.975) * (0.3 / sqrt(5)),
                            7.72 - qnorm(.975) *(0.3 / sqrt(10)),
                            7.72 - qnorm(.975) *(0.3 / sqrt(15)),
                            7.72 - qnorm(.975) *(0.3 / sqrt(25)),
                            7.72 - qnorm(.975) *(0.3 / sqrt(50))
                  )
                  ,
                  upper = c(7.72 + qnorm(.975) * (0.3 / sqrt(5)),
                            7.72 + qnorm(.975) *(0.3 / sqrt(10)),
                            7.72 + qnorm(.975) *(0.3 / sqrt(15)),
                            7.72 + qnorm(.975) *(0.3 / sqrt(25)),
                            7.72 + qnorm(.975) *(0.3 / sqrt(50))
                  )
)

dff$n<-factor(dff$n, levels=c("50", "25", "15", "10", "5"))

ggplot(dff) + 
  geom_errorbar(aes(x=n, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=n, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  xlab("sample size 'n'")+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red")




#####

#get sample means for sampling distribution
results<-vector('list',20)
for(i in 1:20){
  results[[i]]  <- mean(sample(x, 10, replace = T))  
}

res10 <- unlist(results)

results<-vector('list',20)
for(i in 1:20){
  results[[i]]  <- mean(sample(x, 50, replace = T))  
}

res50 <- unlist(results)


lower10 <- res10 - (1.96 * (0.3/sqrt(10)))
upper10 <- res10 + (1.96 * (0.3/sqrt(10)))

lower50 <- res50 - (1.96 * (0.3/sqrt(50)))
upper50 <- res50 + (1.96 * (0.3/sqrt(50)))

dfg <- data.frame(n = rep(c("10","50"),each=20),
                  sample = c(1:20),
                  mean = c(res10,res50), 
                  lower = c(lower10,lower50),
                  upper = c(upper10,upper50)
)

head(dfg)
tail(dfg)
str(dfg)

ggplot(dfg) + 
  geom_errorbar(aes(x=sample, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=sample, y=mean), size=4, shape=21, fill="white") + 
  facet_wrap(~n)+
  coord_flip() +
  theme_classic()+
  xlab("")+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red") 



### z vs t

dfh <- data.frame(sample = c("samp1-z", "samp1-t","samp2-z", "samp2-t"),
                  mean = c(7.72, 7.72, 7.95, 7.95), 
                  lower = c(7.56773,7.56, 7.798179, 7.796),
                  upper = c(7.871372, 7.88, 8.101821, 8.104)
)




ggplot(dfh) + 
  geom_errorbar(aes(x=sample, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=sample, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  xlab("")+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red") 


###


#get sample means for sampling distribution
results<-vector('list',20)
for(i in 1:20){
  results[[i]]  <- sample(x, 15, replace = T)  
}

res15 <- results
res15


#means
res15.means <- unlist(lapply(res15, mean))
res15.means

res15.sds <- unlist(lapply(res15, sd))
res15.sds


lower15z <- res15.means - (1.96 * (0.3/sqrt(15)))
upper15z <- res15.means + (1.96 * (0.3/sqrt(15)))


lower15t <- res15.means - (2.144787 * (res15.sds/sqrt(15)))
upper15t <- res15.means + (2.144787 * (res15.sds/sqrt(15)))




dfi <- data.frame(group = rep(c("z","t"),each=20),
                  sample = c(1:20),
                  mean = c(res15.means,res15.means), 
                  lower = c(lower15z,lower15t),
                  upper = c(upper15z,upper15t)
)

head(dfi)

dfi$group<-factor(dfi$group, levels=c("z","t"))

ggplot(dfi) + 
  geom_errorbar(aes(x=sample, ymin=upper, ymax=lower), width=0.2, size=1, color="blue") + 
  geom_point(aes(x=sample, y=mean), size=4, shape=21, fill="white") + 
  coord_flip() +
  theme_classic()+
  xlab("")+
  facet_wrap(~group)+
  geom_hline(yintercept = 7.8, lwd=1, lty=2, color="red") 

