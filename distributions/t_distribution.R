## The t-distribution shape

### Normal

set.seed(10) # this just makes sure we all get the same results
x <- rnorm(n= 1000000, mean = 0, sd = 1)
df <- data.frame(x)

p <- ggplot(data.frame(x), aes(x = x)) + 
#  geom_histogram(aes(y = ..density..), color = "black", fill = "cornflowerblue", alpha=0.9) + 
  geom_density(alpha = 0.7, fill = "white", color="black", lwd=1)+
  theme_classic() +
  geom_vline(xintercept = 0, lwd=1, lty=2)

p

tdist <- seq(-5,5,by=.01)
Density <- dt(tdist, df=5)
dfx <- data.frame(tdist,Density)

p1 <- p +  geom_line(color='red',lwd=1, data=dfx, aes(x=tdist,y=Density)) +
  ggtitle("t-distribution df=5 vs \n Normal Distribution")
  


tdist2 <- seq(-5,5,by=.01)
Density2 <- dt(tdist2, df=10)
dfx2 <- data.frame(tdist2,Density2)

p2 <- p +  geom_line(color='red',lwd=1, data=dfx2, aes(x=tdist2,y=Density2)) +
  ggtitle("t-distribution df=10 vs \n Normal Distribution")


tdist3 <- seq(-5,5,by=.01)
Density3 <- dt(tdist, df=25)
dfx3 <- data.frame(tdist3,Density3)

p3 <- p +  geom_line(color='red',lwd=1, data=dfx3, aes(x=tdist3,y=Density3)) +
  ggtitle("t-distribution df=25 vs \n Normal Distribution")


tdist4 <- seq(-5,5,by=.01)
Density4 <- dt(tdist4, df=100)
dfx4 <- data.frame(tdist4,Density4)

p4 <- p +  geom_line(color='red',lwd=1, data=dfx4, aes(x=tdist4,y=Density4)) +
  ggtitle("t-distribution df=100 vs \n Normal Distribution")



library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2)
