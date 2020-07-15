

#### Curvilinear and cubic data.


x <- runif(100)
y <- runif(100) + (-3.5*x) + (3.4 * x^2) 
y = y +1
x = x * 10

p1 = ggplot(data.frame(x,y),
       aes(x,y)) +
  geom_point() + stat_smooth(method="lm", se=TRUE, fill=NA,
                             formula=y ~ poly(x, 3, raw=TRUE),colour="red")

p1




x <- runif(100)
y <- runif(100) + (-5.5*x) + (5.4 * x^2) + (-7.4 * x^3) 
y = y +10
x = x * 10

p2 = ggplot(data.frame(x,y),
            aes(x,y)) +
  geom_point() + stat_smooth(method="lm", se=TRUE, fill=NA,
                             formula=y ~ poly(x, 4, raw=TRUE),colour="red")

p2

grid.arrange(p1,p2,nrow=1)
