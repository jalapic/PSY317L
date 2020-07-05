### New Video on Estimating .... What if we


anastasia <- c(65, 74, 73, 83, 76, 65, 86, 70, 80, 55, 78, 78, 90, 77, 68)

bernadette <- c(72, 66, 71, 66, 76, 69, 79, 73, 62, 69, 68, 60, 73, 68, 67, 74, 56, 74)


mean(anastasia)
sd(anastasia)


mean(bernadette)
sd(bernadette)


mean(anastasia)  - mean(bernadette)



anastasia - mean(anastasia)
(anastasia - mean(anastasia))^2
an2_sum <- sum((anastasia - mean(anastasia))^2)
an2_sum

qt(.975, df = 31)
bernadette - mean(bernadette)
(bernadette - mean(bernadette))^2
be2_sum <- sum((bernadette - mean(bernadette))^2)
be2_sum

ssq <- an2_sum + be2_sum
ssq

length(anastasia) + length(bernadette) - 2 

ssq/31

sedm <- sqrt(ssq/31)

sedm 

sedm * sqrt((1/15) + (1/18))


5.48 + (2.04 * 2.59)
5.48 - (2.04 * 2.59)


t.test(anastasia, bernadette, var.equal = T)




##### only summary info:


x1 <- c(110, 95, 99, 101, 103, 111, 121, 93, 98, 97)
x2 <- c(121, 111, 116, 101, 105, 99, 103, 110, 98, 120, 115)

mean(x1)
mean(x2)

sd(x1)
sd(x2)

n1=length(x1)
n2=length(x2)

mean(x1) - mean(x2)


sqrt(((9*(8.7^2))+(10*(8.3^2)))/(19))



x1_2_sum <- sum((x1 - mean(x1))^2)
x2_2_sum <- sum((x2 - mean(x2))^2)
ssq <- x1_2_sum + x2_2_sum
ssq

sqrt(ssq/19)

(sqrt(ssq/19))*(sqrt((1/n1)+(1/n2)))

qt(.975, df=19)

-6.2 + (2.09 * 3.72)
-6.2 - (2.09 * 3.72)

t.test(x1,x2,var.equal = T)
