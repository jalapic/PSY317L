#### z-test problem examples

library(tidyverse)
source("distributions/plothist.R")

### A manufacturing company produces metal parts 

# The parts are normally distributed with a length 10mm and SD 0.2mm

# The manager suspects that the manufacturing machine is faulty
# and that the parts are longer than they should be.

# They select a batch of 25 parts.
# the sample mean is 10.1mm 

# is this an unepectedly large mean ?


### Population:  Mean = 10,  SD = 0.2
# Sample:  n = 25,  mean = 10.1

x <- 10.1

x

# 1. Our one sample mean is one possible sample mean from the sampling distribution of sample means.

# the sampling distribution has:
# mean = 10
# sd =  0.2 / sqrt(25)
0.2 / sqrt(25)  #SD of sampling distribution (standard error of the mean)

sem <- 0.2 / sqrt(25)

sem



# 2. How many "standard deviations" is our sample mean from the mean of the sampling distribution?

# Calculate the z-score:

(x - 10) / sem

z <- (x - 10) / sem

z


# 3. What proportion of the sampling distribution is higher than +2.5 SDs than the mean?
# same as saying what proportion is 2.5SD higher than 0 in the standard normal curve.


pnorm(z)  # this is the area to the left of the line

1 - pnorm(z)  # p = 0.006209665

# as p <.05, this is good evidence that the sample mean is unexpectedly large.






#####################  Visualization Code Below Here -----

## Don't worry about this code- I just want to show you what the graphs look like

p1 <- plothist(mean = 10, sd = 0.1) +
      ggtitle("Population") +
  xlab("Length of part (mm) ")

p2 <- plothist(mean = 10, sd = sem) +
  geom_vline(xintercept = 10.1, color = "darkorange", lwd =1) +
  ggtitle("Sampling Distribution of \n Sample Means for n = 25") +
  xlab("Mean length (mm) of each sample")

p3 <- plothist(mean = 0, sd = 1) +
  geom_vline(xintercept = 1.645, color = "red", lwd =1, lty=2) +
  geom_vline(xintercept = z, color = "darkorange", lwd =1) +
  ggtitle("Standard Normal Curve") +
  xlab("z")

library(gridExtra)
grid.arrange(p1,p2,p3,nrow=1)

