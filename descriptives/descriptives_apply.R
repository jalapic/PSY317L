## What if you want to get more than one piece of information at a time---

library(tidyverse)

cheese <- read_csv("data/cheese.csv")
head(cheese)

xt <- read_csv("data/crosstimes.csv")
xt

summary(cheese) #some summary stats

summary(xt)


# Some ways from other packages
# warning messages are ok.

library(psych)

describe(cheese)

describe(xt)






#### You can also do it by yourself....

head(cheese)

# we just want to get the numerical columns...
# a quick diversion on syntax..

cheese[]

cheese[,]

cheese[,2]

cheese[,6]

head(cheese)

cheese[,2:9]

# apply will apply a function over all columns (if 2 is there)

apply(cheese[,2:9], 2, mean)
apply(cheese[,2:9], 2, sd)
apply(cheese[,2:9], 2, IQR)


### If you have missing data:

xt
xt[,2:6] # only the numerical columns

apply(xt[,2:6], 2, mean)

apply(xt[,2:6], 2, mean, na.rm=T) #have to put that on the end.


