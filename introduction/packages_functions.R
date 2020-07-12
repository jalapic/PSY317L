### Packages and functions

# packages or libaries are things we can download into R Studio to provide functions

# e.g. the 'tidyverse' is a library that enables us to do lots of graphing and data stuff:

library(tidyverse)


# another is the 'psych' package that has stats stuff

library(psych)


# if you don't load your library, the function in it you need won't be there.




### Functions

# Many functions come with the downloaded version of R (called base-R)

# Other functions are available via packages.




# Functions are names that specify something to be done, followed by brackets

x <- 5:10

x

sum(x)

sqrt(x)



round(17.10771947)

# arguments
round(17.10771047, 3)


# name your arguments
round(x = 17.10771047, digits = 3)


# to avoid this
round(3, 17.10771047)

#ok
round(digits = 3, x = 17.10771047)



# arguments may require numbers, words, punctuation, logic

obj1 <- "hello"
obj2 <- "james"
obj3 <- "curley"
paste(obj1, obj2, obj3, sep = "_")



# another example

sample(x, 1)

sample(x = x, size = 10, replace = TRUE)




## R works from in to out...

# and this can get confusing....

x <- c(1, 5, 3, 7, 10, 11, 13, 5, 6, 2, 5, 6, 3, 7)

x


unique(x)

sqrt(unique(x))

sum(sqrt(unique(x)))

round(sum(sqrt(unique(x))), digits = 2)



# so we can also do something called "chaining".... which makes it easier to read:

library(tidyverse)

x %>%
  unique %>%
  sqrt %>%
  sum %>%
  round(2)






