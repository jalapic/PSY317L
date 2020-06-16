
# what about standard error?
length(df$day1) # we need to know the N to calculate the SEM
sd(df$day1) / sqrt(length(df$day1)) #SEM by hand

sem <- function(x){sd(x) / sqrt(length(x))} # a function to get SEM

sem(df$day1) #using the function

# please note  - if you have missing data use this one:
sem <- function(x){sd(x,na.rm=T) / sqrt(length(na.omit(x)))}

