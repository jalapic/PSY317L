
### Some tips about coding.




## Naming of objects


# pick something that makes sense
mycolors <- c("mistyrose", "dodgerblue", "pink")

mycolors1 <- c("mistyrose", "dodgerblue", "pink")

my1colors <- c("mistyrose", "dodgerblue", "pink")


#  - you can use characters or numbers in object names, or both

# - you cannot start with a number though

1mycolors <- c("mistyrose", "dodgerblue", "pink")



# - do not use spaces
my colors <- c("mistyrose", "dodgerblue", "pink")


# - try to use lower case ideally
MyColors <- c("mistyrose", "dodgerblue", "pink")

MYCOLORS <- c("mistyrose", "dodgerblue", "pink")  # no !


# - do not use any puncutation in names for objects, dataframes or column names except for the period `.` or underscore `_`. Everything else is forbidden.

# - if you do use an underscore `_` it cannot go at the beginning, e.g.

_mynumber <- 17  #not allowed



# - you can use a period `.` at the beginning of an object name, but please don't.

.mynumber <- 99


# - periods `.` and underscores `_` are best used in the middle of object names to help read the name, e.g.


prime_numbers <- c(1,3,5,7,11,13)
prime_numbers


# - but even better, use short object names!


primes <- c(1,3,5,7,11,13)
primes



# - you should avoid using certain words, as they have other meanings in R
# see book for list.

# e.g. while, if, break, etc.





## Some things that are useful to know.

# R doesn't, on the whole, care that much about whitespace 
# - this is space that is between code. It just ignores it.  
# For instance, all of the following give the same result

                                                                                                                                                                             
#
a <- c(1,2,3)
b <- c(4,5,6)

a

b


a + b


#
a<- c(   1,            2,     3)
b <- c(4,5,6           )
a +              b


#
a <- c(1    ,2,   3)
b <-    c(    4,5  ,6)
a +    b


# one of the few exceptions:  assigning <-

eg < - 5:10  # this will not work.
eg





### Other common errors:


# closing brackets:


a <- c(4, 4, 2, 7
       
       
       
# don't forget commas

b <- c(3, 6, 9 10)


x <- c(5,  8, )


# check quote marks

days <- c("Monday", "Tuesday", Wednesday)
days

days <- c("Monday', "Tuesday", 'Wednesday')
days









       
       




