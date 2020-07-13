


## Working with Vectors

v1 <- 5:10  

v1



v2 <- c(4,6,8,10,8,6,1)

v2




# you can apply math over each of the vector elements at once:


v1^2

v2 * 3




# some things work on all of the elements of a vector together

sum(v1)  # adds up all of v1

sum(v2)

length(v1) # how many elements in v1 ?

length(v2)







## Characters vectors


the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

the_week


# notice that they have quote marks around them

aa <- 5
bb <- c(9, 11)

c(aa, bb)

c("aa","bb")

new_v <- c(aa, bb)
new_v



# you can mix numbers and characters
cc <- 10
dd <- c("james", "tyler")
ee <- "hedgehog"

aa
bb
cc
dd
ee

eg <- c(aa, bb, cc, dd, ee)

eg


## some things work with both numeric and character vectors

v1

the_week

length(v1)

length(the_week)


## some things only work on numbers or characters

sqrt(v1)

sqrt(the_week)




nchar(the_week)

nchar(v1)



## indexing works the same way on both:

v1[3]

the_week[3]


# e.g. to get the 5th and 5th elements of each vector:


v1[5:6]

the_week[5:6]



# e.g. to get the first, fourth, and sixth element of each vector:


v1[c(1,4,6)]

the_week[c(1,4,6)]





### Logical Operators

# bits of punctuation syntax that are logical operators: - let's see:

# ==   "is equals to"

2+2 == 4
2+2 == 5


# you can also check a vector
x <- c(5, 10, 15, 10, 1, 11, 10)
x

x == 10  # tells you which are 10


##  !=   "not equal to"

x != 10



# Other logical operators are 
#  <
#  <=
#  >
#  >= 

17 >= 15

17 < 15

x < 10

x <= 10

x > 10

x >= 10



## you can add up all the TRUES like this:

x == 10

sum(x==10)




### You can do some indexing with these too, to filter out what you want:

x

x[5]

x[x==10]

x[x!=10]

x[x>10]



