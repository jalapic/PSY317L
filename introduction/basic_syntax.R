# Basic Syntax


## Boring mathematical stuff


# You can add with the `+` key:
4 + 6


# Subtracting uses a hyphen or minus key `-`:
43 - 18


# Multiplying uses the asterisk `*`:
5 * 3


# Dividing uses this slash key `/`:
34 / 7



# Square-rooting
sqrt(45)


# Squaring 
12^2



# logarithms
log(12)



## Our first little warning


# the order you do things matters:


5 * 7 + 9   #44

7 + 9 * 5   #52

# best to use brackets
5 * (7 + 9) 




### Assignment


# `<-` which is just a "greater than sign" followed by a "hyphen" 
# is called an assignment operator. 
# It basically is equivalent to saying you want to save something.  


# the saved thing is known as an 'object'.


x <- 17  # Now, whenever you type, `x`, R thinks that it is the number 17.


x 

x + 2

x * 10



# you can assign/save lots of things, and you can call them almost anything 

hooray <- 17 + 4  
hooray

hooray + 5


hooray + x   






### Vectors

## vectors are 'objects'

# they contain things that are just put together - they are put into the vector in order
# like you'd put books on a bookshelf.


# the : sign means "to"

5:10

101:103



# the `c()` function. 
# stands for `concatenate` 
# it sticks things together.  

c(1,10,100,1000)

c(4,6,8,10,8,6,1)




v1 <- 5:10  

v1

v2 <- c(4,6,8,10,8,6,1)

v2





### Indexing vectors


# use square brackets to get individual things ('elements') out of vectors.

v1[1]  # 1st element of v1

v1[3] # 3rd element of v1

v2[3] # 3rd element of v2

v2[4]   # 4th element of v2



v1[1:3]  # 1st, 2nd and 3rd element


# getting non-consecutive elements of vectors. 
# e.g. the first, fourth and sixth elements of `v1` 
  
v1[c(1,4,6)]

# you can't do this...

v1[1,4,6]

# also it has to exist:
v2[100]


