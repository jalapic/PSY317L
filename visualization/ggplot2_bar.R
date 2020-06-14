
### Making Bar Graphs  - Visualizing Amounts


library(tidyverse)


## Example 1. If you have a dataset where you HAVE already counted the number of each group


## Import the number1s.csv data.  This shows how many number 1 songs various artists have had in the UK singles charts.


df <- read_csv("data/number1s.csv")
head(df)

ggplot()

ggplot(df)

ggplot(df, aes() )

ggplot(df, aes(x = name, y = total) )

ggplot(df, aes(x = name, y = total) ) + geom_col()

# we use geom_col() when the data is already counted.



## notice default order is alphabetical.

# you can reorder by putting reorder around the x-axis column...

ggplot(df, aes(x = reorder(name, total), y = total) ) + geom_col()

ggplot(df, aes(x = reorder(name, -total), y = total) ) + geom_col()


## changing color... notice we use 'fill' here because it's a shape...
ggplot(df, aes(x = reorder(name, -total), y = total) ) + 
  geom_col(fill = "yellow")

# color changes the border of the bars...
ggplot(df, aes(x = reorder(name, -total), y = total) ) + 
  geom_col(fill = "yellow", color="black")


# and you can customize....
ggplot(df, aes(x = reorder(name, -total), y = total) ) + 
  geom_col(fill = "orange", color="brown") +
  xlab("Artist") +
  ylab("Total Number 1's") +
  ggtitle("Number 1 hits in UK") +
  theme_classic()



### Let's do another example....

# read in the world series dataset
ws <- read_csv("data/worldseries.csv")
ws  # it's a list of how many world series each team has.
tail(ws)


# make plot
ggplot(ws, aes(x = Team, y = Wins)) + geom_col()


# it looks awful... even reordering doesn't help....
ggplot(ws, aes(x = reorder(Team, -Wins), y = Wins)) + geom_col()



# but flip onto horizontal... not quite right yet....
ggplot(ws, aes(x = reorder(Team, -Wins), y = Wins)) + 
  geom_col() +
  coord_flip()

# better ...
ggplot(ws, aes(x = reorder(Team, Wins), y = Wins)) + 
  geom_col() +
  coord_flip()

#customize ...
ggplot(ws, aes(x = reorder(Team, Wins), y = Wins)) + 
  geom_col(fill="dodgerblue", color="navy") +
  coord_flip() +
  xlab("") +
  ylab("World Series Wins") +
  ggtitle("Total World Series Wins 1903-2019")




####

## Example 2. If you have a dataset where you haven't already counted the number of each group


# We want to plot how many people have each type of pet.
# pet on x-axis, count on y-axis.

# load in pets.csv dataset
pets <- read_csv("data/pets.csv")

# look at data
head(pets)
tail(pets)


# Notice 'pet' is categorical.

# We don't yet have a 'count' of how many of each pet there is.
# We can quickly look at like this:

table(pets$pet)


# We can make ggplot plot the frequency/count of each pet as a bar graph

# notice here, we use "geom_bar()" - and it counts for us.
# we do not need to supply a y column.
# we only supply the x column

ggplot(pets, aes(x = pet)) + geom_bar()


# customize...

ggplot(pets, aes(pet)) + 
  geom_bar(color="black", fill="plum3") +
  xlab("Pet")+
  ylab("Total")+
  ggtitle("Popularity of Pets in a Class")



####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.


## Example 1.
# import the skittles data. This shows the number of each color of in a pack of skittles
# then make a bar graph of these data with color on the x-axis and number on the y-axis

skittles <- read_csv("data/skittles.csv")
head(skittles)

ggplot(skittles, aes(x= color, y = number)) +  geom_col()


# next redo that plot, but reorder the x-axis from highest to lowest in number

ggplot(skittles, aes(x= reorder(color, -number), y = number)) +  
  geom_col()





## Example 2.
# load the catcolor data set. 
# this is a sample of data taken from Austin Cat Rescue records

# notice there are no summary counts of the data.
# there are 13859 rows of data - each one a different cat.

catcolor <- read_csv("data/catcolor.csv")
head(catcolor)
tail(catcolor)

table(catcolor$color1) # this gives us a quick summary of the totals for each color


# plot a bar graph of color1 on the x-axis, and counts on the y-axis

ggplot(catcolor, aes(x=color1)) + geom_bar()

# next, flip the axes, so the bars are going horizontally.

ggplot(catcolor, aes(x=color1)) + geom_bar() + coord_flip()


# reordering the order of the bars when using geom_bar() is a bit trickier....
# I may discuss that in future videos.
