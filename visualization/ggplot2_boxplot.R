#### Boxplots ####


library(tidyverse)


### Example 1...


# import and look at data

wheels <- read_csv("data/wheels1.csv")
head(wheels)
tail(wheels)


# we can use the following to get a bit more summary information

table(wheels$strain)

summary(wheels)



## Let's plot the data to examine the data:

# you can just plot the raw data points....

ggplot()

ggplot(wheels)

ggplot(wheels, aes() )

ggplot(wheels, aes(x = strain, y = total))

ggplot(wheels, aes(x = strain, y = total)) + geom_point()


# or instead, you can summarize the data with boxplots:

ggplot(wheels, aes(x = strain, y = total)) + geom_boxplot()  #notice the outlier

# the warning message is ok - it's saying some rows don't have data.



# you can overlay the points on the boxplot

ggplot(wheels, aes(x = strain, y = total)) + geom_boxplot() + geom_point()



# you can change the fill and the color of boxplots

ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot(color="navy", fill="dodgerblue") 


# with points

ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot(color="navy", fill="dodgerblue") +
  geom_point(color="navy")



### you can also make the points wobble a bit... 
# but careful it goes crazy...

ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot(color="navy", fill="dodgerblue") +
  geom_jitter(color="navy")

# you can constrain it by setting a width value of how much wobble...
ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot(color="navy", fill="dodgerblue") +
  geom_jitter(color="navy", width = .1) 

# add titles and labels etc.
ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot(color="navy", fill="dodgerblue") +
  geom_jitter(color="navy", width = .1) +
  xlab("Mouse Strain") +
  ylab("Total Wheel Revolutions") +
  ggtitle("Wheel Running By Different Mouse Strains") +
  theme_classic()


# again, if you prefer on it's side, you can flip the axes:
ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot(color="navy", fill="dodgerblue") +
  geom_jitter(color="navy", width = .1) + 
  coord_flip()



## a couple of other things you might want to try:

## Make the outlier more noticeable
ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot()
  

ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot(outlier.color = "green", outlier.size = 8)


# Some people prefer violin shapes to boxplots...
ggplot(wheels, aes(x = strain, y = total)) +  geom_violin()



## You can also color according to the x-axis category...

ggplot(wheels, aes(x = strain, y = total)) + 
  geom_boxplot()


ggplot(wheels, aes(x = strain, y = total, fill = strain)) + 
  geom_boxplot()


# adding points
ggplot(wheels, aes(x = strain, y = total, fill = strain)) + 
  geom_boxplot() +
  geom_jitter(width=.1)




#### Quick Second Example..

## Import the bloodwork dataset

bloodwork <- read_csv("data/bloodwork.csv")
head(bloodwork)
tail(bloodwork)

# Let's make a boxplot looking at 'state' on the x-axis and 'immuncount' on the y-axis

ggplot(bloodwork, aes(x = state, y = immuncount)) + geom_boxplot()


# add color based on state
ggplot(bloodwork, aes(x = state, y = immuncount, fill = state)) + geom_boxplot()

# add some points over the top
ggplot(bloodwork, aes(x = state, y = immuncount, fill = state)) + 
  geom_boxplot() +
  geom_point()

# or to make more wobbly
ggplot(bloodwork, aes(x = state, y = immuncount, fill = state)) + 
  geom_boxplot() +
  geom_jitter(width=.1)


# Did you notice something about the outliers ???


# to remove outliers do the following.
ggplot(bloodwork, aes(x = state, y = immuncount, fill = state)) + 
  geom_boxplot(outlier.shape=NA) 


# So now there is the correct number of points...
ggplot(bloodwork, aes(x = state, y = immuncount, fill = state)) + 
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(width=.1)

# entering a number gives you different shapes
ggplot(bloodwork, aes(x = state, y = immuncount, fill = state)) + 
  geom_boxplot(outlier.shape=4) 





####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.


## Example 1.

# import the BlueJays dataset.
# make a boxplot of "KnownSex" on the x-axis and "Mass" on the y-axis.

BlueJays <- read_csv("data/BlueJays.csv")
head(BlueJays)
tail(BlueJays)

ggplot(BlueJays, aes(x= KnownSex, y = Mass)) + geom_boxplot()


# now customzie the fill and color of the boxplot. Choose your own colors

ggplot(BlueJays, aes(x= KnownSex, y = Mass)) + 
  geom_boxplot(fill="mistyrose", color="purple")


# now make the fill of the boxplots different dependent on KnownSex

ggplot(BlueJays, aes(x= KnownSex, y = Mass, fill = KnownSex) ) + 
  geom_boxplot()


    
## Example 2.

#import the MetroCommutes dataset
metro <- read_csv("data/MetroCommutes.csv")
head(metro)
tail(metro)
table(metro$City)

# make a boxplot of City on the x-axis and Time on the y-axis. 


ggplot(metro,  aes(x=City, y= Time)) + geom_boxplot() 


# make the same plot, but this time flip the x and y axes.

ggplot(metro,  aes(x=City, y= Time)) + geom_boxplot()  + coord_flip()


# keep the same plot, but this time instead of a boxplot do a violin plot

ggplot(metro,  aes(x=City, y= Time)) + geom_violin()  + coord_flip()




