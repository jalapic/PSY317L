### Customizing and Themes

library(tidyverse) 

df <- read_csv("data/BlueJays.csv")
head(df)


# Basic Scatterplot
ggplot(df, aes(x=Mass, y=Head) ) + geom_point()


ggplot(df, aes(x=Mass, y=Head) )  +
  geom_point() 


# Change the x-axis title
ggplot(df, aes(x=Mass, y=Head) ) + 
  geom_point() +
  xlab("Mass in grams")


# Change the y-axis title
ggplot(df, aes(x=Mass, y=Head) ) + 
  geom_point() +
  ylab("Head Size in mm")


# You can modify both at the same time
ggplot(df, aes(x=Mass, y=Head) ) + 
  geom_point() +
  xlab("Mass in grams") +
  ylab("Head Size in mm")
  



# Add a title
ggplot(df, aes(x=Mass, y=Head) ) + 
  geom_point() +
  ggtitle("Body Mass against Head Size in Blue Jays")


# Putting it all together
ggplot(df, aes(x=Mass, y=Head) ) + 
  geom_point() +
  ggtitle("Body Mass against Head Size in Blue Jays") +
  xlab("Mass in grams") +
  ylab("Head Size in mm")


## You can also change default themes

# Changing default background themes
ggplot(df, aes(x=Mass, y=Head) ) + 
  geom_point() +
  theme_minimal()

# Another option
ggplot(df, aes(x=Mass, y=Head) ) + 
  geom_point() +
  theme_classic()



####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.

# 1. For the films data, plot imdb against rottentomatoes and add an x-axis title, y-axis title and main title.  You can choose what to put in the titles.

films <- read_csv("data/films.csv")

ggplot(films, aes(x = imdb, y = rottentomatoes)) +
  geom_point()+
  xlab("IMDB Rating")+
  ylab("Rotten Tomatoes Rating")+
  ggtitle("Comparison of Rating Sites for Selected Films")

# 2. For the films data, plot imdb against metacritic and add an x-axis title, y-axis title and main title.  You can choose what to put in the titles.

head(films)

ggplot(films, aes(x = imdb, y = metacritic)) +
  geom_point()+
  xlab("IMDB Rating")+
  ylab("Metacritic Rating")+
  ggtitle("Rating Comparison for Films")


# 3. Change the theme of the following scatterplot to the Classic theme. (There is no blank here - you have to add something).

ggplot(films, aes(x=imdb, y=metacritic))+
  geom_point() +
  theme_classic()

