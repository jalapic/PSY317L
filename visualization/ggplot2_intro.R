
### Introduction to ggplot2 


## Load libraries and data

library(tidyverse) 


df <- read_csv("data/BlueJays.csv")
head(df)


## Making a ggplot2 graph

ggplot()  #blank canvas

ggplot(df)   #just adding the datset to the blank canvas, but nothing plotted yet

ggplot(df, aes() )   #inside aes() we'll put what our x and y axis will be


ggplot(df, aes(x=Mass, y=Head) )   #we say what the x and y are, and it creates scales on each axis, but we didn't tell it what to plot yet


ggplot(df, aes(x=Mass, y=Head) ) + geom_point()


#What if we want to change the color of the points?
ggplot(df, aes(x=Mass, y=Head) ) + geom_point(color='red')



#What if we want to color the points based on another variable
ggplot(df, aes(x=Mass, y=Head, color = KnownSex) ) + geom_point() 






#####  Let's build up another example......

films <- read_csv("data/films.csv")


ggplot()

ggplot(films)

ggplot(films, aes() )

ggplot(films, aes(x=imdb, y=metacritic) )

ggplot(films, aes(x=imdb, y=metacritic) ) + geom_point()

ggplot(films, aes(x=imdb, y=metacritic) ) + geom_point(color='brown')







####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.

# 1. For the BlueJays (df) data, plot Head against Skull

ggplot(df, aes(x = Head,  y= Skull)) + geom_point()

# 2. For the BlueJays (df) data, plot Mass against BillLength and color by KnownSex

ggplot(df, aes(x = Mass,  y= BillLength, color = KnownSex)) + geom_point()


# 3. From the films data, plot imdb against rottentomatoes and make the points red.

ggplot(films, aes(x = imdb,  y= rottentomatoes)) + geom_point(color = 'red')


# 5. Load in the personality dataset, and plot extraversion against agreeableness.

personality <- read_csv("data/personality.csv")
head(personality)
ggplot(personality, aes(x=extraversion, y=agreeableness)) + geom_point()





##############################

### Some Other Things We Can Do, just for fun...


ggplot(df, aes(x=Mass, y=Head) ) + geom_point() 

#What if we want to change the size of the points ?
ggplot(df, aes(x=Mass, y=Head) ) + geom_point(size=2) 


# We can also make points transparent
ggplot(df, aes(x=Mass, y=Head) ) + geom_point(size=2, alpha=.5) 


#you can combine this with other modifications, e.g. color
ggplot(df, aes(x=Mass, y=Head, color = KnownSex) ) + geom_point(size=2) 

ggplot(df, aes(x=Mass, y=Head, color = KnownSex) ) + geom_point(size=2,alpha=.5) 

#this is the same, I just made the code more readable
ggplot(df, aes(x=Mass, y=Head, color = KnownSex) ) + 
  geom_point(size=2,alpha=.5) 


# you can also change the shape of the points

ggplot(df, aes(x=Mass, y=Head) ) + geom_point() 

ggplot(df, aes(x=Mass, y=Head) ) + geom_point(pch=3) 

ggplot(df, aes(x=Mass, y=Head, color=KnownSex) ) + geom_point(pch=3) 
