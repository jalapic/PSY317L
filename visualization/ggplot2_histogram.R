
### Introduction to Plotting Graphs in R (Histograms)


## Load libraries and data

library(tidyverse) 
df <- read_csv("data/films.csv")

head(df)


## Making a ggplot2 histogram

ggplot()  #blank canvas

ggplot(df)   #just adding the datset to the blank canvas, but nothing plotted yet


ggplot(df, aes(x=imdb)) + geom_histogram()  # telling it that we want to plot imdb scores as a histogram


ggplot(df, aes(x=imdb)) + geom_histogram(color='white') #make border around bars white

ggplot(df, aes(x=imdb)) + geom_histogram(color='white', fill="dodgerblue") #make bars dodgerblue



# you can also control the bin width of histograms..

ggplot(df, aes(x = imdb)) + 
  geom_histogram(binwidth = 0.2, color="white", fill="dodgerblue") # has a bin width of 0.2 rating points



# further you can control where the boundaries of each bin lie on the x-axis....

ggplot(df, aes(x = imdb)) + 
  geom_histogram(binwidth = 0.2, color="white", fill="dodgerblue",boundary=4) # makes boundary line up 

# just be careful with using the boundaries that it does not crop your histogram incorrectly...




## Customizing a little...

ggplot(df, aes(x = imdb)) + 
  geom_histogram(binwidth = 0.2, color="white", fill="dodgerblue",boundary=4) +
  theme_classic() +
  ggtitle("Histogram of IMDB Ratings") +
  xlab("Rating") +
  ylab("Frequency")



### Instead of Viewing as a histogram, you can also show the data as a density curve...

ggplot(df, aes(x = imdb)) +  geom_density(color = "navy", fill = "dodgerblue") 
  

# alpha gives it a bit of transparency
ggplot(df, aes(x = imdb)) +  
  geom_density(color = "navy", fill = "dodgerblue", alpha=.4)



### Second Example

# load in the data "batting.csv".
# this is the career batting averages of all MLB players with at least 200 at bats

bats <- read_csv("data/batting.csv")

head(bats)
tail(bats)


ggplot(bats, aes(x=avg)) + geom_histogram()


ggplot(bats, aes(x=avg)) + 
  geom_histogram(binwidth = .005, color='darkgreen',fill='lightgreen')


ggplot(bats, aes(x=avg)) + geom_density(alpha=0.7, fill='mistyrose')


# you can actually overlay these...

ggplot(bats, aes(x = avg))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  theme_minimal()




ggplot(bats, aes(x=avg))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.7, fill = "mistyrose") + 
  theme_minimal() +
  xlab("Batting Average") +
  ylab("Density") +
  ggtitle("MLB Batting Averages by Player - all time")



## Add a line over a histogram

ggplot(bats, aes(x=avg)) + 
  geom_histogram(binwidth = .005, color='darkgreen',fill='lightgreen')

ggplot(bats, aes(x=avg)) + 
  geom_histogram(binwidth = .005, color='darkgreen',fill='lightgreen') +
  geom_vline(xintercept=0.3, color="black", lty=2, size=1)




### Side by side Histograms - Comparing Distributions


life <- read_csv("data/lifeexp.csv")

head(life)
tail(life)


# let's plot a histogram of life Expectancy across all countries

ggplot(life, aes(x=lifeExp)) + 
  geom_histogram(color='white', fill='lightseagreen') #warning is ok



# But we've combined data from the two years - we should separate histograms for each year

# Overlaid histograms

ggplot(life, aes(x=lifeExp, fill=year)) +  
  geom_histogram(position="identity") #horrible

# add a defined binwith and add an alpha level
ggplot(life, aes(x=lifeExp, fill=year)) +  
   geom_histogram(position="identity", alpha=.5, binwidth=2)


# add defined colors
# and make border of bars black
ggplot(life, aes(x=lifeExp, fill=year)) +  
  geom_histogram(position="identity", alpha=.5, binwidth=2) +
   scale_fill_manual(values = c("#999999", "#E69F00"))


#make borders of bars black as well
ggplot(life, aes(x=lifeExp, fill=year)) +  
  geom_histogram(position="identity", alpha=.5, binwidth=2, color='black') +
  scale_fill_manual(values = c("#999999", "#E69F00"))



### you can also compare density plots - usually simpler to compare
  
ggplot(life, aes(x=lifeExp, fill=year)) +  
  geom_density(aes(fill = year), alpha = 0.4) 

# adding customized colors
ggplot(life, aes(x=lifeExp, fill=year)) +  
  geom_density(aes(fill = year), alpha = 0.4) +
  scale_fill_manual(values = c("#999999", "#E69F00"))
  
# changing theme
ggplot(life, aes(x=lifeExp, fill=year)) +  
  geom_density(aes(fill = year), alpha = 0.4) +
  scale_fill_manual(values = c("#999999", "#E69F00"))  + 
  theme_classic()

  




####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.


# Make a histogram of the number of hits (totalH) in the bats dataset
head(bats)
ggplot(bats, aes(x = totalH)) + geom_histogram()


# Make a histogram of the batting average column (avg) in the bats dataset - make the bindwidth 0.01
ggplot(bats, aes(x = avg)) + geom_histogram(binwidth = 0.01) 

# Make a histogram of the batting average column (avg) in the bats dataset - 
# make the bindwidth 0.01, make the outline color white
ggplot(bats, aes(x = avg)) + geom_histogram(binwidth = 0.01, color="white") 



# Load in the miami.csv dataset
# this shows the average daily temperature for miami in 2017

miami <- read_csv("data/miami.csv")
head(miami)

# make a histogram of the avgtemp column, make the fill color a light blue
ggplot(miami, aes(x = avgtemp)) + geom_histogram(fill="cyan")

# now change the histogram to a density plot and fill with navy color and set the alpha at 0.2
ggplot(miami, aes(x = avgtemp)) + geom_density(alpha=0.2, fill="navy")




# overlay the density plot over the histogram for the last two graphs
ggplot(miami, aes(x = avgtemp))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "cyan") + 
  geom_density(alpha = 0.2, fill = "navy") + 
  theme_minimal()


