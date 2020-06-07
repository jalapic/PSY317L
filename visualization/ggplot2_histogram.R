
### Introduction to Plotting Graphs in R (Histograms)


## Load libraries and data

library(tidyverse) 
df <- read_csv("films2015.csv")



## First I shall briefly explain what a histogram is.
# this is the most basic way of making a histogram in R
hist(df$imdb)
hist(df$imdb, breaks=20) 
hist(df$imdb, breaks=20, col="red") # quick but not pretty
hist(df$imdb, breaks=20, col="red", main="Histogram of IMDB Ratings")
hist(df$imdb, breaks=20, col="red", main="Histogram of IMDB Ratings", xlab="Rating")




## But we should learn how to use 'ggplot2' to make nicer looking charts...



## Making a ggplot2 histogram

ggplot()  #blank canvas

ggplot(df)   #just adding the datset to the blank canvas, but nothing plotted yet


ggplot(df, aes(imdb)) + geom_histogram()  # telling it that we want to plot imdb scores as a histogram


ggplot(df, aes(imdb)) + geom_histogram(color='white') #make border around bars white

ggplot(df, aes(imdb)) + geom_histogram(color='white', fill="blue") #make bars blue



ggplot(df, aes(imdb)) + geom_histogram(binwidth = 0.2) # tell it to make binwidth 0.2

ggplot(df, aes(imdb)) + geom_histogram(binwidth = 0.2, color="white") # tell it to make binwidth 0.2 and change color

ggplot(df, aes(imdb)) + geom_histogram(binwidth = 0.2, color="white", fill="red") # and fill with red


# change default theme using theme()
# note how you can go on successive lines-  but make sure a + is at the end of the row

ggplot(df, aes(imdb)) + 
  geom_histogram(binwidth = 0.2, color="white", fill="red") +
  theme_bw()

# keep adding things, e.g. title...  
ggplot(df, aes(imdb)) + 
  geom_histogram(binwidth = 0.2, color="white", fill="red") +
  theme_bw() +
  ggtitle("Histogram of IMDB Ratings")

# x and y axis labels
ggplot(df, aes(imdb)) + 
  geom_histogram(binwidth = 0.2, color="white", fill="red") +
  theme_bw() +
  ggtitle("Histogram of IMDB Ratings") +
  xlab("Rating") +
  ylab("Frequency")




### Some Fill in the gap examples

ggplot(battingavg, aes(____)) + geom_histogram() # Make a histogram of the batting average column (avg) in the battingavg dataset


ggplot(battingavg, aes(____)) + geom_histogram() # Make a histogram of the number of hits (H) in the battingavg dataset


ggplot(battingavg, aes(____)) + geom_histogram(________) 
# Make a histogram of the batting average column (avg) in the battingavg dataset - make the bindwidth 0.01

ggplot(________, aes(____)) + ___(_________) 
# Make a histogram of the batting average column (avg) in the battingavg dataset - make the bindwidth 0.01, make the outline color white



### Some write from scratch example

# Make histograms of the temperature distribution in Miami and San Francisco - compare and contrast.
# use the 'miami' and 'sanfran' datasets for this.



library(tidyverse)




## Data
library(babynames)
head(babynames)
tail(babynames)

girls <- subset(babynames, sex=="F")
girls

boys <- subset(babynames, sex=="M")
boys

since1950 <- subset(babynames, year>=1950)
since1950


## Number of unique names per year.

ggplot(boys)

ggplot(boys, aes(x=year))

ggplot(boys, aes(x=year)) + geom_bar()

ggplot(girls, aes(x=year)) + geom_bar()

ggplot(babynames, aes(x=year)) + geom_bar() + facet_wrap(~sex)

ggplot(babynames, aes(x=year)) + geom_bar(width=.7) + facet_wrap(~sex)



#girls boys on same chart
ggplot(since1950, aes(x=year, fill=sex)) + geom_bar()

ggplot(since1950, aes(x=year, fill=sex)) + geom_bar(position='dodge')


## If counts are pre-known use stat= identity

patricia <- babynames %>% filter(name=="Patricia")

patricia

patricia %>% arrange(-n)

ggplot(patricia, aes(x=year, y=n)) + geom_bar(stat='identity')

ggplot(patricia, aes(x=year, y=n)) + geom_bar(stat='identity', color='black', lwd=.5, fill='gray33',width=.9)






### Histograms ----

library(Lahman)

head(Batting)

Batting_sum <- Batting %>% 
  group_by(playerID) %>% 
  summarise(totalH = sum(H),
            totalAB = sum(AB),
            avg = totalH/totalAB
  )

Batting_sum <- Batting_sum %>% filter(totalAB>200)


hist(Batting_sum$avg) #quick look using base-r


ggplot(Batting_sum, aes(x=avg)) + geom_histogram()

ggplot(Batting_sum, aes(x=avg)) + geom_histogram(color='darkgreen',fill='lightgreen')

ggplot(Batting_sum, aes(x=avg)) + geom_histogram(bins = 150,color='darkgreen',fill='lightgreen')

ggplot(Batting_sum, aes(x=avg)) + geom_histogram(binwidth = .005, color='darkgreen',fill='lightgreen')

ggplot(Batting_sum, aes(x=avg)) + geom_density()

ggplot(Batting_sum, aes(x=avg)) + geom_density(fill='mistyrose')

# default for histogram is count, but can make it density like this
ggplot(Batting_sum, aes(x=avg)) + geom_histogram(aes(y=..density..),color='darkgreen',fill='lightgreen')


## Add a line
ggplot(Batting_sum, aes(x=avg)) + 
  geom_histogram(bins = 150,color='darkgreen',fill='lightgreen') +
  geom_vline(aes(xintercept=0.3), color="black", linetype="dashed", size=1)




### Side by side Histograms----

# e.g. players prior to 1920 vs players after 1990

Batting_early <- Batting %>%
  filter(yearID<=1920) %>%
  group_by(playerID) %>% 
  summarise(totalH = sum(H),
            totalAB = sum(AB),
            avg = totalH/totalAB
  ) %>%
  mutate(period='early')


Batting_late <- Batting %>%
  filter(yearID>=1990) %>%
  group_by(playerID) %>% 
  summarise(totalH = sum(H),
            totalAB = sum(AB),
            avg = totalH/totalAB
  ) %>%
  mutate(period='late')


Batting_early
Batting_late

Batting_all <- rbind(Batting_early, Batting_late)

Batting_all <- Batting_all %>% filter(totalAB>100)


# Overlaid histograms
ggplot(Batting_all, aes(x=avg, fill=period)) +  geom_histogram(position="identity")

ggplot(Batting_all, aes(x=avg, fill=period)) +  geom_histogram(position="identity", alpha=.7, binwidth=.005)

ggplot(Batting_all, aes(x=avg, fill=period)) +  geom_histogram(position="identity", alpha=.7, binwidth=.005) + facet_wrap(~period)

ggplot(Batting_all, aes(x=avg, fill=period)) +  geom_density(alpha=.7, binwidth=.005) 

# Interleaved histograms
ggplot(Batting_all, aes(x=avg, fill=period)) +  geom_histogram(position="dodge")







### Extra:   Back to Back histograms  (see advanced tutorial)

# e.g. popularity of unisex names like Skylar over decades
# e.g. population pyramids






####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.


