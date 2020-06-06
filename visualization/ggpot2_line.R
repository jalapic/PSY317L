## Line graph - connecting values (y-axis) over time (x-axis)

library(tidyverse)

jennifer <- read_csv("data/jennifer.csv")


head(jennifer)
tail(jennifer)


# Let's build up a line graph
ggplot()

ggplot(jennifer) 

ggplot(jennifer, aes() ) 

ggplot(jennifer, aes(x=year, y=n) ) 

ggplot(jennifer, aes(x=year, y=n) ) + geom_point() # look at data as points

ggplot(jennifer, aes(x=year, y=n) ) + geom_line() # instead use a line

# Can add point and line
ggplot(jennifer, aes(x=year, y=n) ) + 
  geom_point() +
  geom_line() 


# Can Change Color of Line
ggplot(jennifer, aes(x=year, y=n) ) + 
  geom_line(color = "purple")

# this doesn't color points though
ggplot(jennifer, aes(x=year, y=n) ) + 
  geom_point() +
  geom_line(color = "purple") 


# change both colors
ggplot(jennifer, aes(x=year, y=n) ) + 
  geom_point(color = "violet") +
  geom_line(color = "purple") 



# Customize axis labels and title
ggplot(jennifer, aes(x=year, y=n) ) + 
  geom_line(color = "purple") +
  xlab("Year") +
  ylab("Number of Children Born") +
  ggtitle("Popularity of Name Jennifer in USA")


# Change width of lines
ggplot(jennifer, aes(x=year, y=n) ) + geom_line()

ggplot(jennifer, aes(x=year, y=n) ) + geom_line(lwd=2)

ggplot(jennifer, aes(x=year, y=n) ) + 
  geom_line(color = 'purple', lwd=2)


# Change style of lines

ggplot(jennifer, aes(x=year, y=n) ) + geom_line()

ggplot(jennifer, aes(x=year, y=n) ) + geom_line(lty=1)
ggplot(jennifer, aes(x=year, y=n) ) + geom_line(lty=2)
ggplot(jennifer, aes(x=year, y=n) ) + geom_line(lty=3)
ggplot(jennifer, aes(x=year, y=n) ) + geom_line(lty=4)



## Plotting multiple lines on same graph

jenlinda <- read_csv("data/jenlinda.csv")

head(jenlinda)
tail(jenlinda)

ggplot(jenlinda, aes(x=year, y=n, color=name)) + geom_line()


ggplot(jenlinda, aes(x=year, y=n, color=name)) + 
  geom_line()+
  xlab("Year") +
  ylab("Number of Children Born") +
  ggtitle("Popularity of Names Jennifer & Linda in USA")


####################----------------------------##########################


### Try for yourself examples....

# remove the blanks, and replace with the appropriate word.

# 1. Import the texascovid dataset. This gives the cumulative number of deaths from covid19 for all of Texas (as of June 6th). The day column is the number of days since March 4th 2020.

# you may get a 'warning' about the Date column - it's ok.

covid.df <- read_csv("data/texascovid.csv")

head(covid.df)
tail(covid.df)

# plot a line graph of day (x-axis) against total (y-axis)

ggplot(_____, aes(x= _____, y= ______)) + geom_line()


# 2. Import the countycovid dataset.

covid.county <- read_csv("data/countycovid.csv")

head(covid.county)
tail(covid.county)

# plot a line graph of day (x-axis) against total (y-axis), and make separate lines for each county.


ggplot(_____, aes(x= _____, y= ______, color = _______)) + 
  geom_line()
