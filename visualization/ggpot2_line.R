## Line graph - connecting values (y-axis) over time (x-axis)

library(babynames)
jennifer <- babynames %>% filter(name=="Jennifer" & sex=="F")
jennifer$sex <- gsub("F", "Female", jennifer$sex)
write.csv(jennifer,"jennifer.csv",row.names = F)


jenlinda <- babynames %>% filter(name=="Jennifer" | name=="Linda") %>%
  filter(sex=="F")

jenlinda$sex <- gsub("F", "Female", jenlinda$sex)
write.csv(jenlinda,"jenlinda.csv",row.names = F)


jennifer <- read_csv("jennifer.csv")


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


# this doesn't color points purple though
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

jenlinda <- read_csv("jenlinda.csv")

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

# 1. For the covid19austin dataset, 
covid19austin <- read_csv("covid19austin.csv")
head(covid19austin)
tail(covid19austin)


# 2. 


