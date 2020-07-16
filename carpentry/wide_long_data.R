
### Wide and Long Data

# sometimes called "Wide" and "Narrow" Data

# Long Data is often called "Tidy Data"



library(tidyverse)

# Data can be wide or long

df.wide <- data.frame(
                 name = c("James", "Tyler", "Stephen", "Jennifer", "Carmen"),
                 time1 = c(15, 17, 14, 13, 11),
                 time2 = c(16, 19, 20, 21, 23)
                 )


# wide data has more than one score of each type per individual in rows
df.wide


# long data (also called tidy data) has only one column for each type of score

df.long <- data.frame(
                 name = c("James", "Tyler", "Stephen", "Jennifer", "Carmen"),
                 score = c(15, 17, 14, 13, 11, 16, 19, 20, 21, 23),
                 time = rep(c("time1", "time2"), each = 5)
)

df.long # Every row has the information to make each score unique



# `tidyverse` has built in functions that can turn your dataframe from 
#  long format to wide format and vice versa. 

# `pivot_longer` makes the dataframes longer [gather]
# `pivot_wider` makes the dataframes longer [spread]

df.wide

# turn columns 2 and 3 into one long column:
df.wide %>% pivot_longer(cols = 2:3, names_to="time")  



### What if we had other columns in our dataset?

df.wide

df.wide$group <- c("control", "control", "treatment", "treatment", "treatment")

df.wide

df.wide %>% pivot_longer(cols = 2:3, names_to="time")  




## going wider:

df.long

df.long %>% pivot_wider(values_from = score, names_from = time)


# if we had more columns in our dataset:

df.long

df.long$confidence <- c(3, 8, 9, 4, 10)

df.long

df.long %>% pivot_wider(values_from = score, names_from = time)




### Real Data Example.


wheel <- read_csv("data/wheels1.csv")

head(wheel)

# here, say we were interested in the wheel running by day data

wheeldf <- wheel %>% select(id, strain, day1,day2,day3,day4)

head(wheeldf)

tail(wheeldf)

#conver to long
wheeldf %>% pivot_longer(cols = 3:6, names_to="day")  

wheeldf.long <- wheeldf %>% pivot_longer(cols = 3:6, names_to="day")  


## We could now get summary data and plot a graph
# nb. don't worry about this code... we'll go through it in later sections:
wheeldf.long %>% 
  group_by(day,strain) %>%
  summarise(mean = mean(value, na.rm=T)) %>%
  ggplot(aes(x=day,y=mean,group=strain, color=strain)) +
  geom_line(lwd=1) +
  xlab("day")+
  ylab("Daily Revolutions")



wheeldf.long

# to make it go wider again, we do:

wheeldf.long %>% pivot_wider(
                 names_from = day,
                values_from = value
                    )




### Try for yourself example

# 1. Load the data "lifexp.csv"
# this show life expectancy, population size and gdp for many countries over two time points

df1 <- read_csv("data/lifeexp.csv")

# we only need these columns
df1 <- df1 %>% select(country, continent, year, lifeExp)
df1

# make this data wide format putting lifeExp into two separate columns based on the 'year' column.

df1 %>% pivot_wider(
  names_from = year,
  values_from = lifeExp
)


## 2. Load the dataset "hurricanes.csv"
# this shows the number of hurricanes in each of the last 5 decades in 4 states
# it's in wide format

hu <- read_csv("data/hurricanes.csv")
hu

# make this dataset into long format, calling the decade column 'decade', 

hu %>% 
  pivot_longer(
    cols = 2:6, 
    names_to="decade")  

