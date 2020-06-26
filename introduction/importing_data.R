### Importing Data.

# There are a few options for working with data in RStudio.


## 1. Using the import tab in RStudio.

# e.g. let's import the cheese.csv dataset





##. 2. You can import directly using the "read_csv()" command.


library(tidyverse)

read_csv("data/BlueJays.csv") 


df <- read_csv("data/BlueJays.csv") 


# Some basic dataframe functions 
head(cheese) # see top 6 rows

tail(cheese) # see bottom 6 rows

nrow(cheese) # how many rows in dataset

ncol(cheese) # how many columns in dataset

colnames(cheese) # column names




cheese   # look at data in console (if used read_csv it will be a truncated neat output)


##### Try For Yourself Examples ------

## 1. Import the dataset called "skittles.csv"



## 2. Import the dataset called "bmi.csv"


## 3. Use the functions above to find the number of rows, columns, and colnames.






