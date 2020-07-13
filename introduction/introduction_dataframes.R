#### Introduction to DataFrames

## Import the Data
library(tidyverse)

df <- read_csv("data/cheese.csv")


## Explore the Data

# look at top rows
head(df)

head(df, 4)

head(df, 8)

# look at bottom rows
tail(df)

tail(df, 10)


## Dimensions
nrow(df)  #73

ncol(df)  #9

dim(df) #73 9

length(df) #9


### colnames

head(df)

colnames(df)

colnames(df)[6] <- "carbo"

colnames(df)


## some basic carpentry

# create a new column
head(df)

df$food_type <- "cheese"

head(df)
tail(df)

df$food_type <- NULL

head(df)


### Indexing dataframes.

# the dollar sign. ....  indicates columns

df$chol

df$type

df$kcal

# get first 10 rows of kcal
df$kcal[1:10]


## Bit more on indexing.

df

df[]  # this is the same

df[,]  # this is the same

# rows before comma, columns after comma.

df[7,]  # this is the 7th row

df[10:14,]  # this is the 10th to 14th rows


# get only the 1st and 2nd columns

df[,1:2]

# get the 3rd, 5th and 9th column
df[,c(3,5,9)]

df[c(3,5,9)] #with columns, when using 'c()' you can ditch the comma

# get the 20th to 22nd row, and the 1st, 8th and 9th column
df[20:22,c(1,8,9)]

