### Basic Data Carpentry with tidyverse

## We'll learn about:

# filter()    - subsetting data
# select()    - selecting columns
# arrange()   - sorting a column
# mutate()    - adding a new column

#    %>% means "and next do this"
#    ==  means "is equal to"
#    != means "is not equal to"
#     |  means "or"



library(tidyverse) 


df <- read.csv("data/videogames.csv")

head(df)
tail(df)

## table summarizes categorical variables.

table(df$platform)
table(df$genre)
table(df$genre, df$platform)


  
## "filter" to subset data.

# you can filter several variables at a time, or just one:

df %>% filter(genre == "Sports")

df %>% filter(genre == "Sports", year == 2010)

df %>% filter(genre == "Sports", year == 2010, critic > 80)

#save as different object
sports2010 <- df %>% filter(genre == "Sports", year == 2010, critic > 80)

sports2010


# Just a note:  != means 'not equal to'
df %>% filter(genre == "Sports", year != 2010)


### Use "select" to keep certain columns.

head(df)
colnames(df)

df %>% select(name, platform, year, genre, critic, global_sales)

# you can rename columns as you go
df %>% select(title = name, platform, year, genre, critic, sales = global_sales)

# save as a different object
df2 <- df %>% select(title = name, platform, year, genre, critic, sales = global_sales)

df2


colnames(df)

# you can select by column number too:

df %>% select(1,2,3,4,9,10)

df %>% select(1:4,9,10)



#### Another example:

pga <- read_csv("data/pga.csv")

head(pga)

nrow(pga)

ncol(pga)

colnames(pga)

# let's pick out name, year, total.holes, total.putts, score.avg columns

pga %>% select(name, year, total.holes, total.putts, score.avg) 


# save as pga1

pga1 <- pga %>% select(name, year, total.holes, total.putts, score.avg)  


### We can sort a column using arrange()

head(pga1)

pga1 %>% arrange(score.avg)  # sorts data in ascending order
  
pga1 %>% arrange(-score.avg)  # sorts data in descending order


pga1 %>% arrange(name)  # sorts data in ascending alphabetical order


pga1 %>% arrange(year, score.avg)  # sorts data first by year, then by score


### you can add a new column with 'mutate'.
head(pga1)

# let's add a column called putt.avg that is the putting average
# this is total putts divided by total holes.

## there are two ways:

pga1 %>% mutate(putt.avg = total.putts / total.holes)

pga1 <- pga1 %>% mutate(putt.avg = total.putts / total.holes)

pga1

# directly
pga1$putt.avg1 <- pga1$total.putts / pga1$total.holes



#### We could have done all of that in one go:

pga %>%
  select(name, year, total.holes, total.putts, score.avg)  %>%
  mutate(putt.avg = total.putts / total.holes) %>%
  arrange(putt.avg)




###  Last quick example  (select & filter)



cats <- read_csv("data/austin_cat.csv")

head(cats)

nrow(cats)

ncol(cats)

## Select animal_id, name,breed, color1, color2, sex, outcome_type

cats %>% select(animal_id, name, breed, color1, color2, sex, outcome_type)

cats1 <- cats %>% select(animal_id, name, breed, color1, color2, sex, outcome_type)


# let's look at the breeds.

table(cats1$breed)


# say we wanted to look at "american shorthair" cats

cats1 %>% filter(breed == "american shorthair")


# what about if we wanted to look at "angora" OR "balinese" cats

cats1 %>% filter(breed == "balinese" | breed == "angora")  # | means 'or'




################   Try these examples, fill in the blank ------

### Import the bloodwork dataset

blood <- read_csv("data/bloodwork.csv")
blood

# 1. Filter to only keep rows where smoker is equal to 'yes'

blood %>% filter(smoker ==  "yes")


# 2. Filter to only keep rows where children is equal to 0

blood %>% filter(children == 0)


# 3. Filter to only keep rows where bpsyst is greater than 115 and hrate is greater than 70.

blood %>% filter(bpsyst > 115, hrate > 70)


# 4. Select the columns ids, age, smoker, hrate and arrange by hrate (descending order)

blood %>%
  select(ids, age, smoker, hrate) %>%
  arrange(-hrate)


# 5. Select the columns ids, state, immuncount, immuncount2, then create a new column called "tot_immun", that is the sum of 'immuncount' and 'immuncount2' 

blood %>%
  select(ids, state, immuncount, immuncount2) %>%
  mutate(tot_immun = immuncount + immuncount2)



# 6. Filter to only include rows that have the state as "NJ" or "NY".

blood %>% filter(state == "NJ"  |  state == "NY")





