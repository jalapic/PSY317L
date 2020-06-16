## Small multiples - 

# A good way of showing relationships across categories

library(tidyverse)



## Read in the penguins dataset

penguins <- read_csv("data/penguins.csv")

head(penguins)

# we might be interested in how body mass is associated with flipper length

ggplot(penguins, aes(x = body_mass_g,  y = flipper_length_mm)) + geom_point()


# but this mixes species and sexes...
# we could color...

ggplot(penguins, aes(x = body_mass_g,  y = flipper_length_mm, color = species)) + geom_point()


ggplot(penguins, aes(x = body_mass_g,  y = flipper_length_mm, color = sex)) + geom_point()


# if we make small multiples we can look at relationship across species and sex
# facet_wrap() makes the multiples

ggplot(penguins, aes(x = body_mass_g,  y = flipper_length_mm, color = sex)) + 
  geom_point() +
  facet_wrap(~ species)

# you can make the scales vary between panels...
ggplot(penguins, aes(x = body_mass_g,  y = flipper_length_mm, color = sex)) + 
  geom_point() +
  facet_wrap(~ species, scales = "free")




## Example 2 - scatterplot...

# this is a dataset of videogame ratings and sales

vg <- read_csv("data/videogames.csv")

head(vg)



# let's look at the categorical variables

table(vg$platform)

table(vg$genre)

table(vg$rating)

head(vg)

# Let's plot critic score against global sales

ggplot(vg, aes(x = critic, y = global_sales)) + geom_point()  # erm, there are some crazy high values


# what are the crazy high selling games?

vg %>% filter(global_sales > 20) %>% as.data.frame()
 

# logging the y-axis will help with readability...

ggplot(vg, aes(x = critic, y = log(global_sales))) + geom_point()


# but what if we wanted to look at this relationship by genre...
# let's add transparency to the points because there are so many of them
# and change the theme 

ggplot(vg, aes(x = critic, y = log(global_sales))) + 
  geom_point(alpha=.4) +
  theme_minimal() +
  facet_wrap(~genre)


ggplot(vg, aes(x = critic, y = log(global_sales), color=platform )) + 
  geom_point(alpha=.4) +
  theme_minimal() +
  facet_wrap(~genre)



#### Example 3 - A line graph example ....


# Let's look at life expectancy by year

df <- read_csv("data/lifeexp_all.csv")

head(df)


# notice here that we use 'group' and not 'color' to make separate lines...
# we have too many countries to have different colors !!!
# our major aim is to look at patterns across time...

ggplot(df, aes(x = year, y = lifeExp, group = country)) + geom_line()  # yuck !

# we could tidy up a bit...
# make the lines a light color and a bit transparent

ggplot(df, aes(x = year, y = lifeExp, group = country)) + 
  geom_line(color="cornflowerblue", alpha=0.4) +
  theme_minimal()

# better.... but it's still so hard to see what's going on.

# let's make multiples by continent

head(df)


ggplot(df, aes(x = year, y = lifeExp, group = country)) + 
  geom_line(color="cornflowerblue", alpha=0.4) +
  theme_minimal() +
  facet_wrap(~continent)



# sometimes making each panel a different color can help...

ggplot(df, aes(x = year, y = lifeExp, group = country, color=continent)) + 
  geom_line() +
  theme_minimal() +
  facet_wrap(~continent)


# adding some titles

ggplot(df, aes(x = year, y = lifeExp, group = country, color=continent)) + 
  geom_line() +
  theme_minimal() +
  facet_wrap(~continent) +
  xlab("Year") +
  ylab("Life Expectancy")

