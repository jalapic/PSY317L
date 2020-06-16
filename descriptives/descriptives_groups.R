### Descriptives for Groups...





## do it by group
#3. Make a boxplot of the driveavg column.
#3. Make a boxplot of the driveavg column.

psych::describeBy(df, group="strain")


## Let's just focus on the swiss mice
swiss <- subset(df, strain=="Swiss")
swiss
swiss[,2:5]



# Load in the pga dataset
# this contains data on golf stats of PGA players 2004-2015

