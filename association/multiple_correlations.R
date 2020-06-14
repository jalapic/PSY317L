#### Multiple Correlations

# Sometimes you wish to examine multiple correlations
# it can be annoying to type out cor.test() multiple times.
# you can look at multiple correlations at once.


### loading libraries ----

library(tidyverse)
library(Hmisc) # for multiple correlations.
library(psych)


### loading data ----

jays <- read_csv("data/BlueJays.csv")

jayM <- jays %>% filter(KnownSex == "M") # we'll just look at Males

nrow(jayM) # 63 observations

head(jayM)


## One correlation e.g.
cor.test(jayM$Mass, jayM$Head)


## Getting a correlation matrix.----

# We can actually get the correlations between all numerical columns at once.

head(jayM)

jayM[,6:7] # we just got the correlation between the 6th and 7th columns
jayM[,3:8] # but the numerical data is in the 3rd through 8th columns....

cor(jayM[,6:7])  # so we did this above....

cor(jayM[,3:8])  # but we could do this to see all the correlations

plot(jayM[,3:8]) # and this plots them all.




## A matrix of correlation values and p-values----

as.matrix(jayM[,3:8]) # you have to put as.matrix() around your data

jaydata <- as.matrix(jayM[,3:8]) # you have to put as.matrix() around your data

 
Hmisc::rcorr(jaydata) # use 'rcorr' from package Hmisc

Hmisc::rcorr(jaydata, type="spearman")  # to do non-parametric



## Controlling for multiple comparisons ----

# use 'corr.test' from psych package

corr.test(jaydata, method="pearson", adjust="holm")

corr.test(jaydata, method="spearman", adjust="bonferroni")

# What adjustment for multiple tests should be used? 
# ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"). 
# See p.adjust for details about why to use "holm" rather than "bonferroni").








