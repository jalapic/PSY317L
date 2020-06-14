



#############################







## Getting a correlation matrix.----

# We can actually get the correlations between all numerical columns at once.


head(jayM)

jayM[,6:7] # we just got the correlation between the 6th and 7th columns
jayM[,3:8] # but the numerical data is in the 3rd through 8th columns....

cor(jayM[,6:7])  # so we did this above....

cor(jayM[,3:8])  # but we could do this to see all the correlations

plot(jayM[,3:8]) # and this plots them all.







## A matrix of correlation values and p-values----


#examMatrix <- as.matrix(exams[, c("exam", "anxiety", "revise")])

examMatrix <- as.matrix(exams[, 2:4])  # converts data into a 'matrix' format

Hmisc::rcorr(examMatrix) # use 'rcorr' from package Hmisc


#         revise  exam anxiety
# revise    1.00  0.40   -0.66
# exam      0.40  1.00   -0.43
# anxiety  -0.66 -0.43    1.00
# 
# n
#         revise exam anxiety
# revise     103  103     102
# exam       103  103     102
# anxiety    102  102     102
# 
# P
#         revise exam anxiety
# revise          0    0     
# exam     0           0     
# anxiety  0      0          



Hmisc::rcorr(examMatrix, type="spearman")



# we can also get quick inspection of all relevant scatterplots by choosing columns of interest
plot(exams[,2:4]) 
plot(exams[c(2,3,4)]) #another way of writing the above line - better if have non consecutive columns





## Controlling for multiple comparisons ----

psych::corr.test(examMatrix, method="spearman", adjust="holm")

psych::corr.test(examMatrix, method="spearman", adjust="bonferroni")

# What adjustment for multiple tests should be used? 
# ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"). 
# See p.adjust for details about why to use "holm" rather than "bonferroni").








