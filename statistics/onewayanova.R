


#### Some intro stats... ####

p1 + xlab("Mouse Strain") + ylab("Total wheel revolutions") # to remind you of this chart

p1 + xlab("Mouse Strain") + ylab("Total wheel revolutions") +
  theme(legend.position = "none")                                        #gets rid of legend (there are a million legend options)



#### One Way Anova ####

# Data needs to be formatted in:   
# 1) column or vector containing continuous variable 
# 2) column or vector containing categorical variable

aov(total ~ strain, data=wheels1)

aov1 <- aov(total ~ strain, data=wheels1)  #the ~ means "is a function of"

summary(aov1)

summary(aov(total ~ strain, data=wheels1)) #we could have just done this in the first place



# Post-hoc tests  (Tukey HSD test)

TukeyHSD(aov1) 

#  Tukey multiple comparisons of means
#  95% family-wise confidence level
#
#  Fit: aov(formula = total ~ strain, data = wheels1)
#  
#  $strain
#                           diff        lwr       upr     p adj
#  F1-129B6-B6         3726.2630 -12408.324 19860.850 0.9667613
#  F1-B6129-B6         5181.2524 -12356.394 22718.899 0.9214137
#  S129-B6           -14262.0060 -32827.820  4303.808 0.2108483
#  Swiss-B6            -413.7143 -18590.963 17763.535 0.9999962
#  F1-B6129-F1-129B6   1454.9894 -14347.513 17257.492 0.9990053
#  S129-F1-129B6     -17988.2689 -34924.605 -1051.933 0.0318353
#  Swiss-F1-129B6     -4139.9773 -20649.441 12369.487 0.9554041
#  S129-F1-B6129     -19443.2583 -37721.213 -1165.304 0.0314465
#  Swiss-F1-B6129     -5594.9667 -23478.103 12288.169 0.9048754
#  Swiss-S129         13848.2917  -5044.219 32740.802 0.2526450

# quite strict as comparing all pairwise comparisons.  

# Things to consider:
# Do you have a priori hypotheses for group differences
# Are your data normally distributed ?  and are variances bt groups equal ?


# Doing multiple t-tests and adjusting p-value to reduce type I error:
pairwise.t.test(wheels1$total,wheels1$strain,p.adjust.method="holm") #more on holm in a minute 





## another way in baseR....

oneway.test(total ~ strain, data=wheels1)              #applies a Welch correction for nonhomogeneity (adjusts df's)
oneway.test(total ~ strain, data=wheels1, var.equal=T) #turns off Welch correction

# read up on when to use WC and when not to - e.g. not great for very unequal group sizes.
#  http://biol09.biol.umontreal.ca/BIO2041e/Correction_Welch.pdf





#### Testing For Homogeneity of Variance (i.e. do all comparison groups have equal variances) ####

plot(aov1)  # the output from the standard ANOVA aov()  

# a bit of an  effect of larger residuals for larger fitted values -
# this is  'heteroscedascity' - 
# variance in the response might not be equal across groups
# variance might have a specific relationship with the size of the response.


# 1. heteroscedascity plot
# 2. qqplot (see below)
# 3. this looks at whether residuals vary with fitted values (e.g. here they go up)
# 4. This gives an idea of which levels of the factor are best fitted.


# test for it...


# R incorporates the Bartlett test to test the null hypothesis of equal group variances... 
# p<.05 means does not satisfy assumption of equal variances

bartlett.test(total ~ strain, data=wheels1)



# less sensitive version - levene test - but often preferred to bartlett

library(lawstat)    # some Mac users have reported difficulties in loading this package 

levene.test(wheels1$total, wheels1$strain) #default is to use medians i.e. a Brown-Forsythe test

levene.test(wheels1$total, wheels1$strain, location=c("mean"))




### If you have heteroscedasticity (unequal variances bt groups), still check the variances of groups. 

# Linear models are fairly robust to heterogeneity of variance so long as the maximum variance is no more than 4� greater than the minimum variance

split(wheels1$total,wheels1$strain)
mylist <- split(wheels1$total,wheels1$strain)
str(mylist) # a list

lapply(mylist, function(x){ var(x, na.rm=T) })

mylist.out <- lapply(mylist, function(x){ var(x, na.rm=T) })

unlist(mylist.out)

max(unlist(mylist.out))
min(unlist(mylist.out))

max(unlist(mylist.out)) / min(unlist(mylist.out))   #[1] 3.864324   Hooray !


# For more on heteroscedasticity, and One-Way ANOVA, this is excellent:
# http://stats.stackexchange.com/questions/91872/alternatives-to-one-way-anova-for-heteroskedastic-data/91881#91881



#### Non parametric tests comparing medians across groups...  i.e. a Kruskal-Wallis Test ####

kruskal.test(total~strain,data=wheels1) 


# there is no Tukey post-hoc test using Kruskal-Wallis
# instead, you can run multiple pairwise Mann-Whitney U tests 
# you need to account for the elevated risk of type I error though
# Holm is my preffered p.adjust.method, read a stats book for other options (e.g. Bonferonni)

pairwise.wilcox.test(wheels1$total,wheels1$strain,p.adjust.method="holm")  #notice the syntax is a bit different to kruskal.test














