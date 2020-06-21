
plothist <- function(mean = 0, sd = 1){
library(tidyverse)

x <-  rnorm(1000000, mean = mean, sd = sd)
  
df <- data.frame(x)
colnames(df)<-"x"
ggplot(df, aes(x = x))  + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "cornflowerblue", alpha=.4) + 
  geom_density(alpha = 0.2, fill = "navy") + 
  theme_classic()

}