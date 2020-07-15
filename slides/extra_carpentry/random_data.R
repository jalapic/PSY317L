
# example data
N <- 20
df <- data.frame(id = 1:N,age = rnorm(N,18:90),bmi = rnorm(N,15:40),
                   chol = rnorm(N,50:350), insulin = rnorm(N,2:40),sbp = rnorm(N, 50:200),
                   dbp = rnorm(N, 30:150), 
                   smoke = rep(c(1, 2), N/2), educ = sample(LETTERS[1:4]))

df

library(tidyverse)

# specify which variables should have missing data and prc of missing data
c_names = colnames(df)[3:7]
prc_missing = 0.09
n_remove = prc_missing * nrow(df)

df %>%
  gather(var, value, -id) %>%   # reshape data
  sample_frac(1) %>%            # shuffle rows
  group_by(var) %>%             # for each variables
  mutate(value = ifelse(var %in% c_names & row_number() <= n_remove, NA, value)) %>%  # update to NA top x number of rows if it's one of the variables you specified
  spread(var, value)   ->df1

df1=df1 %>% select(id,age,bmi,chol,insulin,dbp,sbp,smoke,educ)
write.csv(df1, "data/bmi.csv",row.names = F)
