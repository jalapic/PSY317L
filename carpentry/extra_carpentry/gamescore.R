
plot(x,y)
shapiro.test(x)
shapiro.test(y)

df <- data.frame(x,y,z)

x <- rnorm(50, 10, 2.5)
y <- rnorm(50, 103, 12.5)

cor.test(x, y, alternative = "less")
cor.test(x, y, alternative = "greater")
cor.test(x, y, alternative = "two.sided")

plot(x,z)
z <- rnorm(50, 5.9, 1.1)
cor.test(x, z, alternative = "less")
cor.test(x, z, alternative = "greater")
cor.test(x, z, alternative = "two.sided")


library(randomNames)
df$names <- randomNames(50)

df
df[,1:3]<-round(df[,1:3],1)

df<-df[,c(4,1,2,3)]

plot(df[c(2:4)])

colnames(df) <- c('name', 'hours', 'score', 'tiredness')
head(df)
write.csv(df, "data/gamescore.csv", row.names = F)
