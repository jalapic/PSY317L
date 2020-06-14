colnames(austin_cat)
cats <- austin_cat %>% select(animal_id, monthyear, name, color1,color2, sex, breed)
head(cats)
cats <- cats[!is.na(cats$name),]
cats$name <- gsub("\\*","", cats$name)

cats <- cats[cats$color1>30,]
colors <- table(cats$color1)[table(cats$color1)>30]
cats <- cats[cats$color1%in%names(colors),]

cats
table(cats$breed)

cats <- cats %>% filter(color1 != "Breed Specific") %>% filter(color1!="lynx")

ggplot(cats, aes(x=color1)) + geom_bar() + coord_flip()
write.csv(cats, "data/catcolor.csv",row.names = F)


###:

df=read_csv("https://raw.githubusercontent.com/allisonhorst/penguins/master/data/penguins_size.csv")
head(df)
write.csv(df, "data/penguins.csv", row.names = F)
