
head(videogames)
colnames(videogames)

videogames <- videogames[complete.cases(videogames),]

table(videogames$Platform)
table(videogames$Rating)

plats <- c("Wii", "X360", "PC", "PS2", "PS3")
gens <- c("Action", "Racing", "Shooter", "Sports")
rats <- c("E", "E10+", "M", "T")
vg <- videogames %>% filter(Platform %in% plats) %>% filter(Genre %in% gens) %>% filter(Rating %in% rats)
head(vg)

vg <- vg %>% select(1,2,3,4,5,6,7,8,10,11,13,15)
colnames(vg)<-c("name","platform","year","genre","publisher","NA_sales","EU_sales","JP_sales","global_sales","critic","user","rating")
head(vg)

write.csv(vg, "data/videogames.csv", row.names = F)
