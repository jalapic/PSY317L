
cheese <- food %>% filter(Category=="CHEESE")
head(food)

cheese <- food %>% filter(Category=="CHEESE")

cheese <-cheese %>% select(Description,`Data.Fat.Saturated Fat`,`Data.Fat.Polysaturated Fat`,`Data.Fat.Monosaturated Fat`,
Data.Protein,Data.Carbohydrate,Data.Cholesterol,Data.Fiber,Data.Kilocalories)


head(cheese)

 cheese$Description <- gsub("CHEESE,", "", cheese$Description)
 cheese$Description <- tolower(cheese$Description)


colnames(cheese)<-c("type","sat_fat", "polysat_fat", "monosat_fat", "protein", "carb", "chol", "fiber", "kcal")
head(cheese)
tail(cheese)

write.csv(cheese,"data/cheese.csv", row.names=F)
