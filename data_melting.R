# data = pilot
library(reshape)

wines <- pilot[,c(1,7:9,23:25)]
melted <- melt(wines)
melted$wine[melted$variable == "rating_wine1"] <- melted$wine1[melted$variable == "rating_wine1"]
melted$wine[melted$variable == "rating_wine2"] <- melted$wine2[melted$variable == "rating_wine2"]
melted$wine[melted$variable == "rating_wine3"] <- melted$wine3[melted$variable == "rating_wine3"]
melted.merged <- merge(melted, pilot)
View(melted.merged)