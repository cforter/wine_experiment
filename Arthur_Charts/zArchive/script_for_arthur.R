final.data <- read.csv("Data_Collection_RealDeal - Input.csv", stringsAsFactors = F)
library(reshape)
wines <- final.data[c(1:39),c(2,11:13,26:28)]
melted <- melt(wines)

melted$wine[melted$variable == "rating_wine1"] <- melted$wine1[melted$variable == "rating_wine1"]
melted$wine[melted$variable == "rating_wine2"] <- melted$wine2[melted$variable == "rating_wine2"]
melted$wine[melted$variable == "rating_wine3"] <- melted$wine3[melted$variable == "rating_wine3"]
View(melted)

melted.merged <- merge(final.data, melted, by.x = "name", by.y = "name" )
View(melted.merged)

#Fix Errors
melted.merged$gender[melted.merged$gender == "F "] <- "F"

melted.merged$wine_color[melted.merged$wine_color == ""] <- NA

# Make expensive wine reference level
melted.merged$wine <- relevel(as.factor(melted.merged$wine), ref = "expensive")

no.rose <- subset(melted.merged, melted.merged$name != "B Rose")

# Regression
wine.lm <- lm(value ~ treatment + wine + (treatment * wine)
              + (gender) + quiz_score + wine_color, data = no.rose)
summary(wine.lm)

