# data = pilot
library(reshape)

options("scipen"=100)

wines <- pilot[,c(1,7:9,23:25)]
melted <- melt(wines)
melted$wine[melted$variable == "rating_wine1"] <- melted$wine1[melted$variable == "rating_wine1"]
melted$wine[melted$variable == "rating_wine2"] <- melted$wine2[melted$variable == "rating_wine2"]
melted$wine[melted$variable == "rating_wine3"] <- melted$wine3[melted$variable == "rating_wine3"]
melted.merged <- merge(melted, pilot)
View(melted.merged)


# Quick analysis
wine.lm <- lm(value ~ treatment + wine + (treatment * wine), data = melted.merged)
summary(wine.lm)

aggregate(value ~ wine, FUN=mean, data = melted.merged)
aggregate(value ~ treatment, FUN=mean, data = melted.merged)