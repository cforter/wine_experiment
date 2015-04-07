#working analysis file by Arthur
#install.packages("reshape")
library(reshape2)
data <- read.csv("Data_Collection_Test - R_data.csv")
# data <- read.csv("Data_Collection_Pilot - R_data.csv")
# options("scipen"=100)

# melt data so that each rating is treated as individual data point (ie. individual row)
data.melted <- melt(data,id.vars=c("id","group","treatment","quiz_score","zip_code","age","gender"),
    variable.name = "wine",
    value.name = "rating"                               
)

#convert wine_type name to cheap, medium, and expensive by re-naming the factors
levels(data.melted$wine)[match("rating_cheap",levels(data.melted$wine))] <- "cheap"
levels(data.melted$wine)[match("rating_medium",levels(data.melted$wine))] <- "medium"
levels(data.melted$wine)[match("rating_expensive",levels(data.melted$wine))] <- "expensive"

# Standard Regression 
data.melted.lm <- lm(rating ~ treatment + wine + (treatment * wine), data = data.melted)
summary(data.melted.lm)

# Adding HTEs
data.melted.lm.hte <- lm(rating ~ treatment + wine + (treatment * wine) + group + quiz_score + zip_code + age + gender, data = data.melted)
summary(data.melted.lm.hte)

# Mean of rating per wine type or treatment subgroups
aggregate(rating ~ wine, FUN=mean, data = data.melted)
aggregate(rating ~ treatment, FUN=mean, data = data.melted)

# regression plot



