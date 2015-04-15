#working analysis file by Arthur
#install.packages("reshape") #Use this to install it, do this only once
library(reshape2)
data <- read.csv("Data_Collection_RealDeal - R_data.csv")
# options("scipen"=100)

# melt data so that each rating is treated as individual data point (ie. individual row)
data.melted <- melt(data,
    id.vars=c("id","group","treatment","quiz_score","zip_code","age","gender","wine_pref","wine_type","wine_color"),
    variable.name = "wine",
    value.name = "rating")
#convert wine_type name to cheap, medium, and expensive by re-naming the factors
levels(data.melted$wine)[match("rating_cheap",levels(data.melted$wine))] <- "cheap"
levels(data.melted$wine)[match("rating_medium",levels(data.melted$wine))] <- "medium"
levels(data.melted$wine)[match("rating_expensive",levels(data.melted$wine))] <- "expensive"

# Steps that mimicks Carson's to ensure our regression results match properly
data.melted$gender[data.melted$gender == "F "] <- "F"        #fix error of gender typo
data.melted$wine_color[data.melted$gender == ""] <- NA   #fix empty input for gender
data.melted$wine_color[data.melted$wine_color == ""] <- NA   #fix empty input for wine_color
data.melted$wine <- relevel(as.factor(data.melted$wine), ref = "expensive") # make expensive wine ref level
data.melted <- subset(data.melted, data.melted$id != 3) #eliminate "B Rose" subject as rating is all 1's

lm.wine <- lm(rating ~ treatment * wine + gender + quiz_score + wine_color, data = data.melted)
summary(lm.wine)

### at this point, realize data do not match


# From this point onwards, continue to clean data beyond Carson's regression check

#fix data
data.melted <- subset(data.melted, data.melted$id != 3) #eliminate "David Brookman" subject
data.melted$wine_color[data.melted$wine_type == "error - multiple"] <- NA   

data.melted$wine_color[data.melted$age == ""] <- NA
data.melted$wine_color[data.melted$wine_type == ""] <- NA   











# data.melted.lm <- lm(rating ~ treatment + wine + (treatment * wine), data = data.melted)

# summary(data.melted.lm)

# Adding HTEs
data.melted.lm.hte <- lm(rating ~ treatment + wine + (treatment * wine) + group + quiz_score + zip_code + age + gender, data = data.melted)
summary(data.melted.lm.hte)

# Mean of rating per wine type or treatment subgroups
aggregate(rating ~ wine, FUN=mean, data = data.melted)
aggregate(rating ~ treatment, FUN=mean, data = data.melted)

# regression plot
stargazer(mydata, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")





#install.packages("stargazer") #Use this to install it, do this only once
library(stargazer)

