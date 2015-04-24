# data = final.data
library(reshape)

options("scipen"=100)

View(final.data)

wines <- final.data[c(1:39),c(2,11:13,26:28)]
melted <- melt(wines)
View(melted)
melted$wine[melted$variable == "rating_wine1"] <- melted$wine1[melted$variable == "rating_wine1"]
melted$wine[melted$variable == "rating_wine2"] <- melted$wine2[melted$variable == "rating_wine2"]
melted$wine[melted$variable == "rating_wine3"] <- melted$wine3[melted$variable == "rating_wine3"]
View(melted)
melted.merged <- merge(final.data, melted, by.x = "name", by.y = "name" )
View(melted.merged)

#Fix Errors
melted.merged$gender[melted.merged$gender == "F "] <- "F"

melted.merged$wine_color[melted.merged$wine_color == ""] <- NA

melted.merged$score_range[melted.merged$quiz_score > 6] <- "Hi"
melted.merged$score_range[melted.merged$quiz_score %in% c(4,5,6)] <- "Med"
melted.merged$score_range[melted.merged$quiz_score < 4] <- "Lo"

# Quick analysis
wine.lm <- lm(value ~ treatment + wine + (treatment * wine), data = melted.merged)
summary(wine.lm)

<<<<<<< HEAD
# Make cheap wine reference level
melted.merged$wine <- relevel(as.factor(melted.merged$wine), ref = "expensive")

#  Exclude all 1's response
no.rose <- subset(melted.merged, melted.merged$name != "B Rose")
View(no.rose)



# Percentage of males in control
not.treat <- subset(melted.merged, melted.merged$treatment == 0)
length(not.treat$gender[not.treat$gender == "M"])/length(not.treat$gender)

# Percentage of males in treatment
only.treat <- subset(melted.merged, melted.merged$treatment == 1)
length(only.treat$gender[only.treat$gender == "M"])/length(only.treat$gender)


aggregate(value ~ wine, FUN=mean, data = subset(melted.merged, melted.merged$treatment == 1))
aggregate(value ~ wine, FUN=mean, data = subset(melted.merged, melted.merged$treatment == 0))
aggregate(value ~ treatment, FUN=mean, data = melted.merged)

# More complex
wine.lm <- lm(value ~ treatment + wine + (treatment * wine)
                 + (gender) + quiz_score + wine_color, data = no.rose)
summary(wine.lm)

wine.lm <- lm(value ~ wine, data = melted.merged)
summary(wine.lm)

gender.lm <- lm(treatment ~ gender, data = melted.merged)
summary(gender.lm)

# confidence interval on HTE

ci <- .4921 * 1.96
cheap.interval <- c((.5888 - ci), (.5888 + ci))
cheap.interval
medium.interval <- c((.4777 - ci), (.4777 + ci))
medium.interval

#Cluster standard error function
cl <- function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

#Cluster vector and clustered SEs
test <- no.rose[,c(1,47,25,48,3,32,9)]
cluster.df <- na.omit(test)
cluster.vector <- cluster.df$name
cl(wine.lm, cluster.vector)

# Randomization inference

# Regular
treatment.prob <- c(rep(0, 54), rep(1, 60))

randomize <- function() {
  sample(treatment.prob, 114, replace = F)
}

ate.calc <- function(outcomes, treatment) {
  ate <- mean(outcomes[treatment == 1]) - mean(outcomes[treatment == 0])
  ate
}

dist.under.sharp <- replicate(10000, ate.calc(no.rose$value, randomize()))
plot(density(dist.under.sharp))
abline(v = )
sd(dist.under.sharp)

#Clustered
treatment.prob <- c(rep(0, 18), rep(1, 20))
person.ids <- unique(no.rose$id)

randomize <- function() {
  treatment <- sample(treatment.prob, 38, replace = F)
  treat.person.ids <- person.ids[treatment == 1]
  treat.person.ids
}

ate.calc <- function(outcomes, treatment.ids) {
  ate <- mean(outcomes[no.rose$id %in% treatment.ids]) - mean(outcomes[!(no.rose$id %in% treatment.ids)])
  ate
}

dist.under.sharp <- replicate(10000, ate.calc(no.rose$value, randomize()))
plot(density(dist.under.sharp))
abline(v = )
sd(dist.under.sharp)
=======
aggregate(value ~ wine, FUN=mean, data = melted.merged)
aggregate(value ~ treatment, FUN=mean, data = melted.merged)

>>>>>>> origin/master
