#working analysis file by Arthur
#install.packages("reshape") #Use this to install it, do this only once
# update.packages() #May need to do this to ensure melt function is behaving properly
library(reshape2) #note if you have already run reshape in the same session, this messes up the melt function. You will need to restart R studio and run reshape2 only
data <- read.csv("Data_Collection_RealDeal - R_data.csv")
options("scipen"=100)

# melt data so that each rating is treated as individual data point (ie. individual row)
data.melted <- melt(data,
    id.vars=c("id","group","treatment","quiz_score","zip_code","county","age","gender","wine_type","wine_color","wine_pref"),
    variable.name = "wine",
    value.name = "rating")
#convert wine_type name to cheap, medium, and expensive by re-naming the factors
levels(data.melted$wine)[match("rating_cheap",levels(data.melted$wine))] <- "cheap"
levels(data.melted$wine)[match("rating_medium",levels(data.melted$wine))] <- "medium"
levels(data.melted$wine)[match("rating_expensive",levels(data.melted$wine))] <- "expensive"

# Steps that mimicks Carson's to ensure our regression results match properly
data.melted$gender[data.melted$gender == "F "] <- "F"        #fix error of gender typo
data.melted$wine_color[data.melted$wine_color == ""] <- NA   #fix empty input for wine_color
data.melted$wine <- relevel(as.factor(data.melted$wine), ref = "expensive") # make expensive wine ref level
data.melted <- subset(data.melted, data.melted$id != 3) #eliminate "B Rose" subject as rating is all 1's

summary(lm(rating ~ treatment * wine + gender + quiz_score + wine_color, data = data.melted))

### VERIFIED TO HAVE EXACT SAME OUTPUT AS CARSON's!!

#############
# From this point onwards, continue to clean data beyond Carson's regression work [last checked at 10pm on 4/15/2015]

#Further fix data
data.melted <- subset(data.melted, data.melted$id != 6) #eliminate "David Brookman" subject
data.melted$zip_code <- factor(data.melted$zip_code) #turn zipcode from numerical values into factors (v. important!!)    

data.melted$zip_code[data.melted$zip_code == "49445"] <- NA  
data.melted$wine_color[data.melted$wine_type == "error - multiple"] <- NA  

data.melted$gender[data.melted$gender == ""] <- NA   #fix empty input for gender
data.melted$age[data.melted$age == ""] <- NA  #fix empty input for age
data.melted$wine_type[data.melted$wine_type == ""] <- NA    #fix empty input for wine_type (wine_type would not be used beyond this)

#CHART1: Descriptive Statistics - Quiz Score and Age
# install.packages("xtable") #Use this to install it, do this only once
# helpful link: http://cran.r-project.org/web/packages/xtable/vignettes/xtableGallery.pdf
#use this link to generate image from latex output http://www.tlhiv.org/ltxpreview/
library(xtable)
# data.quizAge = data.frame(
#     "Mean (C)"=c(mean(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),
#                        mean(data.melted$age[data.melted$treatment==0],na.rm=TRUE)),
#     "Standard Dev. (C)"=c(sd(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),
#                           sd(data.melted$age[data.melted$treatment==0],na.rm=TRUE)),
#     "Min (C)"=c(min(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),
#                 min(data.melted$age[data.melted$treatment==0],na.rm=TRUE)),
#     "Max (C)"=c(max(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),
#                 max(data.melted$age[data.melted$treatment==0],na.rm=TRUE)),
#     "Mean (T)"=c(mean(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),
#                  mean(data.melted$age[data.melted$treatment==1],na.rm=TRUE)),
#     "Standard Dev. (T)"=c(sd(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),
#                                 sd(data.melted$age[data.melted$treatment==1],na.rm=TRUE)),
#     "Min (T)"=c(min(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),
#                 min(data.melted$age[data.melted$treatment==1],na.rm=TRUE)),
#     "Max (T)"=c(max(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),
#                 max(data.melted$age[data.melted$treatment==1],na.rm=TRUE))
# )

data.quizAge = data.frame(
    "Mean (C)"=c("Mean",round(mean(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),2),
                 round(mean(data.melted$age[data.melted$treatment==0],na.rm=TRUE),2)),
    "Standard Dev. (C)"=c("Standard Dev.",round(sd(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),2),
                          round(sd(data.melted$age[data.melted$treatment==0],na.rm=TRUE),2)),
    "Min (C)"=c("Min",min(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),
                min(data.melted$age[data.melted$treatment==0],na.rm=TRUE)),
    "Max (C)"=c("Max",max(data.melted$quiz_score[data.melted$treatment==0],na.rm=TRUE),
                max(data.melted$age[data.melted$treatment==0],na.rm=TRUE)),
    "Mean (T)"=c("Mean",round(mean(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),2),
                 round(mean(data.melted$age[data.melted$treatment==1],na.rm=TRUE),2)),
    "Standard Dev. (T)"=c("Standard Dev.",round(sd(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),2),
                          round(sd(data.melted$age[data.melted$treatment==1],na.rm=TRUE),2)),
    "Min (T)"=c("Min",min(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),
                min(data.melted$age[data.melted$treatment==1],na.rm=TRUE)),
    "Max (T)"=c("Max",max(data.melted$quiz_score[data.melted$treatment==1],na.rm=TRUE),
                max(data.melted$age[data.melted$treatment==1],na.rm=TRUE))
)


rownames(data.quizAge) <- c("","Quiz Score","Age")
colnames(data.quizAge) <- c("","CONTROL",
                            paste("(N=",nrow(data.melted[data.melted$treatment==0,]),")"),
                            "","","TREATMENT",
                            paste("(N=",nrow(data.melted[data.melted$treatment==1,]),")"),"")
table.quizAge <- xtable(data.quizAge)
align(table.quizAge) <- "|r|cccc|cccc|"
print(table.quizAge,hline.after=c(-1,1,3),floating=FALSE)

#CHART2: Descriptive Statistics - Gender and Wine Preferences
gender_c <- data.melted$gender[data.melted$treatment==0]
gender_t <- data.melted$gender[data.melted$treatment==1]
winePref_c <- data.melted$wine_pref[data.melted$treatment==0]
winePref_t <- data.melted$wine_pref[data.melted$treatment==1]
wineColor_c <- data.melted$wine_color[data.melted$treatment==0]
wineColor_t <- data.melted$wine_color[data.melted$treatment==1]

num_male_c <-length(gender_c[gender_c=="M" & !is.na(gender_c)])
num_female_c <-length(gender_c[gender_c=="F" & !is.na(gender_c)])
num_missing_c <- length(gender_c[is.na(gender_c)])
num_prefCheap_c <-length(winePref_c[winePref_c=="cheap" & !is.na(winePref_c)])
num_prefMed_c <-length(winePref_c[winePref_c=="medium" & !is.na(winePref_c)])
num_prefExp_c <-length(winePref_c[winePref_c=="expensive" & !is.na(winePref_c)])
num_prefMissing_c <-length(winePref_c[is.na(winePref_treat)])
num_colorWhite_c <- length(wineColor_c[wineColor_c=="white" & !is.na(wineColor_c)])
num_colorRed_c <- length(wineColor_c[wineColor_c=="red" & !is.na(wineColor_c)])
num_colorMissing_c <- length(wineColor_c[is.na(wineColor_c)])
num_total_c <- nrow(data.melted[data.melted$treatment==0,])

num_male_t <-length(gender_t[gender_t=="M" & !is.na(gender_t)])
num_female_t <-length(gender_t[gender_t=="F" & !is.na(gender_t)])
num_missing_t <- length(gender_t[is.na(gender_t)])
num_prefCheap_t <-length(winePref_t[winePref_t=="cheap" & !is.na(winePref_t)])
num_prefMed_t <-length(winePref_t[winePref_t=="medium" & !is.na(winePref_t)])
num_prefExp_t <-length(winePref_t[winePref_t=="expensive" & !is.na(winePref_t)])
num_prefMissing_t <-length(winePref_t[is.na(winePref_treat)])
num_colorWhite_t <- length(wineColor_t[wineColor_t=="white" & !is.na(wineColor_t)])
num_colorRed_t <- length(wineColor_t[wineColor_t=="red" & !is.na(wineColor_t)])
num_colorMissing_t <- length(wineColor_t[is.na(wineColor_t)])
num_total_t <-nrow(data.melted[data.melted$treatment==1,])

percent_male_c <- num_male_c / length(gender_c) * 100
percent_female_c <- num_female_c / length(gender_c) * 100
percent_missing_c <- num_missing_c / length(gender_c) * 100
percent_prefCheap_c <- num_prefCheap_c / length(winePref_c) * 100
percent_prefMed_c <- num_prefMed_c / length(winePref_c) * 100
percent_prefExp_c <- num_prefExp_c / length(winePref_c) * 100
percent_prefMissing_c <- num_prefMissing_c / length(winePref_c) * 100
percent_colorWhite_c <- num_colorWhite_c / length(wineColor_c) * 100
percent_colorRed_c <- num_colorRed_c / length(wineColor_c) * 100
percent_colorMissing_c <- num_colorMissing_c / length(wineColor_c) * 100
percent_total_c <- 100

percent_male_t <- num_male_t / length(gender_t) * 100
percent_female_t <- num_female_t / length(gender_t) * 100
percent_missing_t <- num_missing_t / length(gender_t) * 100
percent_prefCheap_t <- num_prefCheap_t / length(winePref_t) * 100
percent_prefMed_t <- num_prefMed_t / length(winePref_t) * 100
percent_prefExp_t <- num_prefExp_t / length(winePref_t) * 100
percent_prefMissing_t <- num_prefMissing_t / length(winePref_t) * 100
percent_colorWhite_t <- num_colorWhite_t / length(wineColor_t) * 100
percent_colorRed_t <- num_colorRed_t / length(wineColor_t) * 100
percent_colorMissing_t <- num_colorMissing_t / length(wineColor_t) * 100
percent_total_t <- 100

data.genderPref <- data.frame(
    "% (Control)" <- c(percent_male_c,
                       percent_female_c,
                       percent_missing_c,
                       percent_prefCheap_c,
                       percent_prefMed_c,
                       percent_prefExp_c,
                       percent_prefMissing_c,
                       percent_colorWhite_c,
                       percent_colorRed_c,
                       percent_colorMissing_c,
                       percent_total_c),
    "N (Control)" <- c(num_male_c,
                       num_female_c,
                       num_missing_c,
                       num_prefCheap_c,
                       num_prefMed_c,
                       num_prefExp_c,
                       num_prefMissing_c,
                       num_colorWhite_c,
                       num_colorRed_c,
                       num_colorMissing_c,
                       num_total_c),
    "% (Treatment)" <- c(percent_male_t,
                         percent_female_t,
                         percent_missing_t,
                         percent_prefCheap_t,
                         percent_prefMed_t,
                         percent_prefExp_t,
                         percent_prefMissing_t,
                         percent_colorWhite_t,
                         percent_colorRed_t,
                         percent_colorMissing_t,
                         percent_total_t),
    "N (Treatment)" <- c(num_male_t,
                         num_female_t,
                         num_missing_t,
                         num_prefCheap_t,
                         num_prefMed_t,
                         num_prefExp_t,
                         num_prefMissing_t,
                         num_colorWhite_t,
                         num_colorRed_t,
                         num_colorMissing_t,
                         num_total_t)
)

rownames(data.genderPref) <- c("Male","Female","Gender Missing",
                               "Prefer Cheap Wine","Prefer Medium Wine","Prefer Expensive Wine","Missing Cheap/Medium/Expensive Pref",
                               "Prefer White Wine","Prefer Red Wine","Missing White/Red Wine Pref","Total")
colnames(data.genderPref) <- c("% (Control)","N (Control)","% (Treatment)","N (Treatment)")
table.genderPref <- xtable(data.genderPref)
align(table.genderPref) <- "|r|cc|cc|"
print(table.genderPref,hline.after=c(-1,0,3,7,10,11),floating=FALSE)

# CHART 3: Histogram
# install.packages("ggplot2")  #only need to do this once
# http://felixfan.github.io/rstudy/2014/02/28/ggplot2-cheatsheet/
library(ggplot2)
data.rating <- data.frame(
    "Wine_Type" <- c("Cheap","Medium","Expensive","Cheap","Medium","Expensive"),
    "Group" <- c("Control","Control","Control","Treatment","Treatment","Treatment"),
    "Rating" <- c(mean(data.melted$rating[data.melted$wine=="cheap" & data.melted$treatment==0]),
                  mean(data.melted$rating[data.melted$wine=="medium" & data.melted$treatment==0]),
                  mean(data.melted$rating[data.melted$wine=="expensive" & data.melted$treatment==0]), 
                  mean(data.melted$rating[data.melted$wine=="cheap" & data.melted$treatment==1]),
                  mean(data.melted$rating[data.melted$wine=="medium" & data.melted$treatment==1]),
                  mean(data.melted$rating[data.melted$wine=="expensive" & data.melted$treatment==1])),
    "sd" <- c(sd(data.melted$rating[data.melted$wine=="cheap" & data.melted$treatment==0]),
              sd(data.melted$rating[data.melted$wine=="medium" & data.melted$treatment==0]),
              sd(data.melted$rating[data.melted$wine=="expensive" & data.melted$treatment==0]), 
              sd(data.melted$rating[data.melted$wine=="cheap" & data.melted$treatment==1]),
              sd(data.melted$rating[data.melted$wine=="medium" & data.melted$treatment==1]),
              sd(data.melted$rating[data.melted$wine=="expensive" & data.melted$treatment==1]))
)
colnames(data.rating) <- c("Wine_Type","Group","Rating","sd")
# with(data.rating, table(Rating,Wine_Type))
ggplot(data.rating, aes(y = Rating, x=factor(Wine_Type, levels = c("Cheap","Medium","Expensive")),fill=factor(Group))) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Rating - sd, ymax = Rating + sd), width = 0.3, position=position_dodge(.9), color = "darkblue") +
    xlab("Wine Type") +
    ylab("Rating (Likert Scale from 1 to 5)") +
    ggtitle("Rating of Each Wine Type (Control vs Treatment Groups)")


# Check relative importance to determine covariates
# install.packages("relaimpo") #Use this to install it, do this only once
library(relaimpo)
lm.rel_importance <- lm(rating ~ quiz_score+zip_code+age+gender+wine_color+wine_pref, data = data.melted)
calc.relimp(lm.rel_importance, type = c("lmg"), rela = TRUE)   # http://www.r-bloggers.com/the-relative-importance-of-predictors-let-the-games-begin/

# lmg
# zip_code   0.753183656
# wine_pref  0.116077980
# quiz_score 0.004465814
# age        0.035647668
# gender     0.046319525
# wine_color 0.044305356

# Drop anything below 0.20. Therefore decide to drop age, wine_color, quiz score

# install.packages("stargazer") #Use this to install it, do this only once
# helpful link: http://jakeruss.com/cheatsheets/stargazer.html#control-the-number-of-decimal-places
library(stargazer)    
lm.no_cov = lm(rating ~ treatment * wine, data = data.melted)
lm.final = lm(rating ~ treatment * wine + gender , data = data.melted) #not adding zip_code yet
lm.full = lm(rating ~ treatment * wine + gender + age + wine_color + quiz_score, data = data.melted) #not adding zip_code yet
stargazer(lm.no_cov, lm.final, lm.full, type='html',
    order = c(1,2,3,9,10,4,5,6,7),
    digits=3,
    dep.var.labels="Wine Rating (Likert Scale from 1 to 5)",
    covariate.labels=c("Treatment of Showing Price","Cheap Wine [i]","Medium Wine [i]",
                       "Interaction of Treatment and Cheap Wine","Interaction of Treatment and expensive Wine",
                       "Gender","Zip Code","Age","Prefer White Wine","Wine Quiz Score"),
    add.lines = list(c("Chosen Model", "No", "Yes","No"),c("Fixed effects", "No", "No","No")),
    notes=c("(1) Regression without covariates","(2) Regression with selected set of covariates (final model)","(3) Regression with full set of covariates","[i] Expensive Wine being the reference wine"),
    notes.align="l",
    out='chart_reg_nocluster.htm')

list(c("Chosen Model", "No", "Yes","No"),
     c("Fixed effects", "No", "No","No"))

summary(lm.no_cov)
summary(lm.final)
summary(lm.full)

data.melted$wine_pref

# TBD - CALCULATE CLUSTER STANDARD ERRORS
# #use 2 different clustering functions: one is for normal case, one is for having NA in the data due to covariates
# cl <- function(fm, cluster){ 
#     require(sandwich, quietly = TRUE)
#     require(lmtest, quietly = TRUE)
#     M <- length(unique(cluster))
#     N <- length(cluster)
#     K <- fm$rank
#     dfc <- (M/(M-1))*((N-1)/(N-K))
#     uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
#     vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
#     coeftest(fm, vcovCL)
# }
# cl_removeNA   <- function(fm, cluster){
#     require(sandwich, quietly = TRUE)
#     require(lmtest, quietly = TRUE)
#     M <- length(unique(cluster))
#     N <- length(cluster)
#     K <- fm$rank
#     dfc <- (M/(M-1))*((N-1)/(N-K))
#     uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster[-fm$na.action], sum));
#     vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
#     result<-coeftest(fm, vcovCL)
#     result}
# 
# #factor the cluster variable and apply the function
# data.study1$cluster<-factor(as.character(data.study1$cluster))
# cl(lm.noCluster,data.study1$cluster)
# 
# cl(lm.no_cov,data.melted$group)
# cl_removeNA(lm.final,data.melted$group)
# cl_removeNA(lm.full,data.melted$group)
# 
# library(sandwich)
# cov1        <- vcovHC(output, type = "HC1")
# robust.se   <- sqrt(diag(cov1))
# 
# stargazer(output, output, type = "html", 
#           se = list(robust.se))

# # Mean of rating per wine type or treatment subgroups
# aggregate(rating ~ wine, FUN=mean, data = data.melted)
# aggregate(rating ~ treatment, FUN=mean, data = data.melted)



