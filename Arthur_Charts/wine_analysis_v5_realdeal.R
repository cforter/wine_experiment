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
data.melted <- subset(data.melted, data.melted$id != 7) #eliminate "Anne Sandback" subject

data.melted$zip_code <- factor(data.melted$zip_code) #turn zipcode from numerical values into factors
data.melted$zip_code[data.melted$zip_code == "49445"] <- NA  #zip code is in east coast
data.melted$county[data.melted$county == "Muskegon"] <- NA  #county is in east coast
data.melted$wine_color[data.melted$wine_type == "error - multiple"] <- NA  
data.melted$gender[data.melted$gender == ""] <- NA   #fix empty input for gender
data.melted$age[data.melted$age == ""] <- NA  #fix empty input for age
data.melted$wine_type[data.melted$wine_type == ""] <- NA    #fix empty input for wine_type (wine_type would not be used beyond this)
data.melted$county <- relevel(as.factor(data.melted$county), ref = "Sonoma") # make Sonoma county ref level

#CHART1: Descriptive Statistics - Test Summaries
# install.packages("xtable") #Use this to install it, do this only once
# helpful link: http://cran.r-project.org/web/packages/xtable/vignettes/xtableGallery.pdf
# use this link to generate image from latex output http://www.tlhiv.org/ltxpreview/
library(xtable)

data.test <- data.frame(
    "Pilot Trial" <- c("7 Subjects","3 Subjects","4 Subjects","21 Data Points", "21 Data Points", "0 Data Points"), 
    "Actual Experiment" <- c("39 Subjects", 
                             paste(as.character(nrow(data[data$treatment==0,]))," Subjects"),
                             paste(as.character(nrow(data[data$treatment==1,]))," Subjects"),
                             "117 Data Points", "9 Data Points", "108 Data Points")
)
colnames(data.test) <- c("Pilot Trial","Actual Experiment") 
rownames(data.test) <- c("Total Participants","Control Group","Treatment Group","Data Collected","Data Discarded","Available Data")
table.test <- xtable(data.test)
align(table.test) <- "|r|c|c|"
print(table.test,hline.after=c(-1,0,3,6),floating=FALSE)

#CHART2: Descriptive Statistics - Quiz Score and Age
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
                            paste("(Subjects=",nrow(data.melted[data.melted$treatment==0,])/3,")"),
                            "","","TREATMENT",
                            paste("(Subjects=",nrow(data.melted[data.melted$treatment==1,])/3,")"),"")
table.quizAge <- xtable(data.quizAge)
align(table.quizAge) <- "|r|cccc|cccc|"
print(table.quizAge,hline.after=c(-1,1,3),floating=FALSE)

#CHART3: Descriptive Statistics - Gender and Wine Preferences
gender_c <- data.melted$gender[data.melted$treatment==0]
gender_t <- data.melted$gender[data.melted$treatment==1]
winePref_c <- data.melted$wine_pref[data.melted$treatment==0]
winePref_t <- data.melted$wine_pref[data.melted$treatment==1]
wineColor_c <- data.melted$wine_color[data.melted$treatment==0]
wineColor_t <- data.melted$wine_color[data.melted$treatment==1]
county_c <- data.melted$county[data.melted$treatment==0]
county_t <- data.melted$county[data.melted$treatment==1]

num_male_c <-length(gender_c[gender_c=="M" & !is.na(gender_c)])
num_female_c <-length(gender_c[gender_c=="F" & !is.na(gender_c)])
num_missing_c <- length(gender_c[is.na(gender_c)])
num_prefCheap_c <-length(winePref_c[winePref_c=="cheap" & !is.na(winePref_c)])
num_prefMed_c <-length(winePref_c[winePref_c=="medium" & !is.na(winePref_c)])
num_prefExp_c <-length(winePref_c[winePref_c=="expensive" & !is.na(winePref_c)])
num_prefMissing_c <-length(winePref_c[is.na(winePref_t)])
num_colorWhite_c <- length(wineColor_c[wineColor_c=="white" & !is.na(wineColor_c)])
num_colorRed_c <- length(wineColor_c[wineColor_c=="red" & !is.na(wineColor_c)])
num_colorMissing_c <- length(wineColor_c[is.na(wineColor_c)])
num_countyACC_c <- length(county_c[county_c=="Alameda & Contra Costa" & !is.na(county_c)])
num_countyM_c <- length(county_c[county_c=="Marin" & !is.na(county_c)])
num_countySF_c <- length(county_c[county_c=="San Francisco" & !is.na(county_c)])
num_countySM_c <- length(county_c[county_c=="San Mateo" & !is.na(county_c)])
num_countyS_c <- length(county_c[county_c=="Sonoma" & !is.na(county_c)])
num_countyMissing_c <- length(county_c[is.na(county_c)])
num_total_c <- nrow(data.melted[data.melted$treatment==0,])

num_male_t <-length(gender_t[gender_t=="M" & !is.na(gender_t)])
num_female_t <-length(gender_t[gender_t=="F" & !is.na(gender_t)])
num_missing_t <- length(gender_t[is.na(gender_t)])
num_prefCheap_t <-length(winePref_t[winePref_t=="cheap" & !is.na(winePref_t)])
num_prefMed_t <-length(winePref_t[winePref_t=="medium" & !is.na(winePref_t)])
num_prefExp_t <-length(winePref_t[winePref_t=="expensive" & !is.na(winePref_t)])
num_prefMissing_t <-length(winePref_t[is.na(winePref_t)])
num_colorWhite_t <- length(wineColor_t[wineColor_t=="white" & !is.na(wineColor_t)])
num_colorRed_t <- length(wineColor_t[wineColor_t=="red" & !is.na(wineColor_t)])
num_colorMissing_t <- length(wineColor_t[is.na(wineColor_t)])
num_countyACC_t <- length(county_t[county_t=="Alameda & Contra Costa" & !is.na(county_t)])
num_countyM_t <- length(county_t[county_t=="Marin" & !is.na(county_t)])
num_countySF_t <- length(county_t[county_t=="San Francisco" & !is.na(county_t)])
num_countySM_t <- length(county_t[county_t=="San Mateo" & !is.na(county_t)])
num_countyS_t <- length(county_t[county_t=="Sonoma" & !is.na(county_t)])
num_countyMissing_t <- length(county_t[is.na(county_t)])
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
percent_countyACC_c <- num_countyACC_c / length(county_c) * 100
percent_countyM_c <- num_countyM_c / length(county_c) * 100
percent_countySF_c <- num_countySF_c / length(county_c) * 100
percent_countySM_c <- num_countySM_c / length(county_c) * 100
percent_countyS_c <- num_countyS_c / length(county_c) * 100
percent_countyMissing_c <- num_countyMissing_c / length(county_c) * 100
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
percent_countyACC_t <- num_countyACC_t / length(county_t) * 100
percent_countyM_t <- num_countyM_t / length(county_t) * 100
percent_countySF_t <- num_countySF_t / length(county_t) * 100
percent_countySM_t <- num_countySM_t / length(county_t) * 100
percent_countyS_t <- num_countyS_t / length(county_t) * 100
percent_countyMissing_t <- num_countyMissing_t / length(county_t) * 100
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
                       percent_countyACC_c,
                       percent_countyM_c,
                       percent_countySF_c,
                       percent_countySM_c,
                       percent_countyS_c,
                       percent_countyMissing_c,
                       percent_total_c),
    "N (Control)" <- c(num_male_c/3,
                       num_female_c/3,
                       num_missing_c/3,
                       num_prefCheap_c/3,
                       num_prefMed_c/3,
                       num_prefExp_c/3,
                       num_prefMissing_c/3,
                       num_colorWhite_c/3,
                       num_colorRed_c/3,
                       num_colorMissing_c/3,
                       num_countyACC_c/3,
                       num_countyM_c/3,
                       num_countySF_c/3,
                       num_countySM_c/3,
                       num_countyS_c/3,
                       num_countyMissing_c/3,
                       num_total_c/3),
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
                         percent_countyACC_t,
                         percent_countyM_t,
                         percent_countySF_t,
                         percent_countySM_t,
                         percent_countyS_t,
                         percent_countyMissing_t,
                         percent_total_t),
    "N (Treatment)" <- c(num_male_t/3,
                         num_female_t/3,
                         num_missing_t/3,
                         num_prefCheap_t/3,
                         num_prefMed_t/3,
                         num_prefExp_t/3,
                         num_prefMissing_t/3,
                         num_colorWhite_t/3,
                         num_colorRed_t/3,
                         num_colorMissing_t/3,
                         num_countyACC_t/3,
                         num_countyM_t/3,
                         num_countySF_t/3,
                         num_countySM_t/3,
                         num_countyS_t/3,
                         num_countyMissing_t/3,
                         num_total_t/3)
)
rownames(data.genderPref) <- c("Male","Female","Gender Missing",
                               "Prefer Cheap Wine","Prefer Medium Wine","Prefer Expensive Wine","Missing Cheap/Medium/Expensive Pref",
                               "Prefer White Wine","Prefer Red Wine","Missing White/Red Wine Pref",
                               "Alameda/Contra Costa County","Marin County","San Francisco County","San Mateo County","Sonoma County","Missing County Info",
                               "Total")
colnames(data.genderPref) <- c("% (Control)","N (Control)","% (Treatment)","N (Treatment)")
table.genderPref <- xtable(data.genderPref)
align(table.genderPref) <- "|r|cc|cc|"
digits(table.genderPref) <- c(0,1,0,1,0)
print(table.genderPref,hline.after=c(-1,0,3,7,10,16,17),floating=FALSE)


# CHART 4: Histogram - Ratings Distributions
# install.packages("ggplot2")  #only need to do this once
# http://felixfan.github.io/rstudy/2014/02/28/ggplot2-cheatsheet/
library(ggplot2)
ggplotColours <- function(n=6, h=c(0, 360) +15){
    if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

data.dist <- data.frame(
    "Rating" <- c("1","2","3","4","5","1","2","3","4","5"),
    "Group" <- c("Treatment","Treatment","Treatment","Treatment","Treatment",
                 "Control","Control","Control","Control","Control"),
    "Count" <- c(nrow(data.melted[data.melted$rating==1 & data.melted$treatment==1,]),
                 nrow(data.melted[data.melted$rating==2 & data.melted$treatment==1,]),
                 nrow(data.melted[data.melted$rating==3 & data.melted$treatment==1,]),
                 nrow(data.melted[data.melted$rating==4 & data.melted$treatment==1,]),
                 nrow(data.melted[data.melted$rating==5 & data.melted$treatment==1,]),
                 nrow(data.melted[data.melted$rating==1 & data.melted$treatment==0,]),
                 nrow(data.melted[data.melted$rating==2 & data.melted$treatment==0,]),
                 nrow(data.melted[data.melted$rating==3 & data.melted$treatment==0,]),
                 nrow(data.melted[data.melted$rating==4 & data.melted$treatment==0,]),
                 nrow(data.melted[data.melted$rating==5 & data.melted$treatment==0,]))
)
colnames(data.dist) <- c("Rating","Group","Count")
ggplot(data.dist, aes(y = Count, x=factor(Rating, levels = c("1","2","3","4","5")),group=Group,shape=Group,color=Group)) +     
# ggplot(data.dist, aes(y = Score, x=factor(Rating, levels = c("1","2","3","4","5")),fill=factor(Group))) +     
    #     geom_bar(stat = "identity", position = "dodge") +
    geom_line(size=1.2) +
    geom_point(size=5) +
    ylab("Count of Ratings") +
    xlab("Rating (Likert Scale from 1 to 5)") +
    ggtitle("Distribution of Wine Ratings") +
    scale_fill_manual(values = c(ggplotColours()[4], ggplotColours()[1]))


# CHART 5: Bar Chart - Ratings (Control vs Treatment)
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
    ggtitle("Average Rating of Each Wine Type") 
#     scale_fill_manual(values = c(ggplotColours()[4], ggplotColours()[1]))


# CHART 6: Jitter Chart - Wine Ratings and Preferences
data.jitter <- data.melted[,c("rating","wine","wine_pref")]
data.jitter[data.jitter$wine == data.jitter$wine_pref,"Wine_Choice"] <- "Chose Wine"
data.jitter[data.jitter$wine != data.jitter$wine_pref,"Wine_Choice"] <- "Did Not Choose"

# colnames(data.dist) <- c("Rating","Group","Count")
# ggplot(data.jitter, aes(y = wine_pref, x=factor(rating, levels = c("1","2","3","4","5")))) +     
ggplot(data.jitter, aes(y = factor(wine, levels = c("cheap","medium","expensive")), 
                        x=rating,
                        group=Wine_Choice,color=Wine_Choice)) +     
    geom_point(position=position_jitter(width=0.1, height=0.1)) + 
    ylab("Wine Type") +
    xlab("Rating (Likert Scale from 1 to 5)") +
    ggtitle("Wine Ratings and Preferences") 
#     scale_fill_manual(values = c(ggplotColours()[4], ggplotColours()[1]))

# replot but flip the axis
ggplot(data.jitter, aes(x = factor(wine, levels = c("cheap","medium","expensive")), 
                        y=rating,
                        group=Wine_Choice,color=Wine_Choice)) +     
    geom_point(position=position_jitter(width=0.1, height=0.1)) + 
    xlab("Wine Type") +
    ylab("Rating (Likert Scale from 1 to 5)") +
    ggtitle("Wine Ratings and Preferences") 


# Check relative importance to determine covariates
# install.packages("relaimpo") #Use this to install it, do this only once
library(relaimpo)
lm.rel_importance <- lm(rating ~ quiz_score+county+age+gender+wine_color, data = data.melted)
calc.relimp(lm.rel_importance, type = c("lmg"), rela = TRUE)   # http://www.r-bloggers.com/the-relative-importance-of-predictors-let-the-games-begin/

# "Rating" <- c("1","2","3","4","5","1","2","3","4","5","1","2","3","4","5"),
# "Wine_Pref" <- c(rep("Cheap",5),rep("Medium",5),rep("Expensive",5)),
# "Count" <- c(nrow(data.melted[data.melted$wine_pref=="cheap" & data.melted$rating==1,]),
#              nrow(data.melted[data.melted$wine_pref=="cheap" & data.melted$rating==2,]),
#              nrow(data.melted[data.melted$wine_pref=="cheap" & data.melted$rating==3,]),
#              nrow(data.melted[data.melted$wine_pref=="cheap" & data.melted$rating==4,]),
#              nrow(data.melted[data.melted$wine_pref=="cheap" & data.melted$rating==5,]),
#              nrow(data.melted[data.melted$wine_pref=="medium" & data.melted$rating==1,]),
#              nrow(data.melted[data.melted$wine_pref=="medium" & data.melted$rating==2,]),
#              nrow(data.melted[data.melted$wine_pref=="medium" & data.melted$rating==3,]),
#              nrow(data.melted[data.melted$wine_pref=="medium" & data.melted$rating==4,]),
#              nrow(data.melted[data.melted$wine_pref=="medium" & data.melted$rating==5,]),
#              nrow(data.melted[data.melted$wine_pref=="expensive" & data.melted$rating==1,]),
#              nrow(data.melted[data.melted$wine_pref=="expensive" & data.melted$rating==2,]),
#              nrow(data.melted[data.melted$wine_pref=="expensive" & data.melted$rating==3,]),
#              nrow(data.melted[data.melted$wine_pref=="expensive" & data.melted$rating==4,]),
#              nrow(data.melted[data.melted$wine_pref=="expensive" & data.melted$rating==5,])
# )



# Proportion of variance explained by model: 8.11%
# Metrics are normalized to sum to 100% (rela=TRUE). 
# 
# Relative importance metrics: 
# lmg
# county     0.71464562
# quiz_score 0.02708675
# age        0.05230637
# gender     0.17773321
# wine_color 0.02822805

# Drop anything below 0.20. Therefore decide to drop age, wine_color, quiz score

# install.packages("stargazer") #Use this to install it, do this only once
# helpful link: http://jakeruss.com/cheatsheets/stargazer.html#control-the-number-of-decimal-places
library(stargazer)    
lm.no_cov = lm(rating ~ treatment * wine, data = data.melted)
lm.final = lm(rating ~ treatment * wine + county + gender , data = data.melted) 
lm.full = lm(rating ~ treatment * wine + county + gender + age + wine_color + quiz_score, data = data.melted) 
stargazer(lm.no_cov, lm.final, lm.full, type='html',
    order = c(1,2,3,12,13,4,5,6,7,8,9,10,11),
    digits=3,
    dep.var.labels="Wine Rating (Likert Scale from 1 to 5)",
    covariate.labels=c("Treatment of Showing Price","Cheap Wine [i]","Medium Wine [i]",
                       "Interaction of Treatment and Cheap Wine","Interaction of Treatment and Medium Wine",
                       "Live in Alameda/Contra Costa County [ii]","Live in Marin County [ii]","Live in San Francisco County [ii]","Live in San Mateo County [ii]",
                       "Gender Being Male","Age","Prefer White Wine","Wine Quiz Score"),
    add.lines = list(c("Chosen Model", "No", "Yes","No")),
    notes=c("(1) Regression without covariates",
            "(2) Regression with selected set of covariates (final model)",
            "(3) Regression with full set of covariates",
            "[i] Expensive Wine being the reference wine type",
            "[ii] Sonoma County being the reference county"), 
    notes.align="l",
    out='chart7_reg_nocluster.htm')

# summary(lm.no_cov)
# summary(lm.final)
# summary(lm.full)

# CALCULATE CLUSTER STANDARD ERRORS
#use 2 different clustering functions: one is for normal case
# one is for having NA in the data due to covariates (note that the cl_NAremove isn't working porperly yet)
library(sandwich)
cl <- function(fm, cluster){ 
#     require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    coeftest(fm, vcovCL)
}
cl_removeNA   <- function(fm, cluster){
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster[-fm$na.action], sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    result<-coeftest(fm, vcovCL)
    result}

#create function to extract standard errors from regression
# se_cluster <- function(cl_result){
#     sd.result <- vector()
#     for (i in cl_result[,"Std. Error"]){
#         sd.result <- c(sd.result,i)
#     }
#     sd.result}

#factor the cluster variable and apply the function (clusert variable being the id)
data.no_cov <- cl(lm.no_cov,data.melted$id)
data.no_cov
sd.no_cov <- data.no_cov[,"Std. Error"]

data.final <- cl_removeNA(lm.final,data.melted$id)  
data.final
sd.final <- data.final[,"Std. Error"]

data.full <- cl_removeNA(lm.full,data.melted$id)
data.full
sd.full <- data.full[,"Std. Error"]

stargazer(lm.no_cov, lm.final, lm.full, type='html',
          order = c(1,2,3,12,13,4,5,6,7,8,9,10,11),
          digits=3,
          dep.var.labels="Wine Rating (Likert Scale from 1 to 5)",
          covariate.labels=c("Treatment of Showing Price","Cheap Wine [i]","Medium Wine [i]",
                             "Interaction of Treatment and Cheap Wine","Interaction of Treatment and Medium Wine",
                             "Live in Alameda/Contra Costa County [ii]","Live in Marin County [ii]","Live in San Francisco County [ii]","Live in San Mateo County [ii]",
                             "Gender Being Male","Age","Prefer White Wine","Wine Quiz Score"),
          add.lines = list(c("Chosen Model", "No", "Yes","No")),
          notes=c("Clustered standard error is used (cluster by each subject id)",
                  "(1) Regression without covariates",
                  "(2) Regression with selected set of covariates (final model)",
                  "(3) Regression with full set of covariates",
                  "[i] Expensive Wine being the reference wine type",
                  "[ii] Sonoma County being the reference county"), 
          notes.align="l",
          se=list(sd.no_cov,sd.final,sd.full), #add the clustered standard error here
          out='chart7_reg_cluster.htm')

#Raffle function
raffle <- function(dataset){
    winners <- as.data.frame(matrix(,nrow=3,ncol=2))
    colnames(winners) <- c("wine_type","winner_id")
    wine_types <- c('cheap','medium','expensive')
    for (i in 1:length(wine_types)){
        data.wine_type <- dataset[dataset$wine_pref==wine_types[i],]
        lucky_draw <- vector()
        for (j in 1:nrow(data.wine_type)){
            lucky_draw<-c(lucky_draw,rep(data.wine_type[j,'id'],data.wine_type$quiz_score[j]))
        }
        print(lucky_draw)
        winners[i,] <- c(wine_types[i],sample(lucky_draw,1))
    }
    return(winners)
}

raffle(data)

########################################
#use wine preference instead of ratings
data.chosen <- data.melted
data.chosen$wine_chosen <- ifelse(data.melted$wine_pref==data.melted$wine,1,0)

lm.no_cov = lm(wine_chosen ~ treatment * wine, data = data.chosen)
lm.final = lm(wine_chosen ~ treatment * wine + county + gender , data = data.chosen) 
lm.full = lm(wine_chosen ~ treatment * wine + county + gender + age + wine_color + quiz_score, data = data.chosen) 
stargazer(lm.no_cov, lm.final, lm.full, type='html',
          order = c(1,2,3,12,13,4,5,6,7,8,9,10,11),
          digits=3,
          dep.var.labels="Wine Selection for Raffle",
          covariate.labels=c("Treatment of Showing Price","Cheap Wine [i]","Medium Wine [i]",
                             "Interaction of Treatment and Cheap Wine","Interaction of Treatment and Medium Wine",
                             "Live in Alameda/Contra Costa County [ii]","Live in Marin County [ii]","Live in San Francisco County [ii]","Live in San Mateo County [ii]",
                             "Gender Being Male","Age","Prefer White Wine","Wine Quiz Score"),
          add.lines = list(c("Chosen Model", "No", "Yes","No")),
          notes=c("(1) Regression without covariates",
                  "(2) Regression with selected set of covariates (final model)",
                  "(3) Regression with full set of covariates",
                  "[i] Expensive Wine being the reference wine type",
                  "[ii] Sonoma County being the reference county"), 
          notes.align="l",
          out='chart8_reg_raffle_nocluster.htm')

#clustered version for wine preference (instead of wine rating)
data.no_cov <- cl(lm.no_cov,data.chosen$id)
data.no_cov
sd.no_cov <- data.no_cov[,"Std. Error"]

data.final <- cl_removeNA(lm.final,data.chosen$id)  
data.final
sd.final <- data.final[,"Std. Error"]

data.full <- cl_removeNA(lm.full,data.chosen$id)
data.full
sd.full <- data.full[,"Std. Error"]

stargazer(lm.no_cov, lm.final, lm.full, type='html',
          order = c(1,2,3,12,13,4,5,6,7,8,9,10,11),
          digits=3,
          dep.var.labels="Wine Selection for Raffle",
          covariate.labels=c("Treatment of Showing Price","Cheap Wine [i]","Medium Wine [i]",
                             "Interaction of Treatment and Cheap Wine","Interaction of Treatment and Medium Wine",
                             "Live in Alameda/Contra Costa County [ii]","Live in Marin County [ii]","Live in San Francisco County [ii]","Live in San Mateo County [ii]",
                             "Gender Being Male","Age","Prefer White Wine","Wine Quiz Score"),
          add.lines = list(c("Chosen Model", "No", "Yes","No")),
          notes=c("Clustered standard error is used (cluster by each subject id)",
                  "(1) Regression without covariates",
                  "(2) Regression with selected set of covariates (final model)",
                  "(3) Regression with full set of covariates",
                  "[i] Expensive Wine being the reference wine type",
                  "[ii] Sonoma County being the reference county"), 
          notes.align="l",
          se=list(sd.no_cov,sd.final,sd.full), #add the clustered standard error here
          out='chart8_reg_raffle_cluster.htm')




