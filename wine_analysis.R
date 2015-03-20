data <- read.csv("Data_Collection - R_data.csv")
data <- data[complete.cases(data),]

#ignore cluster error adjustments for now
lm_cheap <- lm(rating_cheap ~ treatment + quiz_score + zip_code + age + gender + group, data=data)
lm_medium <- lm(rating_medium ~ treatment + quiz_score + zip_code + age + gender + group, data=data)
lm_expensive <- lm(rating_expensive ~ treatment + quiz_score + zip_code + age + gender + group, data=data)

lm_cheap <- lm(rating_cheap ~ treatment, data=data)

# Model with all price points and heterogeneous treatment effects, cheap = base-level factor
# Raw treatment coefficient represents treatment effect for the cheap wine
# Not fully saturated, but we might want to consider adding interaction terms for geneder etc.
# in case there are other heterogeneous effects
# We'll need to melt data and restructure so each wine rating is its own record
lm.all <- lm(rating_all ~ treatment + quiz_score + zip_code
             + age + gender + group + mid_dummy + expensive_dummy
             + (mid_dummy * treatment) + (expensive_dummy * treatment), data=data)

summary(lm.all)



