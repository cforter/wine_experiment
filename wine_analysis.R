data = read.csv("Data_Collection - R_data.csv")
data = data[complete.cases(data),]

#ignore cluster error adjustments for now
lm_cheap = lm(rating_cheap ~ treatment + quiz_score + zip_code + age + gender + group, data=data)
lm_medium = lm(rating_cheap ~ treatment + quiz_score + zip_code + age + gender + group, data=data)
lm_expensive = lm(rating_cheap ~ treatment + quiz_score + zip_code + age + gender + group, data=data)

lm_cheap = lm(rating_cheap ~ treatment, data=data)

