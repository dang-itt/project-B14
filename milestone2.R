library(readxl)
library(dplyr)

data<-read_xlsx("C:/Users/user-pc/Downloads/data for part 2 (not stacked).xlsx")


#appliance variables
app_years <-data$Year...2
app_temp <-data$Temp...3
app_births <-data$Births...4
app_workdays <-data$Workdays...5
app_weekends <-data$Weekends...6
app_holidays <-data$Holidays...7
app_summer <-data$Summer...8
app_autumn <-data$Autumn...9
app_winter <-data$Winter...10
app_spring <-data$Spring...11
app_returns <-data$Returns...12
#appliance model
app_model<- lm(app_returns ~ app_years+app_temp+app_births+app_workdays+app_weekends+app_holidays+app_summer+app_autumn+app_winter+app_spring)
app_summary<-summary(app_model)
app_summary
coefficients(app_model)
layout(matrix(c(1,2,3,4),2,2)) 
plot(app_model)

#baby variables
bab_years <-data$Year...14
bab_temp <-data$Temp...15
bab_births <-data$Births...16
bab_workdays <-data$Workdays...17
bab_weekends <-data$Weekends...18
bab_holidays <-data$Holidays...19
bab_summer <-data$Summer...20
bab_autumn <-data$Autumn...21
bab_winter <-data$Winter...22
bab_spring <-data$Spring...23
bab_returns <-data$Returns...24
#baby model
bab_model<- lm(bab_returns ~ bab_years+bab_temp+bab_births+bab_workdays+bab_weekends+bab_holidays+bab_summer+bab_autumn+bab_winter+bab_spring)
bab_summary<-summary(bab_model)
bab_summary
coefficients(bab_model)
layout(matrix(c(1,2,3,4),2,2)) 
plot(bab_model)

#bags variables
bag_years <-data$Year...26
bag_temp <-data$Temp...27
bag_births <-data$Births...28
bag_workdays <-data$Workdays...29
bag_weekends <-data$Weekends...30
bag_holidays <-data$Holidays...31
bag_summer <-data$Summer...32
bag_autumn <-data$Autumn...33
bag_winter <-data$Winter...34
bag_spring <-data$Spring...35
bag_returns <-data$Returns...36
#bags model
bag_model<- lm(bag_returns ~ bag_years+bag_temp+bag_births+bag_workdays+bag_weekends+bag_holidays+bag_summer+bag_autumn+bag_winter+bag_spring)
bag_summary<-summary(bag_model)
bag_summary
coefficients(bag_model)
layout(matrix(c(1,2,3,4),2,2)) 
plot(bag_model)

#blankets variables
blanket_years <-data$Year...38
blanket_temp <-data$Temp...39
blanket_births <-data$Births...40
blanket_workdays <-data$Workdays...41
blanket_weekends <-data$Weekends...42
blanket_holidays <-data$Holidays...43
blanket_summer <-data$Summer...44
blanket_autumn <-data$Autumn...45
blanket_winter <-data$Winter...46
blanket_spring <-data$Spring...47
blanket_returns <-data$Returns...48
#blanket model
blanket_model<- lm(blanket_returns ~ blanket_years+blanket_temp+blanket_births+blanket_workdays+blanket_weekends+blanket_holidays+blanket_summer+blanket_autumn+blanket_winter+blanket_spring)
blanket_summary<-summary(blanket_model)
blanket_summary
coefficients(blanket_model)
layout(matrix(c(1,2,3,4),2,2)) 
plot(blanket_model)

#r > 0.7 this value is generally considered strong effect size

#Accuracy calculations

#It is noted that the spring feature is linearly related to other variables and no unique solution exists without dropping the feature.
coef <- as.numeric(coefficients(blanket_model[-11]))
blanket_linregeq <- coef[1] + coef[2]*blanket_years+coef[3]*blanket_temp+coef[4]*blanket_births+coef[5]*blanket_workdays+coef[6]*blanket_weekends+coef[7]*blanket_holidays+coef[8]*blanket_summer+coef[9]*blanket_autumn+coef[10]*blanket_winter
blanket_MAPE <- 100*sum(abs((blanket_returns-blanket_linregeq)/blanket_returns))/length(blanket_returns)
blanket_MAPE 

coef <- as.numeric(coefficients(bag_model[-11]))
bag_linregeq <- coef[1] + coef[2]*bag_years+coef[3]*bag_temp+coef[4]*bag_births+coef[5]*bag_workdays+coef[6]*bag_weekends+coef[7]*bag_holidays+coef[8]*bag_summer+coef[9]*bag_autumn+coef[10]*bag_winter
bag_MAPE <- 100*sum(abs((bag_returns-bag_linregeq)/bag_returns))/length(bag_returns)
bag_MAPE 

coef <- as.numeric(coefficients(bab_model[-11]))
bab_linregeq <- coef[1] + coef[2]*bab_years+coef[3]*bab_temp+coef[4]*bab_births+coef[5]*bab_workdays+coef[6]*bab_weekends+coef[7]*bab_holidays+coef[8]*bab_summer+coef[9]*bab_autumn+coef[10]*bab_winter
bab_MAPE <- 100*sum(abs((bab_returns-bab_linregeq)/bab_returns))/length(bab_returns)
bab_MAPE 

coef <- as.numeric(coefficients(app_model[-11]))
app_linregeq <- coef[1] + coef[2]*app_years+coef[3]*app_temp+coef[4]*app_births+coef[5]*app_workdays+coef[6]*app_weekends+coef[7]*app_holidays+coef[8]*app_summer+coef[9]*app_autumn+coef[10]*app_winter
app_MAPE <- 100*sum(abs((app_returns-app_linregeq)/app_returns))/length(app_returns)
app_MAPE
