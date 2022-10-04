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

layout(matrix(c(1,2,3,4),2,2)) 
plot(blanket_model)

#bedding variables
bedding_years <-data$Year...50
bedding_temp <-data$Temp...51
bedding_births <-data$Births...52
bedding_workdays <-data$Workdays...53
bedding_weekends <-data$Weekends...54
bedding_holidays <-data$Holidays...55
bedding_summer <-data$Summer...56
bedding_autumn <-data$Autumn...57
bedding_winter <-data$Winter...58
bedding_spring <-data$Spring...59
bedding_returns <-data$Returns...60
#bedding model
bedding_model<- lm(bedding_returns ~ bedding_years+bedding_temp+bedding_births+bedding_workdays+bedding_weekends+bedding_holidays+bedding_summer+bedding_autumn+bedding_winter+bedding_spring)
bedding_summary<-summary(bedding_model)
bedding_summary

layout(matrix(c(1,2,3,4),2,2)) 
plot(bedding_model)

#kids variables
kids_years <-data$Year...62
kids_temp <-data$Temp...63
kids_births <-data$Births...64
kids_workdays <-data$Workdays...65
kids_weekends <-data$Weekends...66
kids_holidays <-data$Holidays...67
kids_summer <-data$Summer...68
kids_autumn <-data$Autumn...69
kids_winter <-data$Winter...70
kids_spring <-data$Spring...71
kids_returns <-data$Returns...72
#kids model
kids_model<- lm(kids_returns ~ kids_years+kids_temp+kids_births+kids_workdays+kids_weekends+kids_holidays+kids_summer+kids_autumn+kids_winter+kids_spring)
kids_summary<-summary(kids_model)
kids_summary

layout(matrix(c(1,2,3,4),2,2)) 
plot(kids_model)

#apparel variables
appar_years <-data$Year...74
appar_temp <-data$Temp...75
appar_births <-data$Births...76
appar_workdays <-data$Workdays...77
appar_weekends <-data$Weekends...78
appar_holidays <-data$Holidays...79
appar_summer <-data$Summer...80
appar_autumn <-data$Autumn...81
appar_winter <-data$Winter...82
appar_spring <-data$Spring...83
appar_returns <-data$Returns...84
#apparel model
appar_model<- lm(appar_returns ~ appar_years+appar_temp+appar_births+appar_workdays+appar_weekends+appar_holidays+appar_summer+appar_autumn+appar_winter+appar_spring)
appar_summary<-summary(appar_model)
appar_summary

layout(matrix(c(1,2,3,4),2,2)) 
plot(appar_model)

#r > 0.7 this value is generally considered strong effect size

#Accuracy calculations

#It is noted that the spring feature is linearly related to other variables and no unique solution exists without dropping the feature.
coef <- as.numeric(coefficients(blanket_model[-11]))
blanket_linregeq <- coef[1] + coef[2]*blanket_years+coef[3]*blanket_temp+coef[4]*blanket_births+coef[5]*blanket_workdays+coef[6]*blanket_weekends+coef[7]*blanket_holidays+coef[8]*blanket_summer+coef[9]*blanket_autumn+coef[10]*blanket_winter
blanket_MAPE <- 100*sum(abs((blanket_returns-blanket_linregeq)/blanket_returns))/length(blanket_returns)
blanket_MAPE
coefficients(blanket_model)

coef <- as.numeric(coefficients(bag_model[-11]))
bag_linregeq <- coef[1] + coef[2]*bag_years+coef[3]*bag_temp+coef[4]*bag_births+coef[5]*bag_workdays+coef[6]*bag_weekends+coef[7]*bag_holidays+coef[8]*bag_summer+coef[9]*bag_autumn+coef[10]*bag_winter
bag_MAPE <- 100*sum(abs((bag_returns-bag_linregeq)/bag_returns))/length(bag_returns)
bag_MAPE 
coefficients(bag_model)

coef <- as.numeric(coefficients(bab_model[-11]))
bab_linregeq <- coef[1] + coef[2]*bab_years+coef[3]*bab_temp+coef[4]*bab_births+coef[5]*bab_workdays+coef[6]*bab_weekends+coef[7]*bab_holidays+coef[8]*bab_summer+coef[9]*bab_autumn+coef[10]*bab_winter
bab_MAPE <- 100*sum(abs((bab_returns-bab_linregeq)/bab_returns))/length(bab_returns)
bab_MAPE 
coefficients(bab_model)

coef <- as.numeric(coefficients(app_model[-11]))
app_linregeq <- coef[1] + coef[2]*app_years+coef[3]*app_temp+coef[4]*app_births+coef[5]*app_workdays+coef[6]*app_weekends+coef[7]*app_holidays+coef[8]*app_summer+coef[9]*app_autumn+coef[10]*app_winter
app_MAPE <- 100*sum(abs((app_returns-app_linregeq)/app_returns))/length(app_returns)
app_MAPE
coefficients(app_model)

coef <- as.numeric(coefficients(bedding_model[-11]))
bedding_linregeq <- coef[1] + coef[2]*bedding_years+coef[3]*bedding_temp+coef[4]*bedding_births+coef[5]*bedding_workdays+coef[6]*bedding_weekends+coef[7]*bedding_holidays+coef[8]*bedding_summer+coef[9]*bedding_autumn+coef[10]*bedding_winter
bedding_MAPE <- 100*sum(abs((bedding_returns-bedding_linregeq)/bedding_returns))/length(bedding_returns)
bedding_MAPE
coefficients(bedding_model)

coef <- as.numeric(coefficients(kids_model[-11]))
kids_linregeq <- coef[1] + coef[2]*kids_years+coef[3]*kids_temp+coef[4]*kids_births+coef[5]*kids_workdays+coef[6]*kids_weekends+coef[7]*kids_holidays+coef[8]*kids_summer+coef[9]*kids_autumn+coef[10]*kids_winter
kids_MAPE <- 100*sum(abs((kids_returns-kids_linregeq)/kids_returns))/length(kids_returns)
kids_MAPE
coefficients(kids_model)

coef <- as.numeric(coefficients(appar_model[-11]))
appar_linregeq <- coef[1] + coef[2]*appar_years+coef[3]*appar_temp+coef[4]*appar_births+coef[5]*appar_workdays+coef[6]*appar_weekends+coef[7]*appar_holidays+coef[8]*appar_summer+coef[9]*appar_autumn+coef[10]*appar_winter
appar_MAPE <- 100*sum(abs((appar_returns-appar_linregeq)/appar_returns))/length(appar_returns)
appar_MAPE
coefficients(appar_model)