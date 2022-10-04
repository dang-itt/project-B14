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
