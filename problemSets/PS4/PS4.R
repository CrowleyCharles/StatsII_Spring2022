################################################################################################################
#Loading packages and setting up environment
###########################################
rm(list=ls())
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# loading libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}


lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

getwd()
setwd("C:/Users/crowl/Documents/Trinity/ASDS Course/POP77003_Applied_Statistical_Analysis_2/ProblemSet/PS4")

################################################################################################################
#structuring/inspecting the data
################################

data(child) #our dataset 

child_surv <- with(child, Surv(enter, exit, event)) #building a survival object for children
km <- survfit(child_surv ~ 1, data = child) #children end at the age of 15 
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km) #visualization of the survival rate of children in our dataset over time period of 15 years. 

km_m.age <- survfit(child_surv ~ m.age, data = child) #doesn't give a readable output due to formatting issues 
autoplot(km_m.age)


km_sex <- survfit(child_surv ~ sex, data = child) #gives a visualization of the survival rates of male vs female children over first 15 years
autoplot(km_sex) #we can see that female children have a higher survival rate, when compared to their male counterparts 

################################################################################################################
#Fitting a Cox Proportional Hazard Model
########################################


cox <- coxph(Surv(enter, exit, event) ~ sex + m.age, data = child)
summary(cox)
drop1(cox, test = "Chisq") #9.4646
stargazer(cox, type = "latex")

# There is a 0.0822 decrease in the expected log of the hazard for female babies compared to 
# male, holding m.age constant. There is a 0.0076 increase in the expected log of the hazard
# for babies of mothers with one extra unit of age, holding sex constant.


exp(-0.082215) #exponentiate parameter estimates to obtain hazard ratios for gender
# The hazard ratio of female babies is 0.9210739 that of male babies. This means that for every 100 male babies that die, we would expect
# that only ~ 92 female babies would die. This could be interrupted in another sense as female babies have a 8% lower death rate than equivalent 
# male babies.

exp(0.007617) #exponentiate parameter estimates to obtain hazard ratios for mother's age 
# The hazard ratio of a mother aged 0 is 1.0076466, this number is not quite accurate as no mother can have a child a age zero but it does allow 
# us to analyse a one unit increase in age, and its affect on infant mortality. This could be interrupted in another sense as for each unit 
# increase in a mothers age, the chance of that child dying is 0.7 percent higher than if the mother was one unit of age younger. 

cox_fit <- survfit(cox)
autoplot(cox_fit)

max(child$m.age) #obtaining the range of ages for mothers 
min(child$m.age)
mean(child$m.age)

newdata <- with(child, data.frame(sex = c("male", "female"), m.age = cut(x = child$m.age, breaks = c(15.82, 32.03376, 50.864), labels=c("15-32", "32-51"))))
#creating two age categories (15-32, 33-51) however decided it did not add much to the model so left it out. 


# Adding an interaction model
cox.int <- coxph(child_surv ~ sex * m.age, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq") #0.10623
stargazer(cox.int, type = "latex")

# There is a 0.127105 decrease in the expected log of the hazard for female babies compared to male babies, holding m.age constant.
# There is a 0.006963 increase in the expected log of the hazard for babies of mothers with one extra unit of age, holding sex constant. 
# However the P-Values are at level that they are not statistically significant when compared to the additive model.

exp(-0.127105) #exponentiate parameter estimates to obtain hazard ratios for mother's age 
# The hazard ratio of a mother aged 0 is 0.8806412 that of male babies. This means that for every 100 male babies that die, we would expect
# that only ~ 88 female babies would die. This could be interrupted in another sense as female babies have a 8% lower death rate than equivalent 
# male babies. 

exp(0.006963) #exponentiate parameter estimates to obtain hazard ratios for mother's age 
# The hazard ratio of a mother aged 0 is 1.006987, this number is not quite accurate as no mother can have a child a age zero but it does allow 
# us to analyse a one unit increase in age, and its affect on infant mortality. This could be interrupted in another sense as for each unit 
# increase in a mothers age, the chance of that child dying is 0.6987 percent higher than if the mother was one unit of age younger. 