#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

library(tidyverse)
library(stargazer)
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
getwd()
setwd("C:/Users/crowl/Documents/Trinity/ASDS Course/POP77003_Applied_Statistical_Analysis_2/ProblemSet/PS2/template")
#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))

#need to change support to numeric value 0 - 1

#Climate <- read.table("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true",
#                         stringsAsFactors = TRUE) #didn't work 

#Climate3 <- load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true",
#                         colClasses = c("choice" = "factor", 
#                                        "countries" = "factor",
#                                        "sanctions" = "factor"))) #didn't work

#Part 1, Fit an additive model. 
head(climateSupport)

summary(lm(choice ~ sanctions + countries, climateSupport)) #need a non non linear regression for analysis 
summary(climateSupport)


#climateSupport <- as.logical(as.numeric(as.factor(climateSupport$choice))) 
 
?optim #checking what model to use 

reg <- glm(choice ~ ., 
           data = climateSupport, 
           family = "binomial")

summary(reg)

logit_reg <- glm(choice ~ sanctions + countries, data= climateSupport, family =    #slide 30 week 4 
                                             binomial (link = "logit"))

summary(logit_reg)

#Provide the summary output, the global null hypothesis
#p-value
#describe the results and provide a conclusion.

#anova(glm.reduced, glm.full, test="Chisq")     this is used for likihood ratio tests (week 4 slide 39)
#If p-value < ??, then we can conclude that at least one predictor is a significant predictor in logistic regression model

#2a 
logit_reg_1a <- glm(choice ~ (sanctions == "5%") + (countries == "160 of 192"), data= climateSupport, family = binomial (link = "logit"))
summary(logit_reg_1a)

logit_reg_1 <- glm(choice ~ (sanctions == "15%") + (countries == "160 of 192"), data= climateSupport, family = binomial (link = "logit"))
summary(logit_reg_1)

#2b
logit_reg_2a <- glm(choice ~ (sanctions == "5%") + (countries == "20 of 192"), data= climateSupport, family = binomial (link = "logit"))
summary(logit_reg_1a)

logit_reg_2 <- glm(choice ~ (sanctions == "15%") + (countries == "20 of 192"), data= climateSupport, family = binomial (link = "logit"))
summary(logit_reg_1)

#2c change data
reg_3 <- glm(choice ~ (sanctions == "None") + (countries == "80 of 192"), data= climateSupport, family = binomial (link = "logit"))

predicted_data <- with(climateSupport, expand.grid(choice = unique(choice),
                                               sanctions = unique(sanctions),
                                               countries = unique(countries)))


predicted_data <- cbind(predicted_data, predict(reg_3, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))


predicted_data <- within(predicted_data,
                         {
                           PredictedProb <- plogis(fit)
                           LL <- plogis(fit - (1.96 * se.fit))
                           UL <- plogis(fit + (1.96 * se.fit))
                         })

Summary(predicted_data)

#2d

reg_null1 <- glm(as.factor(choice) ~ 1, data = climateSupport, family = "binomial") 
anova(reg_null1, reg, test = "Chisq")


