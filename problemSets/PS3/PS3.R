getwd()
setwd("C:/Users/crowl/Documents/Trinity/ASDS Course/POP77003_Applied_Statistical_Analysis_2/ProblemSet/PS3/template")

#Load in data 

data <- read.csv("C:/Users/crowl/Documents/Trinity/ASDS Course/POP77003_Applied_Statistical_Analysis_2/ProblemSet/PS3/template/GDPchange.csv")


library(foreign)
library(nnet)
library(stargazer)
library(jtools)
library(MASS)
library(nnet)
library(ggplot2)
install.packages("pscl")
library(pscl)

summary(data)

ftable(xtabs(~ GDPWdiff + OIL + REG, data = data)) #creating a contingency table



#############
#Data wrangling
#############
#Must create the 3 categories for GDPWdiff of positive (>0), negative (<0), and no change (=0)

data$GDPWdiff <- cut(x = data$GDPWdiff, breaks = c(-9257, -0.9, 0.1, 7867)) #creating bounds of values for the levels, as they are real numbers  
levels(data$GDPWdiff) <- c("negative", "no_change", "positive")

data$GDPWdiff


data$GDPWdiff <- factor(data$GDPWdiff , ordered = FALSE ) #how to make a table un-ordered

table(data$GDPWdiff)


ftable(xtabs(~ GDPWdiff + OIL + REG, data = data))
########################################################################################
#Fitting the data to an unordered multinomial logit model
# 

data$GDPWdiff = relevel(data$GDPWdiff, ref = "no_change") #setting the reference level to 0

# run model
mult.log <- multinom(GDPWdiff ~ OIL + REG, data = data)
summary(mult.log)
expc <- exp(coef(mult.log))

stargazer(expc, type="latex")

# get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
z
p <- ((1 - pnorm(abs(z), 0, 1)) * 2) #2 tailed z test 
p

stargazer(p, type="latex")

library(stargazer)
stargazer(mult.log, type="latex") #gives the P value and coefficents

head(fitted(mult.log)) # checking the fitted values 


##############
#Part 2
##############

ord.log <- polr(GDPWdiff ~ OIL + REG, data = data, Hess = TRUE)
summary(ord.log)

stargazer(ord.log, type="latex")

# Calculate a p value
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

stargazer(ctable, type="latex")

# Calculate confidence intervals
ci <- confint(ord.log)
ci


# convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

###########################################################################################
#Question 2
###########################################################################################

#Part (A)
Mex_data <- read.csv("C:/Users/crowl/Documents/Trinity/ASDS Course/POP77003_Applied_Statistical_Analysis_2/ProblemSet/PS3/template/MexicoMuniData.csv")

summary(Mex_data)
str(Mex_data)

with(Mex_data, #Testing to see if our response variable meets the assumptions for a Poisson test. 
     list(mean(PAN.visits.06), var(PAN.visits.06))) #The outcome would suggest it does as the expected mean and expected  are approx equal 

#Firstly need to check the data is ok? and see if anything needs to be categorized 
#As competitive.district and PAN.governor.06 are binary (0/1) responses, I will turn them into logical (true/false) responses

Mex_data <- within(Mex_data, {
  PAN.governor.06 <- as.logical(PAN.governor.06)
  competitive.district <- as.logical(competitive.district)
})

mod.pos <- glm(PAN.visits.06 ~ ., data = Mex_data, family = poisson(link = "log")) 
summary(mod.pos)


table(Mex_data$competitive.district) #this shows that FALSE is the reference category 

                                     #I want to change this to TRUE to easily compare my categories 
Mex_data$competitive.district <- factor(Mex_data$competitive.district , ordered = FALSE )

Mex_data$competitive.district2 <- relevel(Mex_data$competitive.district, ref= "TRUE")

table(Mex_data$competitive.district2) #shows the referennce has now changed 

mod.pos1 <- glm(PAN.visits.06 ~ MunicipCode + pan.vote.09 + marginality.06 + PAN.governor.06 +competitive.district2, data = Mex_data, family = poisson)
summary(mod.pos1)

stargazer(mod.pos1, type="latex") #summary of first regression 


stargazer(mod.pos, type="latex") #summary of second regression with the reference changed 

######################

#Part B
cofs <- coef(mod.pos)
cofs

stargazer(cofs, type="latex")

confint(mod.pos) #confidence interval set at 95%

exp(coef(mod.pos)) #exponential model coefficients 
exp(confint(mod.pos)) #CI for the exponential model coefficients


sjPlot::tab_model(mod.pos , show.intercept = TRUE, #shows the P value 
                  show.se = FALSE, dv.labels = "Predictors of presidental Visit", auto.label = TRUE, show.re.var = FALSE, show.icc =FALSE, 
                  show.r2 = FALSE, show.ngroups = FALSE, show.obs = FALSE)


#Part C 

# I note that the model has many Zero's, which may cause Zero inflation 


exp(cofs[1] + cofs[2] + cofs[3] + cofs[4]*0 + cofs[5]*1 + cofs[6]*1)

pred <- data.frame(Mex_data$MunicipCode, 
                   Mex_data$pan.vote.09,
                   marginality.06 = 0,
                   PAN.governor.06 = TRUE,
                   competitive.district = TRUE)

colnames(pred)

names(pred)[1] <- "MunicipCode" #had to rename the columns, or they would not be recognised
names(pred)[2] <- "pan.vote.09"

colnames(pred)


# check with predict() function
predict(mod.pos, newdata = pred, type = "response")

mod.zip <- zeroinfl(PAN.visits.06 ~ ., data = Mex_data, dist = "poisson") #i get an error message when I attempt to run this, it a non finite 
summary(mod.zip)                                                          # vlaue has been supplied, I have not been able to reslove it. 
