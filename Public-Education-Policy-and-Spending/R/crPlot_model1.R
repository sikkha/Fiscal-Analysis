# Assignment1

# Loading
library("tidyverse")
library("readxl")


#setwd("~/Desktop/workspace/DA8410 Fiscal and Monetary Policy Analysis/DA8410 Part 2")
setwd("~/Documents/GitHub/Fiscal-Analysis/Public-Education-Policy-and-Spending/data")
# xls files
#my_data <- read_excel("DA.841,PA8603-DATA.xls")
my_data <- read.csv("DA.841,PA8603-DATA_CSV.csv")

#data check (for debug purpose)
#head("my_data", 4)
#print(my_data)

#demand-side explanation
#Model1: examining all independent variables

## old version
model1 <- lm(GEDU ~ GDP + POP + URB + GLOBAL + INEQTY + REV + TRADE + LABOR + DGOV + GEDUtm1, data = my_data)
#summary(model1) $coefficient
summary(model1)
confint (model1)

## new version
#fitted.model1 <- lm(GEDU ~ GDP + POP + URB + GLOBAL + INEQTY + REV + TRADE + LABOR + DGOV + GEDUtm1, data = my_data)
#summary(model1) $coefficient
#confint (model1)


#Model2: Wagner's law
model2 <- lm(GEDU ~ GDP + POP + URB + REV, data = my_data)
model2_1 <- lm(GEDU ~ POP + URB + REV, data = my_data)
model2_2 <- lm(GEDU ~ GDP + POP + URB, data = my_data)

summary(model2) 
confint(model2)

summary(model2_1)
confint(model2_1)

summary(model2_2)
confint(model2_2)


#Model3: Compensation Theory
model3 <- lm(GEDU ~ GLOBAL + DGOV, data = my_data)
summary(model3) 
confint(model3)

#Model4: Median Voter
model4 <- lm(GEDU ~ INEQTY + DGOV, data = my_data)
summary(model4) 
confint(model4)

#Model5: Interest Group Theory
model5 <- lm(GEDU ~ TRADE + LABOR, data = my_data)
summary(model5) 
confint(model5)

#Supply-Side Explanation
#Model 6: Incrementalism Theory
model6 <- lm(GEDU ~ GEDUtm1, data = my_data)
summary(model6) 
confint(model6)


#library(car)
#vif(model1)

#check for multicollinearity problem
library(corpcor)
cor2pcor(cov(model1))

library(mctest)
imcdiag(mod = model1, method = "VIF", vif = 5)
imcdiag(mod = model2, method = "VIF", vif = 5)
imcdiag(mod = model2_1, method = "VIF", vif = 5)
imcdiag(mod = model2_2, method = "VIF", vif = 5)
imcdiag(mod = model3, method = "VIF", vif = 5)
imcdiag(mod = model4, method = "VIF", vif = 5)
imcdiag(mod = model5, method = "VIF", vif = 5)
imcdiag(mod = model6, method = "VIF", vif = 5)

#library(GGally)
#ggpairs(mydata)
#ggpairs(my_data, c('GEDU', 'GDP', 'POP', 'URB', 'GLOBAL', 'INEQTY', 'REV', 'TRADE', 'LABOR', 'DGOV', 'GEDUtm1'))

#library(car)
#crPlots(model1)
#crPlots(model2_1)
#crPlots(model3)
#crPlots(model4)
#crPlots(model5)
#crPlots(model6)

library(apaTables)
apa.reg.table(model1)

library(stargazer)
stargazer(model1, model2_1, model3, model4, model5, model6, type="text")

library(broom)
tidy(model1)

#mathmatical derived by equatiamatic
library(equatiomatic)
cat(extract_eq(model1))

