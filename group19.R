## Stats 506, Fall 2018, Group Project.
## Author: Mimi Tran.
## Date: November 25, 2018.
## Data: Insurance

# Library----------------------------------------------------------------------
library(readr)
library(dplyr)
library(faraway)
# Load data--------------------------------------------------------------------
ds0 <- read_csv("Documents/Academics/University of Mihigan/Mimi's classes/Stats 506/project 506/insurance.csv")
names(ds0)

# Check if is there any null---------------------------------------------------
is.na(ds0)
# No missing value, we're good.

# Recode: sex:femal = 1, male = 0, smoke: yes=1, no=0
ds0$sex[ds0$sex == "male"]="0"
ds0$sex[ds0$sex == "female"]="1"
ds0$smoker[ds0$smoker == "no"]="0"
ds0$smoker[ds0$smoker == "yes"]="1"
View(ds0)

# Take log for charge since its heavy tail-------------------------------------
hist(ds0$charges)
ds0$logcharges <- log(ds0$charges+1)
hist(ds0$logcharges, breaks = 10)
View(ds0)

## Model diagnostic:-----------------------------------------------------------

# VIF of each column-----------------------------------------------------------
# Load library(faraway) to get funtion vif, and we see vif of each column is ok
# since they're all smaller than 5, even smaller than 2.
fit0 <- lm(logcharges ~age+sex+bmi+children+smoker+as.factor(region), data=ds0)
X <- model.matrix(fit0)[, -1]
round(vif(X),2)

# Residual distribution--------------------------------------------------------
hist(fit0$residuals, xlab="Residuals")
plot(fit0$res, xlab="Residuals")
abline(h=0) # acting like noraml so it's good.

# Since the residuals itself is normal, box-cox is not neceesary---------------

# Partial residual plots-------------------------------------------------------
#Which attempts to show how covariate is related to dependent variable
# if we control for the effects of all other covariates
# partial residual plots look acceptable.
fit <- lm(logcharges~ bmi, data=ds0)
plot(fit)
plot(bmi, coef(fit0)["bmi"]*bmi+fit0$residuals)

## Model Selection-------------------------------------------------------------
# Original model
fit0 <- lm(logcharges ~age+sex+bmi+children+smoker+as.factor(region), data=ds0)
summary(fit0)
AIC(fit0)
BIC(fit0)

# Try No.1: add interactive covariate smoker*bmi
fit1 <-lm(logcharges ~age+sex+bmi+children+smoker+as.factor(region)+smoker*bmi, data=ds0)
summary(fit1)
BIC(fit1)
# which certainly improve the performance of the model

# Try No.2: add an interactive covariate smoker*age
fit2 <-lm(logcharges ~age+sex+bmi+children+smoker+as.factor(region)+smoker*bmi,as.numeric(smoker)*age, data=ds0)
summary(fit2)
BIC(fit2)
# which increase the performance of the model significantly

# Try No.3: since we only have two continuous covariates, we can try to give them an extra power.
# add bmi^1.5
fit3 <-lm(logcharges ~age+sex+bmi+children+smoker+as.factor(region)+
            smoker*bmi,as.numeric(smoker)*age+bmi^1.5, data=ds0)
summary(fit3)
BIC(fit3)
# R^2 is decreased however BIC is improved.(you let me know Ben)

# Try No.4: add age^1.5
fit4 <-lm(logcharges ~age+sex+bmi+children+smoker+as.factor(region)+
            smoker*bmi,as.numeric(smoker)*age+bmi^1.5+age^1.5, data=ds0)
summary(fit4)
BIC(fit4)
# R^ decreases and BIC increases, not a good model.

# Try No.5: use charges instead of logcharges for the response.
fit5 <-lm(charges ~age+sex+bmi+children+smoker+as.factor(region)+
            smoker*bmi,as.numeric(smoker)*age+bmi^1.5+age^1.5, data=ds0)
summary(fit5)
BIC(fit5)
# Still not as good as No.2
# According to the analysis above, the best model is model No.2 where:
# logcharges= ageage+sex+bmi+children+smoker+as.factor(region)+smoker*bmi,as.numeric(smoker)*age
