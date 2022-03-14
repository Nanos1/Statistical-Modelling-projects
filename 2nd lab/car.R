library(MASS) 
library(qqplotr)
library(car)
cars <- read.table('C:/Users/nanos/OneDrive/Υπολογιστής/edem/Statistical Modelling/labs/2nd lab/cars.txt', header = TRUE)
attach(cars)
model <- lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb)
summary(model)
plot(model, which=1,pch=20)
plot(model, which=2,pch=20)
plot(cooks.distance(model), pch=20)
plot(hatvalues(model), pch=20)
par (mfrow=c(2,2))
plot(model, pch=20)
dfbetas(model)
dffits(model)
vif(model)

vif(lm(mpg~cyl+hp+drat+wt+qsec+vs+am+gear+carb))
vif(lm(mpg~hp+drat+wt+qsec+vs+am+gear+carb))
plot(cars)

mod_fw = step(lm(mpg~1), mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, direction = 'forward', test = 'F')  
mod_bw = step(model, direction = 'backward', test='F')
mod_both = step(model, direction = 'both', test='F')

AIC(mod_fw)
AIC(mod_bw)

#finding press. Press has a relationship with Rsq-pred. Minimum press equiv to Maximum Rsq-pred
library(MPV)
PRESS(mod_fw)
PRESS(mod_bw)

#finding Rsq and Rsq-adj
summary(mod_fw)
summary(mod_bw)

#finding cp-mallows
library(olsrr)
ols_mallows_cp(mod_fw, model)
ols_mallows_cp(mod_bw, model)
#best subsets using leaps
#library(leaps)

#Added variable and Component residual plots
#1st model
avPlots(mod_fw)
crPlots(mod_fw)

#2nd model
avPlots(mod_bw)
crPlots(mod_bw)


#box cox transformation
library(MASS)
bc = boxcox(mod_bw,lambda = seq(-3,3))

par(mfrow=c(2,2))
mod_bw.inv = lm(log(mpg) ~ wt+cyl+am)
plot(mod_bw.inv)
vif(mod_bw.inv)

par(mfrow=c(1,1))
bc = boxcox(mod_bw,lambda = seq(-3,3))

mod_bw.inv = lm(log(mpg) ~ wt+cyl+am)
vif(mod_bw.inv)

dfb <- dfbetas(mod_bw.inv)
print(dfb[dfb > 0.35])

dff <- dffits(mod_bw.inv)
print(dff[dff > 6.5])

plot(cooks.distance(mod_bw.inv))
plot(hatvalues(mod_bw.inv))
