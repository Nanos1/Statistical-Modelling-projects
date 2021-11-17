#--------part_B_--------
adat<-read.table("./chol.csv", header = TRUE)
attach(adat)
adat
plot(y~x , pch=19)
abline(lm(y~x))
model<-lm(y~x)
summary(model)
confint(model, level=0.95)
predict(model, newdata = list(x=35), interval = "prediction", level = 0.99)
predict(model, newdata = list(x=35), interval = "confidence", level = 0.99)
qqnorm(model$residuals, pch=19)
qqline(model$residuals)
plot(model, which=1, pch = 19)
par(mfrow=c(2,2))
#plot(model, pch=19)


#library(ggplot2)
#ggplot(adat, aes(x=x, y=y))+ geom_point()+ geom_smooth(method=lm, se=TRUE)

#--------part_C_--------
x = c(2, 4, 6, 12, 18, 24)
y = c(1.07, 1.88, 2.26, 2.78, 2.97, 2.99)
par(mfrow=c(1,1))
#make a transformation
z = log(3-y)
z
plot(z~x, pch=19)
abline(lm(z~x))
model<-lm(z~x)
summary(model)
confint(model, level=0.95)
predict(model, newdata = list(x=9), interval = "prediction", level = 0.95)
predict(model, newdata = list(x=9), interval = "confidence", level = 0.95)
exp(1.15435)
