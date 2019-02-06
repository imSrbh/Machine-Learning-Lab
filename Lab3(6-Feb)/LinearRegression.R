
data=read.csv("C:/Users/imsau/Desktop/6th Sem/ML/ML_lab/Lab3(6-Feb)/drug2.csv")
View(data)
summary(data)
str(data)
boxplot(data$dose)
boxplot(data$response)

head(data)
scatter.smooth(x=data$dose, y=data$response, main="dose ~ response")

cor(data$dose, data$response)
cor(data$sex, data$response)

model1 = lm(data$response~data$dose)
summary(model1)
error = residuals(model1)
error
hist(error)

scatter.smooth(x=model1$residuals, y=model1$fitted.values,colors(), main="predicted value vs error")
scatter.smooth(x=data$response, y=model1$fitted.values, main="predicted value vs Actual value")


model2 = lm(data$response~data$dose+data$sex)
summary(model2)
error2 = residuals(model2)
hist(error2)

scatter.smooth(x=model2$residuals, y=model2$fitted.values, main="Model2-predicted value vs error")
scatter.smooth(x=data$response+data$sex, y=model2$fitted.values, main="Model2-predicted value vs Actual value")


library(e1071)
par(mfrow=c(1, 3))  # divide graph area in 2 columns

plot(density(data$dose), main="Density Plot: Dose", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data$dose), 2)))  # density plot for 'speed'
polygon(density(data$dose), col="green")
plot(density(data$response), main="Density Plot: Response", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data$response), 2)))  # density plot for 'dist'
polygon(density(data$response), col="green")
plot(density(data$sex), main="Density Plot: sex", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(data$sex), 2)))  # density plot for 'dist'
polygon(density(data$sex), col="green")



modelSummary <- summary(model1)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
 
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
