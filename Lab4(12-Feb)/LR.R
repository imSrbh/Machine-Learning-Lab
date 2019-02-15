data=read.csv("C:/Users/imsau/Desktop/6th Sem/ML/ML_lab/Lab4(12-Feb)/dmf.csv")
model1= lm(data$dmf~data$flor)
summary (model1)
err= residuals (model1)
hist(err)
plot(model1$fitted.values,model1$residuals)
plot(model1$fitted.values, data$dmf)


flor2 <- data$flor^2
model2=lm(data$dmf~ data$flor+flor2)
summary(model2)
err2= residuals(model2)
hist(err2)
plot(model2$fitted.values,model2$residuals)
plot(model2$fitted.values,data$dmf)


sqrtflor <- sqrt(data$flor)
model3 = lm(data$dmf~ data$flor+flor2+1/sqrtflor)
summary(model3)

err3= residuals(model3)
hist(err3)
plot(model3$fitted.values,model3$residuals)
plot(model3$fitted.values,data$dmf)


fourootflor <- sqrt(sqrt(data$flor))
model4= lm(data$dmf ~log(data$flor+1)+ data$flor + flor2+1/sqrtflor + 1/fourootflor)
summary(model4)
err4= residuals(model4)
hist(err4)
plot(model4$fitted.values,model4$residuals)
plot(model4$fitted.values,data$dmf)
