library(readxl)
library(e1071)
library(MLmetrics)
library(tseries)
library(forecast)
library(lmtest)
library(zoo)

##METODE ARIMA

setwd("D:/KULIAH/SKRIPSI")

#INPUTDATA
data=read_excel("week2a.xlsx")
data

IDR = data$nilai
IDR

#EKSPLORASI DATA
plot(IDR, type = "l", lwd = 3, main = "Plot Nilai Tukar Rupiah terhadap Dolar AS")
ujiterasvirta = terasvirta.test(as.ts(IDR),type = "Chisq")
ujiterasvirta
qchisq(0.95,2)

#PEMBAGIAN TRAINING TESTING
trainIDR = IDR[1:84]
testIDR = IDR[85:105]
plot(IDR, type = "l", lwd = 3, main = "Plot Pembagian Training Set dan Testing Set")
abline(v=84, lwd = 3, lty = 2) # PEMBATAS TESTING TRAINING
lines(trainIDR, type = "l", lwd = 3, col ="red")
lines(85:105,testIDR, type = "l", lwd = 3,col ="blue")
legend("topleft",
       legend = c("Data Training "
                  ,"Data Testing "),
       col = c("red","blue"),
       lty = c(1,1),
       lwd = 2)

#ACF PACF TRAIN
plot(trainIDR, type = "l", lwd = 3,main = "Plot Training Set Saham IDR")
acf(trainIDR)
pacf(trainIDR)

#BOXCOX
b = BoxCox.lambda(trainIDR)
b
trainIDRbox = trainIDR^-1
trainIDRbox
plot(trainIDRbox, type = "l", lwd = 3, main = "Plot Transformasi Saham IDR")

#ARIMA TRAINING
ts.plot(trainIDRbox)
acf(trainIDRbox)
pacf(trainIDRbox)

#DIF 1
dif1train = diff(trainIDRbox, lag =1, differences = 1)
plot(dif1train, type = "l", lwd = 3, main = "Plot Differencing 1")
abline(h=mean(dif1train), lwd = 3, lty = 2, col = 'red')
acf(dif1train)
pacf(dif1train)

#DIF 2
dif2train = diff(trainIDRbox, lag =1, differences = 2)
plot(dif2train, type = "l", lwd = 3, main = "Plot Differencing 2")
abline(h=mean(dif2train), lwd = 3, lty = 2, col = 'red')
acf(dif2train)
pacf(dif2train)

#MODEL DIF 2
modelarima021 = arima(trainIDRbox, order = c(0,2,1), method = "ML")
summary(modelarima021)
coeftest(modelarima021)
modelarima120 = arima(trainIDRbox, order = c(1,2,0), method = "ML")
summary(modelarima120)
coeftest(modelarima120)
modelarima220 = arima(trainIDRbox, order = c(2,2,0), method = "ML")
summary(modelarima220)
coeftest(modelarima220)
modelarima121 = arima(trainIDRbox, order = c(1,2,1), method = "ML")
summary(modelarima121)
coeftest(modelarima121)
modelarima221 = arima(trainIDRbox, order = c(2,2,1), method = "ML")
summary(modelarima221)
coeftest(modelarima221)
#(021) (120) (220) BAIK

#Chisquare
qchisq(0.95,11)
qchisq(0.95,23)
qchisq(0.95,35)
qchisq(0.95,48)

#MODEL 021
hist(modelarima021$residuals, prob = TRUE, main = "Histogram Residu ARIMA (0,2,1)") #tambah garis
x2 = seq(min(modelarima021$residuals),max(modelarima021$residuals), length = 40)
fun = dnorm(x2,mean = mean(modelarima021$residuals), sd = sd(modelarima021$residuals))
lines(x2,fun, lwd = 3, col = "red")
qqnorm(modelarima021$residuals, main='Plot Residu ARIMA (0,2,1)')
qqline(modelarima021$residuals)
ks.test(modelarima021$residuals,'pnorm',mean(modelarima021$residuals),sd(modelarima021$residuals))

Box.test(modelarima021$residuals,lag=12, type = "Ljung-Box")
Box.test(modelarima021$residuals,lag=24, type = "Ljung-Box")
Box.test(modelarima021$residuals,lag=36, type = "Ljung-Box")
Box.test(modelarima021$residuals,lag=48, type = "Ljung-Box")

#MODEL 120
hist(modelarima120$residuals, prob = TRUE, main = "Histogram Residu ARIMA (1,2,0)") #tambah garis
x2 = seq(min(modelarima120$residuals),max(modelarima120$residuals), length = 40)
fun = dnorm(x2,mean = mean(modelarima120$residuals), sd = sd(modelarima120$residuals))
lines(x2,fun, lwd = 3, col = "red")
qqnorm(modelarima120$residuals, main='Plot Residu ARIMA (0,2,1)')
qqline(modelarima120$residuals)
ks.test(modelarima120$residuals,'pnorm',mean(modelarima120$residuals),sd(modelarima120$residuals))

Box.test(modelarima120$residuals,lag=12, type = "Ljung-Box")
Box.test(modelarima120$residuals,lag=24, type = "Ljung-Box")
Box.test(modelarima120$residuals,lag=36, type = "Ljung-Box")
Box.test(modelarima120$residuals,lag=48, type = "Ljung-Box")

#MODEL 220
hist(modelarima220$residuals, prob = TRUE) #tambah garis
x2 = seq(min(modelarima220$residuals),max(modelarima220$residuals), length = 40)
fun = dnorm(x2,mean = mean(modelarima220$residuals), sd = sd(modelarima220$residuals))
lines(x2,fun, lwd = 3, col = "red")
qqnorm(modelarima220$residuals, main='Normal')
qqline(modelarima220$residuals)
ks.test(modelarima220$residuals,'pnorm',mean(modelarima220$residuals),sd(modelarima220$residuals))

Box.test(modelarima220$residuals,lag=12, type = "Ljung-Box")
Box.test(modelarima220$residuals,lag=24, type = "Ljung-Box")
Box.test(modelarima220$residuals,lag=36, type = "Ljung-Box")
Box.test(modelarima220$residuals,lag=48, type = "Ljung-Box")

#Rumus residual
rmse <- function(error) {
  sqrt(mean(error^2))
}

mape <- function(actual_values, predicted_values) {
  mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
  return(mape)
}

#RESIDUAL TRAIN 021
model021train = 
  rmsetrain021= sqrt(mean((trainIDR-(fitted(modelarima021))^-1)^2))
rmsetrain021
mapetrain021= mean(abs((trainIDR-(fitted(modelarima021))^-1)/trainIDR))*100
mapetrain021

#RESIDUAL TRAIN 120
rmsetrain120= sqrt(mean((trainIDR-(fitted(modelarima120))^-1)^2))
rmsetrain120
mapetrain120= mean(abs((trainIDR-(fitted(modelarima120))^-1)/trainIDR))*100
mapetrain120

#Plot Training ARIMA 021
plot(trainIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi Data Training', ylim = c(14000, 15950))
lines(1:84,fitted(modelarima021)^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Training","ARIMA (0,2,1)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

#Plot Training ARIMA 12O
plot(trainIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi Data Training', ylim = c(14000, 15950))
lines(1:84,fitted(modelarima120)^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Training","ARIMA (1,2,0)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

#PREDIKSI ARIMA 021
rmsetest021 = sqrt(mean((testIDR-modelarima021foretest^-1)^2))
rmsetest021
mapetest021 = mean(abs((testIDR-modelarima021foretest^-1)/testIDR))*100
mapetest021

modelarima021foretest=predict(modelarima021,21)$pred
modelarima021foretest
modelarima021foretest^-1
MAPE(modelarima021foretest^-1,testIDR)*100
RMSE(modelarima021foretest^-1,testIDR)

#PREDIKSI ARIMA 120
rmsetest120 = sqrt(mean((testIDR-modelarima120foretest^-1)^2))
rmsetest120
mapetest120 = mean(abs((testIDR-modelarima120foretest^-1)/testIDR))*100
mapetest120

modelarima120foretest=predict(modelarima120,21)$pred
modelarima120foretest
modelarima120foretest^-1
MAPE(modelarima120foretest^-1,testIDR)*100
RMSE(modelarima120foretest^-1,testIDR)

#Plot Testing ARIMA 021
plot(testIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi', ylim = c(14700, 15950))
lines(1:21,modelarima021foretest^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Testing","ARIMA (0,2,1)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

#Plot Testingn ARIMA 120
plot(testIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi', ylim = c(14700, 15650))
lines(1:21,modelarima120foretest^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Testing","ARIMA (0,2,1)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

## METODE SVR

setwd("D:/KULIAH/SKRIPSI")
data<-import("week2.xlsx")

View(data)

data<-data[2:105, 2:3];
win.graph()
ts.plot(data)
train<-data[1:84,]
test<-data[-1:-84,]

pacf(data$value) #Lag signifikan pada Lag 1, maka input Lagnya Xt-1

#membuat model svm
model1 <- svm(value ~ yt_1, train, kernel="polynomial", ranges = list(cost = 2^(0:9), epsilon = seq(0, 1, 0.1), gamma = 2^(-3:3)))
model2 <- svm(value ~ yt_1, train, kernel="radial", ranges = list(cost = 2^(0:9), epsilon = seq(0, 1, 0.1), gamma = 2^(-3:3)))

#
wSV = model1$coefs
wSV
twSV = t(model1$coefs)
twSV

w = t(model1$coefs)%*%model1$SV
w
b = model1$rho
b
dim(w)

cat("Weight vector (w):", w, "\n")

model2$coefs

w = t(model2$coefs)%*%model2$SV
w
b = model2$rho
b

#residual
rmse <- function(error) {
  sqrt(mean(error^2))
}

mape <- function(actual_values, predicted_values) {
  mape <- mean(abs((actual_values - predicted_values) / actual_values)) * 100
  return(mape)
}

#tuning
tuneResult1 <- tune(svm, value ~ .,  data = train, kernel = "polynomial", ranges = list(cost = 2^(0:9), epsilon = seq(0, 1, 0.1), gamma = 2^(-3:3),degree = 2:5), iter.max = 10)
tuneResult2 <- tune(svm, value ~ .,  data = train, kernel="radial", ranges = list(cost = 2^(0:9), epsilon = seq(0, 1, 0.1), gamma = 2^(-3:3)))

print(tuneResult1)
print(tuneResult2)

tunedModel1 <- tuneResult1$best.model #Polynomial
tunedModel2 <- tuneResult2$best.model #RBF

#Train Tunning Polynomial
tunedModelTrain1 <- predict(tunedModel1, train)
tunedModelTrain1
errorTrain1 <- train$value-tunedModelTrain1
tunedModelRMSETrain1 <- rmse(errorTrain1);tunedModelRMSETrain1 
tunedmodelmapetrain1 <- mape(train$value, tunedModelTrain1)
tunedmodelmapetrain1

#Train Tunning Radial
tunedModelTrain2 <- predict(tunedModel2, train)
tunedModelTrain2
errorTrain2 <- train$value-tunedModelTrain2
tunedModelRMSETrain2 <- rmse(errorTrain2);tunedModelRMSETrain2 
tunedmodelmapetrain2 <- mape(train$value, tunedModelTrain2)
tunedmodelmapetrain2

#Plot Tuning Polynom
plot(train$value, type = 'l', main = "Train Vs. Fit SVR Tuned Polynomial", lwd = 3, ylim = c(14000,16000))
lines(tunedModelTrain1, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR Polynomial"), col = c("black","red"), lty = c(1,2), lwd = 3)

#Plot Tuning RBF
plot(train$value, type = 'l', main = "Train Vs. Fit SVR Tuned RBF", lwd = 3)
lines(tunedModelTrain2, col='red', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR RBF"), col = c("black","red"), lty = c(1,2), lwd = 3)

#Test Tunning Test
tunedModelTest1 <- predict(tunedModel1, test)
tunedModelTest1
errorTest1 <- test$value-tunedModelTest1
tunedModelRMSETest1 <- rmse(errorTest1);tunedModelRMSETest1
tunemapetest1 <- mape(test$value, tunedModelTest1)
tunemapetest1

#Test Tunning Test
tunedModelTest2 <- predict(tunedModel2, test)
tunedModelTest2
errorTest2 <- test$value-tunedModelTest2
tunedModelRMSETest2 <- rmse(errorTest2);tunedModelRMSETest2
tunemapetest2 <- mape(test$value, tunedModelTest2)
tunemapetest2

#Plot Tuning Test
plot(test$value, type = 'l', main = "Train Vs. Fit SVR Tuned Polynomial", lwd = 3, ylim = c(14600,15800))
lines(tunedModelTest1, col='green', lty="dashed", lwd = 3)
lines(tunedModelTest2, col='blue', lty="dashed", lwd = 3)
legend("topright", legend = c("Train","SVR RBF","SVR Polynomial"), col = c("black","green","blue"), lty = c(1,2), lwd = 3)

