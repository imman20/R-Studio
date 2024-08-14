library(readxl)
library(e1071)
library(MLmetrics)
library(tseries)
library(forecast)
library(lmtest)
library(zoo)
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

# Install and load the openxlsx package
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

# Replace "output_file.xlsx" with the desired file name and path
output_file <- "D://KULIAH/SKRIPSI/Data/Data TESTING ARIMA 2.xlsx"

# Convert trainIDRbox to a data frame if it's not already
trainIDRbox_df <- as.data.frame(modelarima021foretest^-1)

# Write the data frame to Excel
write.xlsx(trainIDRbox_df, file = output_file, rowNames = FALSE)

# Print a message indicating successful export
cat("Data has been exported to", output_file, "\n")


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

qchisq(0.95,11)
qchisq(0.95,23)
qchisq(0.95,35)
qchisq(0.95,48)

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

#residual
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

plot(trainIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi Data Training', ylim = c(14000, 15950))
lines(1:84,fitted(modelarima021)^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Training","ARIMA (0,2,1)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

plot(trainIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi Data Training', ylim = c(14000, 15950))
lines(1:84,fitted(modelarima120)^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Training","ARIMA (1,2,0)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

#PREDIKSI ARIMA
rmsetest021 = sqrt(mean((testIDR-modelarima021foretest^-1)^2))
rmsetest021
mapetest021 = mean(abs((testIDR-modelarima021foretest^-1)/testIDR))*100
mapetest021

modelarima021foretest=predict(modelarima021,21)$pred
modelarima021foretest
modelarima021foretest^-1
MAPE(modelarima021foretest^-1,testIDR)*100
RMSE(modelarima021foretest^-1,testIDR)

#PREDIKSI ARIMA
rmsetest120 = sqrt(mean((testIDR-modelarima120foretest^-1)^2))
rmsetest120
mapetest120 = mean(abs((testIDR-modelarima120foretest^-1)/testIDR))*100
mapetest120

modelarima120foretest=predict(modelarima120,21)$pred
modelarima120foretest
modelarima120foretest^-1
MAPE(modelarima120foretest^-1,testIDR)*100
RMSE(modelarima120foretest^-1,testIDR)


plot(testIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi', ylim = c(14700, 15950))
lines(1:21,modelarima021foretest^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Testing","ARIMA (0,2,1)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

plot(testIDR, type = "o",col="black",lwd=3, main = 'Plot Perbandingan Hasil Prediksi', ylim = c(14700, 15650))
lines(1:21,modelarima120foretest^-1, col = "red",pch=4,lwd=3, type='o')
legend("topright",
       legend = c("Data Testing","ARIMA (0,2,1)"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

### SVR
#SVR
pacf(IDR) #Lag signifikan pada Lag 1, maka input Lagnya Xt-1

#MATRIX FULL X Waktu 0 Termasuk
matrixfull = matrix(0,174,2)
matrixfull[,2] = IDR[2:175]
matrixfull[,1] = seq(1,174)
colnames(matrixfull) = c("X","Y")
matrixfull

#X WAKTU
matrixtrain = matrix(0,160,2)
matrixtrain[,2] = matrix(trainIDR)
matrixtrain[,1] = seq(0,159)
colnames(matrixtrain) = c("X","Y")
head(matrixtrain)
tail(matrixtrain)

matrixtest = matrix(0,15,2)
matrixtest[,2] = matrix(testIDR)
matrixtest[,1] = seq(161,175)
colnames(matrixtest) = c("X","Y")
head(matrixtest)
tail(matrixtest)

#MATRIX FULL X Waktu 0 Tidak termasuk
matrixfull = matrix(0,174,2)
matrixfull[,2] = IDR[2:175]
matrixfull[,1] = seq(1,174)
colnames(matrixfull) = c("X","Y")
matrixfull

#X WAKTU
matrixtrain = matrixfull[1:160,]
colnames(matrixtrain) = c("X","Y")
head(matrixtrain)
tail(matrixtrain)

matrixtest = matrixfull[160:174,]
matrixtest[,2] = matrix(testIDR)
matrixtest[,1] = seq(160,174)
colnames(matrixtest) = c("X","Y")
head(matrixtest)
tail(matrixtest)

matrixtest = matrixfull[160:174,]
colnames(matrixtest) = c("X","Y")
head(matrixtest)
tail(matrixtest)

#
df <- data.frame(IDR, dplyr::lag(IDR, n = 1), dplyr::lag(IDR, n = 5))
colnames(df) <- c("yt", "yt-1", "yt-5")
df.train <- df[6:160,]

df.test <- df[-(6:160),]
df.test <- df[161:175,]

svm.linear <- svm(yt ~ ., data = df.train, kernel = "linear")
summary(svm.linear)

fit.svm.linear.train <- predict(svm.linear, df.train)

# Root Mean Square Error & MAPE ####
rmse <- function(err) {
  rootmse <- sqrt(mean(err^2))
  return(rootmse)
}

mape <- function(actual_values, predicted_values) {
  n <- length(actual_values)
  mape <- sum(abs((actual_values - predicted_values) / actual_values)) / n * 100
  return(mape)
}

residual.train.svm.linear <- df.train$yt - fit.svm.linear.train

rmse.train.svm.linear <- rmse(residual.train.svm.linear)

(mape.train.svm.linear <- mape(df.train$yt, fit.svm.linear.train))

# Tuning ####
# Loose Grid
svm.tune.linear <- tune(svm, yt ~ .,  data = df.train, kernel = "linear", ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(1:9)))
print(svm.tune.linear)

win.graph()
plot(svm.tune.linear, main = "Tuning Result SVR Linear")

# Finer Grid
svm.tune.linear.2 <- tune(svm, yt ~ .,  data = df.train, kernel = "linear", ranges = list(epsilon = seq(0, 0.1, 0.01), cost = 2^(5:9)))
print(svm.tune.linear.2)

#membuat sebuah prediksi untuk masing-masing data training (FIT)
prediksiY <- predict(model,df.train)
#residual
rmse <- function(error)
{
  sqrt(mean(error^2))
}
#
resTrain<-train$sunspotarea-prediksiY
RMSEtrain<-rmse(resTrain);RMSEtrain

## BARU
data<-read_excel("IDR.xlsx");View(data)
data<-data[4:175,2:5];View(data)
win.graph()
ts.plot(data$sunspotarea)
train<-data[1:127,];View(train)
test<-data[-1:-127,];View(test)

#membuat model svm
model1 <- svm(Close~yt_1+yt_2+yt_3, trainIDR)
model2 <- svm(Close~., trainIDR)

#membuat sebuah prediksi untuk masing-masing data training (FIT)
prediksiY <- predict(model,train)
#residual
rmse <- function(error)
{
  sqrt(mean(error^2))
}
#
resTrain<-train$sunspotarea-prediksiY
RMSEtrain<-rmse(resTrain);RMSEtrain

#plot fit vs train
win.graph()
par(mfrow=c(2,1))
ts.plot(train$sunspotarea)
ts.plot(prediksiY)

#Testing (out-sample)
prediksiTest<-predict(model,test)
resTest<-test$sunspotarea-prediksiTest
RMSETest<-rmse(resTest);RMSETest

#plot
win.graph()
par(mfrow=c(2,1))
ts.plot(test$sunspotarea)
ts.plot(prediksiTest)

#tuning
tuneResult <- tune(svm, sunspotarea~.,  data = train,
                   ranges = list(epsilon = seq(0,0.4,0.01), cost = 2^(1:9))
)
print(tuneResult)
# menggambarkan hasil tuning
win.graph()
plot(tuneResult)

tunedModel <- tuneResult$best.model

#Train Tunning
tunedModelTrain <- predict(tunedModel, train)
errorTrain <- train$sunspotarea-tunedModelTrain
tunedModelRMSETrain <- rmse(errorTrain);tunedModelRMSETrain 

#Test Tunning
tunedModelTest <- predict(tunedModel, test)
errorTest <- test$sunspotarea-tunedModelTest
tunedModelRMSETest <- rmse(errorTest);tunedModelRMSETest

#plot
win.graph()
par(mfrow=c(2,1))
ts.plot(train$sunspotarea)
ts.plot(tunedModelTrain)

#plot
win.graph()
par(mfrow=c(2,1))
ts.plot(test$sunspotarea)
ts.plot(tunedModelTest)


# Mencari SVR Training nilai Cost Terbaik
## Cost 0.01
modelsvrtraininglinear_0.01 = svm(formula = Y~X,
                             data = matrixtrain,
                             kernel = "linear",
                             ranges = list(epsilon = seq(0, 1, 0.1), cost = 2^(2:9)))
win.graph()
plot(modelsvrtraininglinear_0.01, main = "Tuning Result SVR Linear")

modelsvrtraininglinear_0.01
prediksisvrtraininglinear_0.01 = predict(modelsvrtraininglinear_0.01,matrixtrain)
prediksisvrtraininglinear_0.01
mapetrainlinear = MAPE(prediksisvrtraininglinear_0.01,matrixtrain[,2])
mapetrainlinear*100
rmsetrainlinear = RMSE(prediksisvrtraininglinear_0.01,matrixtrain[,2])
rmsetrainlinear

## Cost 0.1
modelsvrtraininglinear_0.1 = svm(formula = Y~X,
                                  data = matrixtrain,
                                  kernel = "linear",
                                  cost=0.1)
modelsvrtraininglinear_0.1
prediksisvrtraininglinear_0.1 = predict(modelsvrtraininglinear_0.1,matrixtrain)
prediksisvrtraininglinear_0.1
mapetrainlinear = MAPE(prediksisvrtraininglinear_0.1,matrixtrain[,2])
mapetrainlinear*100
rmsetrainlinear = RMSE(prediksisvrtraininglinear_0.1,matrixtrain[,2])
rmsetrainlinear

## Cost 1
modelsvrtraininglinear_1 = svm(formula = Y~X,
                                 data = matrixtrain,
                                 kernel = "linear",
                                 cost=1)
modelsvrtraininglinear_1
prediksisvrtraininglinear_1 = predict(modelsvrtraininglinear_1,matrixtrain)
prediksisvrtraininglinear_1
mapetrainlinear = MAPE(prediksisvrtraininglinear_1,matrixtrain[,2])
mapetrainlinear*100
rmsetrainlinear = RMSE(prediksisvrtraininglinear_1,matrixtrain[,2])
rmsetrainlinear

## Cost 10
modelsvrtraininglinear_10 = svm(formula = Y~X,
                               data = matrixtrain,
                               kernel = "linear",
                               cost=10)
modelsvrtraininglinear_10
prediksisvrtraininglinear_10 = predict(modelsvrtraininglinear_10,matrixtrain)
prediksisvrtraininglinear_10
mapetrainlinear = MAPE(prediksisvrtraininglinear_10,matrixtrain[,2])
mapetrainlinear*100
rmsetrainlinear = RMSE(prediksisvrtraininglinear_10,matrixtrain[,2])
rmsetrainlinear

# Model Terbaik Cost 1


#####
#Membuat PLOT SVR Model Terbaik
plot(matrixtrain, type = "l",col="black", lwd = 3, main = "Train Linear")
lines(prediksisvrtraininglinear_1, col = "red",pch=4, lwd = 3)
legend("topright",
       legend = c("Data Training","SVR Training"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

# Mencari  SVR Test Cost Terbaik
## Cost 0.01
testsvrlinear_0.01 = svm(formula = Y~X,
                    data = matrixtrain,
                    kernel = "linear",
                    cost = 0.01)
prediksisvrtestlinear_0.01 = predict(testsvrlinear_0.01, matrixtest) 
prediksisvrtestlinear_0.01
mapetestlinear = MAPE(prediksisvrtestlinear_0.01,matrixtest[,2])
mapetestlinear*100
rmsetestlinear = RMSE(prediksisvrtestlinear_0.01,matrixtest[,2])
rmsetestlinear


## Cost 0.1
testsvrlinear_0.1 = svm(formula = Y~X,
                         data = matrixtrain,
                         kernel = "linear",
                         cost = 0.1)
prediksisvrtestlinear_0.1 = predict(testsvrlinear_0.1, matrixtest) 
prediksisvrtestlinear_0.1
mapetestlinear = MAPE(prediksisvrtestlinear_0.1,matrixtest[,2])
mapetestlinear*100
rmsetestlinear = RMSE(prediksisvrtestlinear_0.1,matrixtest[,2])
rmsetestlinear

## Cost 1
testsvrlinear_1 = svm(formula = Y~X,
                        data = matrixtrain,
                        kernel = "linear",
                        cost = 1)
prediksisvrtestlinear_1 = predict(testsvrlinear_1, matrixtest) 
prediksisvrtestlinear_1
mapetestlinear = MAPE(prediksisvrtestlinear_1,matrixtest[,2])
mapetestlinear*100
rmsetestlinear = RMSE(prediksisvrtestlinear_1,matrixtest[,2])
rmsetestlinear

## Cost 10
testsvrlinear_10 = svm(formula = Y~X,
                      data = matrixtrain,
                      kernel = "linear",
                      cost = 10)
prediksisvrtestlinear_10 = predict(testsvrlinear_10, matrixtest) 
prediksisvrtestlinear_10
mapetestlinear = MAPE(prediksisvrtestlinear_10,matrixtest[,2])
mapetestlinear*100
rmsetestlinear = RMSE(prediksisvrtestlinear_10,matrixtest[,2])
rmsetestlinear

# Model Terbaik Cost 1
# Plot SVR TEST
#SVR TEST Linear Cost 1
plot(matrixtest, type = "o",col="black",lwd=3, ylim = c(14800,15400))
lines(160:174,prediksisvrtestlinear_1, col = "red",pch=4,lwd=3, type = 'o')
legend("topright",
       legend = c("Data Test","SVR Testing"),
       col = c("black","red"),
       lty = c(1,1),
       lwd = 3)

#PLOT PENUH kernel Linear
plot(IDR, type = "l",col="black",lwd=3, ylim = c(14500,15600), main = "Plot penuh Kernel Linear")
lines(IDR[1:160], col = "black",pch=4,lwd=3)
lines(161:175,IDR[161:175], col = "navyblue",pch=4,lwd=3)
lines(prediksisvrtraininglinear_1, col = "pink",pch=4,lwd=3)
lines(161:175,prediksisvrtestlinear_1, col = "red",pch=4,lwd=3)
abline(v=161, lwd = 3, lty = 2) # PEMBATAS TESTING TRAINING
legend("bottomleft",
       legend = c("Data Training","Data Testing",
                  "SVR Training","SVR Testing"),
       col = c("black","navyblue",
                      "pink","red"),
                      lty = c(1,1,1,1,1),
       lwd = 3)

#PLOT TRAINING
plot(trainIDR, type = "l",col="black",lwd=3,ylim = c(14600,15700))
lines(2:161,prediksisvrtraininglinear_1, col = "green",pch=4,lwd=3)
lines(fitted(modelarima021)^-1, col = "blue",pch=4,lwd=3)
legend("bottomleft",
       legend = c("Data Training","SVR Linear"
                  ,"ARIMA (0,2,1)"),
       col = c("black","green","blue"),
       lty = c(1,1,1),
       lwd = 3)

#PLOT TESTING
plot(testIDR, type = "o",col="black",lwd=3,ylim = c(14600,15700))
lines(1:15,prediksisvrtestlinear_1, col = "green",pch=4,lwd=3, type = "o")
lines(1:15,modelarima021foretest^-1, col = "blue",pch=4,lwd=3, type = "o")
legend("bottomleft",
       legend = c("Data Testing","SVR Linear"
                  ,"ARIMA (0,2,1)"),
       col = c("black","green","blue"),
       lty = c(1,1,1),
       lwd = 3)


#TABEL PERBANDINGAN TRAINING TESTING
tabelperbandinganfull=function()
{
  matrixperbandingan = matrix(0,2,4,
                              dimnames = list(c("Training","Testing"),
                                              c("MAPE ARIMA(0,2,1)"
                                                ,"MAPE SVR LINEAR"
                                                ,"RMSE ARIMA(0,2,1)"
                                                ,"RMSE SVR LINEAR")))
  matrixperbandingan[1,1] = round(MAPE(fitted(modelarima021)^-1,trainIDR)*100,2)
  matrixperbandingan[1,2] = round(MAPE(prediksisvrtraininglinear_1,matrixtrain[,2])*100,2)
  matrixperbandingan[1,3] = round(RMSE(fitted(modelarima021)^-1,trainIDR))
  matrixperbandingan[1,4] = round(RMSE(prediksisvrtraininglinear_1,matrixtrain[,2]))
  
  matrixperbandingan[2,1] = round(MAPE(modelarima021foretest^-1,testIDR)*100,2)
  matrixperbandingan[2,2] = round(MAPE(prediksisvrtestlinear_1,matrixtest[,2])*100,2)
  matrixperbandingan[2,3] = round(RMSE(modelarima021foretest^-1,testIDR))
  matrixperbandingan[2,4] = round(RMSE(prediksisvrtestlinear_1,matrixtest[,2]))
  
  cat("\n\t\tTabel Perbandingan Prediksi ARIMA 0,2,1 dan SVR Linear\n")
  matrixperbandingan
}
tabelperbandinganfull()