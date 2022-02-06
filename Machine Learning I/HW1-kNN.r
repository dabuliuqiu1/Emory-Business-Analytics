### Q1

AllData <- read.table("Desktop/ML 1/HW1/GradedHW1-All-Data.csv",header=T,sep=",",
                      stringsAsFactors = F,na.strings="")
View(AllData)
typeof(AllData)
class(AllData)

AllData <- AllData[AllData$Bldg.Type=="1Fam",]
#View(AllData)

RPerm <- sample(nrow(AllData))
RPerm
AllData <- AllData[RPerm,]

nrow(AllData)
TrainInd <- ceiling(nrow(AllData)/2)
TrainInd
ValInd <- ceiling((nrow(AllData)-TrainInd)/2)+TrainInd
ValInd

TrainData <- AllData[1:TrainInd,]
ValData <- AllData[(TrainInd+1):ValInd,]
TestData <- AllData[(ValInd+1):nrow(AllData),]
#View(TrainData)
#View(ValData)
#View(TestData)

#########################################################


# Q2
library(readr)
train_grade <- read_csv("/Users/huangzm/Desktop/ML 1/HW1/GradedHW1-Train-Data.csv")
val_grade <- read_csv("/Users/huangzm/Desktop/ML 1/HW1/GradedHW1-Validation-Data.csv")
test_grade <- read_csv("/Users/huangzm/Desktop/ML 1/HW1/GradedHW1-Test-Data.csv")

variables = c('Lot.Area',
              'Total.Bsmt.SF',
              'Gr.Liv.Area',
              'Full.Bath',
              'Bedroom.AbvGr',
              'Year.Built',
              'SalePrice')
train_grade = train_grade[,variables]
val_grade = val_grade[,variables]
test_grade = test_grade[,variables]

train_grade$Bld.Age = 2010 - train_grade$Year.Built
val_grade$Bld.Age = 2010 - val_grade$Year.Built
test_grade$Bld.Age = 2010 - test_grade$Year.Built

train_grade = train_grade[,-c(6)]
val_grade = val_grade[,-c(6)]
test_grade = test_grade[,-c(6)]

#View(val_grade)

library(dplyr)
check_na = function(df){
  na_result = matrix(ncol=ncol(df),nrow=2)
  for (i in 1:ncol(df)){
    na_result[1,i] = colnames(df)[i]
    na_result[2,i] = nrow(df[,i] %>% filter(is.na(.))>0)
  }
  return(na_result)
}
check_na(train_grade)
check_na(val_grade)
check_na(test_grade)

val_grade = na.omit(val_grade)

hist(train_grade$Lot.Area, breaks=seq(min(train_grade$Lot.Area), max(train_grade$Lot.Area), length.out = 10000))
hist(train_grade$Total.Bsmt.SF, breaks=seq(0,7000,100))
hist(train_grade$Gr.Liv.Area,breaks=seq(0,7000,100))
hist(train_grade$Full.Bath)
hist(train_grade$Bedroom.AbvGr)
hist(train_grade$Bld.Age, breaks=seq(0,140,1))
hist(train_grade$SalePrice,breaks=seq(0,1000000,10000))

library(psych)
describe(train_grade)


################################################################

# Q3 SIMPLE
suppressWarnings(if(!require("FNN")) { install.packages("FNN"); require("FNN") })

MSE = rep(NA,40)
K_list = rep(NA,40)
for (i in 1:40){
  out = knn.reg(train=train_grade[,-c(6)],test=val_grade[,-c(6)],y=train_grade$SalePrice,k=i)
  ypred = matrix(out$pred)
  rmse = sqrt(mean((val_grade$SalePrice - ypred) ^ 2))
  MSE[i] = rmse
  K_list[i] = i
}

library(ggplot2)
plot_data = data.frame(K_list, MSE)
ggplot(plot_data,aes(K_list,MSE)) + geom_line() + 
  scale_x_continuous("k", breaks = seq(0,40,5)) +
  scale_y_continuous("Root MSE", breaks = seq(0,100000,2000)) + 
  labs(title = "Root MSE vs K (Raw, Unstandardized)") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15, face="bold",hjust=0.5)) +
  geom_point(data=plot_data,aes(K_list,MSE)) 

MSE[1]
MSE[20]
which(MSE==min(MSE))
min(MSE)

####################################################################

#Q4
best_out = knn.reg(train=train_grade[,-c(6)],test=test_grade[,-c(6)]
                   ,y=train_grade$SalePrice,k=12)
y_test_pred = matrix(best_out$pred)
rmse_best = sqrt(mean((test_grade$SalePrice - y_test_pred) ^ 2))
rmse_best

####################################################################

#Q5 STANDARDIZED

library(caret)
normParam <- preProcess(train_grade[,-c(6)], method = c("center", "scale"))
scaled_train = cbind(data.frame(predict(normParam,train_grade[,-c(6)])),train_grade[,c(6)])
scaled_val = cbind(data.frame(predict(normParam,val_grade[,-c(6)])),val_grade[,c(6)])
scaled_test = cbind(data.frame(predict(normParam,test_grade[,-c(6)])),test_grade[,c(6)])

#describe(scaled_val)
#View(scaled_val)

MSE1 = rep(NA,40)
K_list1 = rep(NA,40)
for (i in 1:40){
  out1 = knn.reg(train=scaled_train[,-c(7)],test=scaled_val[,-c(7)],y=scaled_train$SalePrice,k=i)
  ypred1 = matrix(out1$pred)
  rmse1 = sqrt(mean((scaled_val$SalePrice - ypred1) ^ 2))
  MSE1[i] = rmse1
  K_list1[i] = i
}

plot_data1 = data.frame(K_list1, MSE1)
ggplot(plot_data1,aes(K_list1,MSE1)) + geom_line() + 
  scale_x_continuous("k", breaks = seq(0,40,5)) +
  scale_y_continuous("Root MSE",breaks = seq(0,100000,2000)) + 
  labs(title = "Root MSE vs K (Raw, Standardized)") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15, face="bold",hjust=0.5)) +
  geom_point(data=plot_data1,aes(K_list1,MSE1)) 
MSE1[1]
MSE1[20]
which(MSE1==min(MSE1))

####################################################################

#Q6
best_out1 = knn.reg(train=scaled_train[,-c(7)],test=scaled_test[,-c(7)],
                    y=scaled_train$SalePrice,k=12)
y_test_pred1 <- matrix(best_out1$pred)
rmse_best1 = sqrt(mean((scaled_test$SalePrice - y_test_pred1) ^ 2))
rmse_best1

####################################################################

#Q7 TRANSFORMED

trans_train = train_grade 
trans_val = val_grade 
trans_test = test_grade

trans_train$Lot.Area = log(trans_train$Lot.Area+0.0000000000001)
trans_val$Lot.Area = log(trans_val$Lot.Area+0.0000000000001)
trans_test$Lot.Area = log(trans_test$Lot.Area+0.0000000000001)

trans_train$Total.Bsmt.SF = sqrt(trans_train$Total.Bsmt.SF)
trans_val$Total.Bsmt.SF = sqrt(trans_val$Total.Bsmt.SF)
trans_test$Total.Bsmt.SF = sqrt(trans_test$Total.Bsmt.SF)

trans_train$Gr.Liv.Area = log(trans_train$Gr.Liv.Area+0.0000000000001)
trans_val$Gr.Liv.Area = log(trans_val$Gr.Liv.Area+0.0000000000001)
trans_test$Gr.Liv.Area = log(trans_test$Gr.Liv.Area+0.0000000000001)

trans_train$SalePrice = log(trans_train$SalePrice+0.0000000000001)
trans_val$SalePrice = log(trans_val$SalePrice+0.0000000000001)
trans_test$SalePrice = log(trans_test$SalePrice+0.0000000000001)

#View(trans_train)
'''
library(psych)
describe(train_grade)
skew(log(trans_train$Lot.Area+0.0000000000001)) ####
skew((trans_train$Lot.Area)^(1/2))
skew((trans_train$Lot.Area)^(1/3))

skew(log(trans_train$Total.Bsmt.SF+0.0000000000001))
skew((trans_train$Total.Bsmt.SF)^(1/2)) ####
skew((trans_train$Total.Bsmt.SF)^(1/3))

skew(log(trans_train$Gr.Liv.Area+0.0000000000001)) ####
skew((trans_train$Gr.Liv.Area)^(1/2)) 
skew((trans_train$Gr.Liv.Area)^(1/3))

skew(log(trans_train$SalePrice+0.0000000000001)) ####
skew((trans_train$SalePrice)^(1/2)) 
skew((trans_train$SalePrice)^(1/3))
'''

#View(trans_test)

MSE2 = rep(NA,40)
K_list2 = rep(NA,40)
for (i in 1:40){
  out2 = knn.reg(train=trans_train[,-c(6)],test=trans_val[,-c(6)],y=trans_train$SalePrice,k=i)
  ypred2 = exp(matrix(out2$pred))
  rmse2 = sqrt(mean((exp(trans_val$SalePrice) - ypred2) ^ 2))
  MSE2[i] = rmse2
  K_list2[i] = i
}

plot_data2 = data.frame(K_list2, MSE2)
ggplot(plot_data2,aes(K_list2,MSE2)) + geom_line() + 
  scale_x_continuous("k", breaks = seq(0,40,5)) +
  scale_y_continuous("Root MSE", breaks = seq(0,100000,2000)) + 
  labs(title = "Root MSE vs K (Transformed, Unstandardized)") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15, face="bold",hjust=0.5)) +
  geom_point(data=plot_data2,aes(K_list2,MSE2)) 

####################################################################

#Q8
MSE2[1]
MSE2[20]
which(MSE2==min(MSE2))
min(MSE2)

best_out2 = knn.reg(train=trans_train[,-c(6)],test=trans_test[,-c(6)],
                    y=trans_train$SalePrice,k=2)
y_test_pred2 = exp(matrix(best_out2$pred))
rmse_best2 = sqrt(mean((exp(trans_test$SalePrice) - y_test_pred2) ^ 2))
rmse_best2

####################################################################

#Q9 STANDARDIZED & TRANSFORMED

normParam <- preProcess(trans_train[,-c(6)], method = c("center", "scale"))
scaled_trans_train = cbind(data.frame(predict(normParam,trans_train[,-c(6)])),trans_train[,c(6)])
scaled_trans_val = cbind(data.frame(predict(normParam,trans_val[,-c(6)])),trans_val[,c(6)])
scaled_trans_test = cbind(data.frame(predict(normParam,trans_test[,-c(6)])),trans_test[,c(6)])
View(scaled_trans_test)

MSE3 = rep(NA,40)
K_list3 = rep(NA,40)
for (i in 1:40){
  out3 = knn.reg(train=scaled_trans_train[,-c(7)],test=scaled_trans_val[,-c(7)],y=scaled_trans_train[,c(7)],k=i)
  ypred3 = exp(matrix(out3$pred))
  rmse3 = sqrt(mean((exp(scaled_trans_val[,c(7)]) - ypred3) ^ 2))
  MSE3[i] = rmse3
  K_list3[i] = i
}

plot_data3 = data.frame(K_list3, MSE3)
ggplot(plot_data3,aes(K_list3,MSE3)) + geom_line() + 
  scale_x_continuous("k", breaks = seq(0,40,5)) +
  scale_y_continuous("Root MSE", breaks = seq(0,100000,2000)) + 
  labs(title = "Root MSE vs K (Transformed, Standardized)") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15, face="bold",hjust=0.5)) +
  geom_point(data=plot_data3,aes(K_list3,MSE3)) 

MSE3[1]
MSE3[20]
which(MSE3==min(MSE3))
min(MSE3)

best_out3 = knn.reg(train=scaled_trans_train[,-c(7)],test=scaled_trans_test[,-c(7)],
                    y=scaled_trans_train[,c(7)], k=10)
y_test_pred3 = exp(matrix(best_out3$pred))
rmse_best3 = sqrt(mean((exp(scaled_trans_test[,c(7)]) - y_test_pred3) ^ 2))

rmse_best
rmse_best1
rmse_best2
rmse_best3

min(MSE)
min(MSE1)
min(MSE2)
min(MSE3)

