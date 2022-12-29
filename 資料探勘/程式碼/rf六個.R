rm = list(ls = all(T))
library(randomForest)
library(dplyr)
set.seed(1)
data = read.csv("目標資料集10.csv")
train_data = read.csv("train80.csv")
test_data = read.csv("test20.csv")
names(data)
#將train_data and test_data做資料清理
wash_train_data<- cbind(train_data["total.price.NTD.1"],train_data["busfar"],train_data["convifar"],train_data["all_post_office"],train_data["all.police.station.far"],train_data["schoolfar"],train_data["hospital.far"])
wash_test_data <- cbind(test_data["total.price.NTD.1"],test_data["busfar"],test_data["convifar"],test_data["all_post_office"],test_data["all.police.station.far"],test_data["schoolfar"],test_data["hospital.far"])


#畫圖
for (i in 1:6){
  new_rf1 <- randomForest(total.price.NTD.1~.,data = wash_train_data,mtry = i,importance = TRUE)
  name = paste("varImpPlot",i,sep = "_")
  name = paste(name,"png",sep = ".")
  jpeg(file = name)
  jpeg(name,width=1200,height=600)
  varImpPlot(new_rf1)
  dev.off()
}

#做預測
set.seed(1)
new_rf1 <- randomForest(total.price.NTD.1~.,data = wash_train_data,mtry = 6,importance = TRUE)
View(wash_test_data)
#ttest <- data.frame(busfar=1900,convifar=550,all_post_office=1500,all.police.station.far=950,schoolfar=750,hospital.far=1600)
#yhat <- predict(new_rf1,newdata = ttest)
yhat <- predict(new_rf1,newdata = wash_test_data)
mean((yhat-wash_test_data$total.price.NTD.1)^2) #mse
mean(abs((wash_test_data$total.price.NTD.1-yhat)/wash_test_data$total.price.NTD.1)) #MAPE

