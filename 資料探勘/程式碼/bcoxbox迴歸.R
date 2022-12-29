library(data.table)
library(dplyr)
##全部下去跑迴歸
test20 <- fread("test20.csv",header = T)
train80 <- fread("train80.csv",header = T)
train <- train80 %>% select(total.price.NTD.1,hospital.far,day,busfar,materials,all.police.station.far,convifar,all_post_office)
rc=data.frame()
for (i in 1:nrow(train)) {
  if (train[i,5]==3){
    rc[i,1]=1
  }
  else{
    rc[i,1]=0
  }
}
train <- cbind(train,rc)
test=data.frame()
for (i in 1:nrow(test20)) {
  if (test20[i,11]==3){
    test[i,1]=1
  }
  else{
    test[i,1]=0
  }
}
test=cbind(test20,test)
names(test)[13] <- "rc"
names(train)[9] <- "rc"
boxcox <- function(hospital_far,day,busfar,rc,all_police_station_far,convifar,all_post_office){
  lambday <- 862.073938-0.028524*hospital_far-0.007728*day+0.019445*busfar-92.915140*rc-0.006234*all_police_station_far+0.007280*convifar--0.002763*all_post_office
  fity <- (lambday*0.5+1)^2
  return(fity)
}
names(test20.x )
test20.x <- test[,c(10,4,5,13,8,6,7)] %>% data.frame()
fittedy <- data.frame()
summary(train80)#ymean118763
for (loop in 1:1) {
  SSE <- 0
  SST <- 0
  MAPE <- 0
  for (i in 1:nrow(test20)) {
    fittedy[i, 1] <- boxcox(test20.x[i, 1],
                            test20.x[i, 2],
                            test20.x[i, 3],
                            test20.x[i, 4],
                            test20.x[i, 5],
                            test20.x[i, 6],
                            test20.x[i, 7])
    SSE <- SSE + (test20[i, 1] - fittedy[i, 1]) ^ 2
    SST <- SST + (test20[i, 1] - 118763) ^ 2
    MAPE <- MAPE+(abs(test20[i, 1]-fittedy[i, 1])/test20[i, 1])/nrow(test20)
  }
  Rsquared <- 1 - SSE / SST %>% as.numeric()
  MSE <- SSE /(nrow(test20)-7) %>% as.numeric()
  MAPE <- MAPE %>% as.numeric()
  print(c(Rsquared=Rsquared,MSE=MSE,MAPE=MAPE))
}
##單一只有跑距離變數
distance <- function(hospital_far,busfar,all_police_station_far,convifar,all_post_office,schoolfar){
  lambday <- 720.646018-0.026699*hospital_far+0.023433*busfar-0.009658*all_police_station_far+0.004920*convifar-0.002707*all_post_office-0.000421*schoolfar
  fity <- (lambday*0.5+1)^2
  return(fity)
}
test.x <- test20[,c(4,5,6,7,8,9,10)] %>% data.frame()
names(test.x)
fittedy <- data.frame()
for (loop in 1:1) {
  SSE <- 0
  SST <- 0
  MAPE <- 0
  for (i in 1:nrow(test20)) {
    fittedy[i, 1] <- distance(test.x[i, 7],
                            test.x[i, 2],
                            test.x[i, 5],
                            test.x[i, 3],
                            test.x[i, 4],
                            test.x[i, 6])
    SSE <- SSE + (test20[i, 1] - fittedy[i, 1]) ^ 2
    SST <- SST + (test20[i, 1] - 118763) ^ 2
    MAPE <- MAPE+(abs(test20[i, 1]-fittedy[i, 1])/test20[i, 1])/nrow(test20)
  }
  Rsquared <- 1 - SSE / SST %>% as.numeric()
  MSE <- SSE /(nrow(test20)-7) %>% as.numeric()
  MAPE <- MAPE %>% as.numeric()
  print(c(Rsquared=Rsquared,MSE=MSE,MAPE=MAPE))
}

