getwd()
library(dplyr)
library(data.table)
#my_data <- read.csv("C:/Users/Huang/Desktop/研究/bigdata/台灣電力公司.csv",encoding="UTF-8")
my_data <- fread("台灣電力公司.csv",encoding="UTF-8")
names(my_data) <- c("date","city","houseSaleElec","housePercent","serviceUseElec","servicePercent","agriSaleElec","agriPercent","industrySaleElec","industryPercent","saleTotal","cityUseTotal")
library(lubridate)
my_data$date <- gsub("年","-",my_data$date)
my_data$date <- gsub("月","",my_data$date)
my_data$date <- ym(my_data$date)
class(my_data$date)
my_data <- my_data %>% mutate(TotalUse = serviceUseElec / servicePercent) %>% mutate(
  houseUse = TotalUse * housePercent,
  agriUse = TotalUse * agriPercent,
  industryUse = TotalUse * industryPercent
)
condition <- c("桃園市", "高雄市", "台中市")
Taoyuan <-
  my_data %>% select(date, city, TotalUse) %>% filter(city == "桃園市") %>% select(date, TotalUse) %>% arrange(date)
Kaohsiung <-
  my_data %>% select(date, city, TotalUse) %>% filter(city == "高雄市") %>% select(date, TotalUse) %>% arrange(date)
Taichung <-
  my_data %>% select(date, city, TotalUse) %>% filter(city == "台中市") %>% select(date, TotalUse) %>% arrange(date)
Tainan <-
  my_data %>% select(date, city, TotalUse) %>% filter(city == "台南市") %>% select(date, TotalUse) %>% arrange(date)
write.csv(Taoyuan,"Taoyuanorange.csv",row.names = FALSE,fileEncoding="UTF-8")
write.csv(Kaohsiung,"Kaohsiungorange.csv",row.names = FALSE,fileEncoding="UTF-8")
write.csv(Taichung,"Taichungorange.csv",row.names = FALSE,fileEncoding="UTF-8")
write.csv(Tainan,"Tainanorange.csv",row.names = FALSE,fileEncoding="UTF-8")
##正式時間數列----
data<- Taoyuan
data.origin <- data[,2]
prod <- data[1:72,]
prod_train <- cbind(num=(1:72),prod)
prod_test <- cbind(num=(1:6),data[73:78,])

#訓練集資料
library(tseries)
windows()
par(mfrow=c(2,1))
acf(prod$TotalUse,60);pacf(prod$TotalUse,60)
adf.test(prod$TotalUse);pp.test(prod$TotalUse)

##季節性差分的話
windows()
seasonal=diff(prod$TotalUse,24)
par(mfrow=c(2,1))
acf(seasonal,60);pacf(seasonal,60)
adf.test(seasonal);pp.test(seasonal)

##再做一階差分的話
windows()
first=diff(seasonal,1)
par(mfrow=c(2,1))
acf(first,60);pacf(first,60)
adf.test(first);pp.test(first)

#pdq
windows()
TotalUse <- prod_train$TotalUse
arima111 <- arima(TotalUse,order=c(1,1,1),seasonal=list(order=c(0,1,0),period=24))
arima112 <- arima(TotalUse,order=c(1,1,2),seasonal=list(order=c(0,1,0),period=24))
arima011 <- arima(TotalUse,order=c(0,1,1),seasonal=list(order=c(0,1,0),period=24))
arima110 <- arima(TotalUse,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=24))
tsdiag(arima111,60)
tsdiag(arima112,60)
tsdiag(arima011,60)
tsdiag(arima110,60)

c(AIC(arima111),BIC(arima111))
c(AIC(arima111),BIC(arima112))
c(AIC(arima011),BIC(arima011))
c(AIC(arima110),BIC(arima110))
#配適
arima011
#names(data.fit)
data.fit=arima(TotalUse,order=c(0,1,1),seasonal=list(order=c(0,1,0),period=24))
c(data.fit$coef,data.fit$sigma2,AIC=data.fit$aic)
windows()
tsdiag(data.fit,60)

#predict
data.pre=predict(arima011, n.ahead=6)
#getwd()
#out=data.frame(his=data.origin$TotalUse[186:197],predict=data.pre$pred,up=U1,down=L1) %>% t()
#is.data.frame(out)
#out=as.data.frame(out)
#colnames(out) <- data$date[186:197]
#write_csv(out,file = 'predict.csv')

#names(data.pre)
U1=data.pre$pred+1.96*data.pre$se
L1=data.pre$pred-1.96*data.pre$se
plot.ts(data.origin,xlim=c(1, 78),type="o",pch=20)   


lines(U1,col="red",lty="dashed",pch=20)
lines(L1,col="red",lty="dashed",pch=20)
lines(data.pre$pred,col="blue",lty="dashed",type="o",pch=20)
abline(v=185,lty= "dotted")
+title("桃園市用電量預測")
#011


