####前期的資料分類####----
getwd()
library(readxl)
library(dplyr)
my_data <- read_excel("f_lvr_land_a.xls",sheet = "不動產買賣",skip = 1)
#名稱
#View(read_excel("f_lvr_land_a.xls",sheet = "不動產買賣",n_max = 1) %>% data.frame())
##補變數英文名稱
item <- c("mainArea","alArea","balArea","elevator","transid")
for (i in 1:5) {
  names(my_data)[i+28] <- item[i]
}

#資料型態
slice_head(my_data,n=15)
#找出買賣物的元素
distinct(my_data[,2]) %>% as.data.frame()
#View(my_data)

##依交易標的分類
dataland <- my_data %>% filter(my_data[2]=="土地")
datacar <- my_data %>% filter(my_data[2]=="車位")
datahouse <- my_data %>% filter(my_data[2]!="土地"&my_data[2]!="車位")
#write.csv(datahouse,"目標資料集.csv",fileEncoding ="Big-5")
##去掉建物只有兩筆去掉對整個992筆資料沒影響
datahouse <- datahouse %>%filter(datahouse[2]!="建物")

####正式資料處理####----
distinct(datahouse[2]) %>% as.data.frame()
names(datahouse)

nrow(datahouse)
##檢驗反映變數是否為空值
sum(datahouse[,23]!=0)
summary(datahouse)
datahouse <- datahouse %>% as.data.frame()

####簡單分析y ####----
# library
library(ggplot2)
#windows()
## basic histogram
p <- ggplot(datahouse, aes(x=datahouse[,23])) + 
  geom_histogram()
p

## A really basic boxplot.
ggplot(datahouse, aes(y=datahouse[,23])) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
##離群值
outlier <- datahouse %>% arrange(desc(datahouse[,23])) %>% head(n=6)
outlier

####解釋變數資料處理####----
##針對土地分區規劃
names(datahouse)
datahouse[,34] <- c(rep(0,992)) %>% as.data.frame()
names(datahouse)[34] <- c("ruledArea")

for (i in 1:nrow(datahouse)) {
    datahouse[i, 34] = datahouse[i, 5]
    if(is.na(datahouse[i, 34])){
      datahouse[i, 34] = datahouse[i, 6]
    }
    if(is.na(datahouse[i, 34])){
      datahouse[i, 34] = datahouse[i, 7]
    }
}
##抓出空值是誰?
datahouse %>% filter(is.na(datahouse[34]))
#差補依據Google
txt <- c(214,297,320,346,356,372,428,535,570,595,723,901)
value <- c("工","住","住","住","商","工","住","住","住","商","住","住")
length(txt)
for (i in 1:12) {
  datahouse[txt[i], 34] = value[i]
}
#datahouse %>% filter(is.na(datahouse[34]))for check沒問題


distinct(datahouse[34])
datahouse[34]
for (i in 1:992) {
  datahouse[i,34] <- gsub("都市：其他:","", datahouse[i,34])
  datahouse[i,34] <- gsub("乙種工業區","工", datahouse[i,34])
  datahouse[i,34] <- gsub("工業區","工", datahouse[i,34])
  datahouse[i,34] <- gsub("住宅區","住", datahouse[i,34])
  datahouse[i,34] <- gsub("一般農業區","農", datahouse[i,34])
  datahouse[i,34] <- gsub("山坡地保育區","限制開發", datahouse[i,34])
  datahouse[i,34] <- gsub("風景區","限制開發", datahouse[i,34])
  datahouse[i,34] <- gsub("農村區","農", datahouse[i,34])
  datahouse[i,34] <- gsub("鄉村區","農", datahouse[i,34])
  datahouse[i,34] <- gsub("第二種特定專用區","住", datahouse[i,34])
  datahouse[i,34] <- gsub("保安保護區","限制開發", datahouse[i,34])
  datahouse[i,34] <- gsub("行水區","限制開發", datahouse[i,34])
  datahouse[i,34] <- gsub("乙種風景區","限制開發", datahouse[i,34])
  datahouse[i,34] <- gsub("第四種住","住", datahouse[i,34])
  datahouse[i,34] <- gsub("第二種住","住", datahouse[i,34])
  datahouse[i,34] <- gsub("道路用地","限制開發", datahouse[i,34])
  datahouse[i,34] <- gsub("已開發建築密集地區","限制開發", datahouse[i,34])
}
for (i in 1:992) {
  datahouse[i,34] <- gsub("乙種限制開發","限制開發", datahouse[i,34])
}
distinct(datahouse[34])

# 總價格 total.price.NTD
# 車位價格 the.berth.total.price.NTD
# 建物面積 building.shifting.total.area
# 車位面積 berth.shifting.total.area.square.meter

#檢查有沒有遺失值
which(is.na(my_data$total.price.NTD))
which(is.na(my_data$the.berth.total.price.NTD))
which(is.na(my_data$building.shifting.total.area))
which(is.na(my_data$berth.shifting.total.area.square.meter))

#計算每平方單位價格
price.per.square <- ((datahouse[22] -datahouse[26])/(datahouse[16]-datahouse[25]))
length(price.per.square)
names(datahouse)
#新增到資料集
datahouse <- cbind(datahouse,price.per.square)


#輸出資料集
#write.csv(my_data, file = "D:/資料探勘/目標資料集2.csv",row.names = FALSE)
#convifar<- read.csv("目標資料集2.csv") %>% select(convifar)
#my_data <- cbind(datahouse,convifar)
#write.csv(my_data, file = "目標資料集3.csv",row.names = FALSE)
my_data3 <- read.csv("目標資料集3.csv")
my_data3 %>% group_by(my_data3$main.building.materials) %>% summarise(main.building.materials=n())


names(my_data3)
Mydata <- my_data3
distinct(Mydata[14])
finCon <- my_data3$construction.to.complete.the.years %>% as.numeric()

finTra <- my_data3$transaction.year.month.and.day
finTra[992] <- c("1100801")
finTra <- fintra %>% as.numeric()
for (i in 1:992) {
  if(is.na(finCon)){
    print(i)
  }
  if (nchar(finCon[i]) < 7) {
    finCon[i] <- paste0(0,finCon[i])
  }
}



is.na(finCon)
nchar(finCon[1]) == 6

names(my_data3)

all_post_office <- read.csv("all_post_office.csv")
busfar <- read.csv("busfar.csv")
mydata4 <- my_data3 %>% select(
  The.villages.and.towns.urban.district,
  land.sector.position.building.sector.house.number.plate,
  main.building.materials,
  Whether.there.is.manages.the.organization,
  total.price.NTD.1,
  convifar
)
mydata4 <- cbind(mydata4,all_post_office,busfar)
names(mydata4)[7] <- "all_post_office"
write.csv(mydata4,"目標資料集4.csv",row.names = FALSE)
busfar.csv

mydata4[3]
distinct(Mydata[14])
distinct(mydata4[3])
#https://ghome.medium.com/%E5%8A%A0%E5%BC%B7%E7%A3%9A%E9%80%A0%E6%98%AF%E4%BB%80%E9%BA%BC-%E9%8B%BC%E7%AD%8B%E6%B7%B7%E5%87%9D%E5%9C%9F%E4%B8%8D%E5%90%8C%E5%9C%A8%E5%93%AA-%E5%84%AA%E7%BC%BA%E9%BB%9E%E8%A7%A3%E6%9E%90-485f086fa773
#https://kingarchit.com.tw/QA.php?Know=42
#test <- rep(0,992) %>% as.data.frame()
for (i in 1:nrow(mydata4)) {
  mydata4[i,3] <- gsub("鋼骨鋼筋混凝土構造、鋼骨構造、鋼筋混凝土構造","SRC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨構造、鋼骨鋼筋混凝土構造","SRC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨構造、鋼筋混凝土構造","SRC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨構造、鋼骨構造鋼筋混凝土","SRC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨構造、鋼筋混凝土","SRC",mydata4[i,3])
  mydata4[i,3] <- gsub("加強磚造、鋼筋混凝土造","加強磚造",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨鋼筋混凝土構造","SRC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨混凝土造","SC",mydata4[i,3])
  mydata4[i,3] <- gsub("Ｒ．Ｃ造","RC",mydata4[i,3])
  mydata4[i,3] <- gsub("ＲＣ造","RC",mydata4[i,3])
  mydata4[i,3] <- gsub("加強鋼筋混凝土造","RC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨鋼筋混凝土造","SRC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼筋混凝土造","RC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼筋混凝土構造","RC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨構造","SC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼造","SC",mydata4[i,3])
  mydata4[i,3] <- gsub("鋼骨造","SC",mydata4[i,3])
}

time <- read.csv("time.csv")
all.police.station.far <- read.csv("警局距離.csv")
mydata4 <- cbind(mydata4,all.police.station.far,time)
#mydata4 <- mydata4[,-9]
names(mydata4)[10] <- "day"
#write.csv(mydata4,"目標資料集5.csv",row.names = FALSE)
mydata5 <- read.csv("目標資料集5.csv")


test <- busfar
determine <- rep(1,992) %>% as.data.frame()
compute <- rep(1,992) %>% as.data.frame()
#grepl("里",test[i,1])
for (i in 1:992) {
  if(grepl("里",test[i,1])==1){
    determine[i,1] <- 1000
    compute[i,1] <- as.numeric(trimws(gsub("公里","",test[i,1])))
    test[i,1] <- compute[i,1]*1000
  }else{
    determine[i,1] <- 1
    compute[i,1] <- as.numeric(trimws(gsub("公尺","",test[i,1])))
    test[i,1] <- compute[i,1]*1
  }
}

names(mydata5)
a=7
for (a in 6:9) {
  for (i in 1:992) {
    if(grepl("里",mydata5[i,a])==1){
      determine[i,1] <- 1000
      compute[i,1] <- as.numeric(trimws(gsub("公里","",mydata5[i,a])))
      mydata5[i,a] <- compute[i,1]*1000
    }else{
      determine[i,1] <- 1
      compute[i,1] <- as.numeric(trimws(gsub("公尺","",mydata5[i,a])))
      mydata5[i,a] <- compute[i,1]*1
    }
  }
}
View(is.na(mydata5[1]) %>% as.data.frame())
sum(is.na(mydata5[8]))

for (j in 1:1) {
  determine <- rep(1,992) %>% as.data.frame()
  compute <- rep(1,992) %>% as.data.frame()
  for (i in 1:992) {
    if(grepl("里",mydata5[i,1])==1){
      determine[i,1] <- 1000
      compute[i,1] <- as.numeric(trimws(gsub("公里","",mydata5[i,1])))
      mydata5[i,1] <- compute[i,1]*1000
    }else{
      determine[i,1] <- 1
      compute[i,1] <- as.numeric(trimws(gsub("公尺","",mydata5[i,1])))
      mydata5[i,1] <- compute[i,1]*1
    }
  }
}


#要另新的變數不然轉不過數值
compute[1,1] <- as.numeric(trimws(gsub("公尺","",test[1,1])))
test[1,1] <- trimws(gsub("公尺","",test[j,1])) %>% as.numeric()
class(compute[1,1])
distinct(mydata6[3])
#write.csv(mydata5,"目標資料集6.csv",row.names = FALSE)
mydata6 <- read.csv("目標資料集6.csv")
schoolfar <- read.csv("schoolfar.csv")
hospital.far <- read.csv("醫院距離2.csv")
names(mydata7)
mydata7 <- cbind(mydata6, schoolfar, hospital.far)
#mydata7 <- mydata7[,-12]
mydata7 <-
  mydata7 %>% select(
    total.price.NTD.1,
    The.villages.and.towns.urban.district,
    land.sector.position.building.sector.house.number.plate,
    day,
    main.building.materials,
    Whether.there.is.manages.the.organization,
    busfar,
    convifar,
    all_post_office,
    all.police.station.far,
    schoolfar,
    hospital.far
  )
#write.csv(mydata7,"目標資料集7.csv",row.names = FALSE)
test <- fread("目標資料集10.csv")
write.csv(test,"mining.csv",row.names = FALSE,fileEncoding="UTF-8")

set.seed(3347)
Splitdata = function(data, p = 0.8)
{
  #p = 訓練樣本佔全部觀察值的比例
  index = sample(2, nrow(data), replace = TRUE, prob = c(p, 1 - p))
  train = data[index == 1, ]
  test = data[index == 2, ]
  out = list(train = train, test = test)
  return(out)
}
#out <- Splitdata(test)
#nrow(out$test)
#write.csv(out$train,"train80orange.csv",row.names = FALSE,fileEncoding="UTF-8")
#write.csv(out$test,"test20.csv",row.names = FALSE,fileEncoding="big-5")
