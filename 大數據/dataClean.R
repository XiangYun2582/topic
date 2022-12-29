getwd()
library(dplyr)
library(data.table)
#my_data <- read.csv("C:/Users/Huang/Desktop/研究/bigdata/台灣電力公司.csv",encoding="UTF-8")
my_data <- fread("台灣電力公司.csv",encoding="UTF-8")
View(my_data)
names(my_data) <- c("date","city","houseSaleElec","housePercent","serviceUseElec","servicePercent","agriSaleElec","agriPercent","industrySaleElec","industryPercent","saleTotal","cityUseTotal")
#detect Percent
a <- (my_data$housePercent+my_data$servicePercent+my_data$agriPercent+my_data$industryPercent)
sum(a<0.999)
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
names(my_data)
####台灣六年平均月份用電量長條圖####----
mondata <- my_data %>% filter(substring(my_data$date,1,4)!="2022")
mondata$date <- substr(mondata$date,6,7)
mondata <- mondata %>% select("date","saleTotal"=,"TotalUse","houseUse","agriUse","industryUse")
mondata <- mondata %>% group_by(date) %>% summarise(TotalUsemean=mean(TotalUse),houseUsemean=mean(houseUse),agriUsemean=mean(agriUse),industryUsemean=mean(industryUse))
write.csv(mondata,"台灣六年平均月份用電量.csv",fileEncoding = "big-5")

####台灣近六年來用電量折線圖&縣市長條圖####----
##抓出2016~2021
years=c(2016:2021)
data2016 = my_data %>% filter(grepl(paste0(years[1]), my_data$date)) %>% arrange(date)
data2017 = my_data %>% filter(grepl(paste0(years[2]), my_data$date)) %>% arrange(date)
data2018 = my_data %>% filter(grepl(paste0(years[3]), my_data$date)) %>% arrange(date)
data2019 = my_data %>% filter(grepl(paste0(years[4]), my_data$date)) %>% arrange(date)
data2020 = my_data %>% filter(grepl(paste0(years[5]), my_data$date)) %>% arrange(date)
data2021 = my_data %>% filter(grepl(paste0(years[6]), my_data$date)) %>% arrange(date)
six=c(paste0("data",2016:2021))
write.csv(data2016,"data2016.csv",fileEncoding = "big-5")
write.csv(data2017,"data2017.csv",fileEncoding = "big-5")
write.csv(data2018,"data2018.csv",fileEncoding = "big-5")
write.csv(data2019,"data2019.csv",fileEncoding = "big-5")
write.csv(data2020,"data2020.csv",fileEncoding = "big-5")
write.csv(data2021,"data2021.csv",fileEncoding = "big-5")

Use2016<- data2016 %>% select(city,
                    serviceUseElec,
                    cityUseTotal,
                    houseUse,
                    agriUse,
                    industryUse,
                    TotalUse) %>% group_by(city) %>% summarise(
                      house = mean(houseUse),
                      agri = mean(agriUse),
                      industry = mean(industryUse),
                      Total = mean(TotalUse)
                    )
Use2017<- data2017 %>% select(city,
                              serviceUseElec,
                              cityUseTotal,
                              houseUse,
                              agriUse,
                              industryUse,
                              TotalUse) %>% group_by(city) %>% summarise(
                                house = mean(houseUse),
                                agri = mean(agriUse),
                                industry = mean(industryUse),
                                Total = mean(TotalUse)
                              )
Use2018<- data2018 %>% select(city,
                              serviceUseElec,
                              cityUseTotal,
                              houseUse,
                              agriUse,
                              industryUse,
                              TotalUse) %>% group_by(city) %>% summarise(
                                house = mean(houseUse),
                                agri = mean(agriUse),
                                industry = mean(industryUse),
                                Total = mean(TotalUse)
                              )
Use2019<- data2019 %>% select(city,
                              serviceUseElec,
                              cityUseTotal,
                              houseUse,
                              agriUse,
                              industryUse,
                              TotalUse) %>% group_by(city) %>% summarise(
                                house = mean(houseUse),
                                agri = mean(agriUse),
                                industry = mean(industryUse),
                                Total = mean(TotalUse)
                              )%>% filter(data2016_2022[2]!="合計")
Use2020<- data2020 %>% select(city,
                              serviceUseElec,
                              cityUseTotal,
                              houseUse,
                              agriUse,
                              industryUse,
                              TotalUse) %>% group_by(city) %>% summarise(
                                house = mean(houseUse),
                                agri = mean(agriUse),
                                industry = mean(industryUse),
                                Total = mean(TotalUse)
                              )
Use2021<- data2021 %>% select(city,
                              serviceUseElec,
                              cityUseTotal,
                              houseUse,
                              agriUse,
                              industryUse,
                              TotalUse) %>% group_by(city) %>% summarise(
                                house = mean(houseUse),
                                agri = mean(agriUse),
                                industry = mean(industryUse),
                                Total = mean(TotalUse)
                              )
write.csv(Use2016,"use2016.csv",fileEncoding = "big-5")
write.csv(Use2017,"use2017.csv",fileEncoding = "big-5")
write.csv(Use2018,"use2018.csv",fileEncoding = "big-5")
write.csv(Use2019,"use2019.csv",fileEncoding = "big-5")
write.csv(Use2020,"use2020.csv",fileEncoding = "big-5")
write.csv(Use2021,"use2021.csv",fileEncoding = "big-5")



cityUse <- rbind(Use2016,Use2017,Use2018,Use2019,Use2020,Use2021)
cityUse <- cityUse %>% group_by(city) %>% summarise(newTotal=mean(Total))
#cityUse <- cityUse[-5,]
cityUse

names(data2016_2022)
data2016_2022 <- rbind(data2016,data2017,data2018,data2019,data2020,data2021)
data2016_2022 <- data2016_2022 %>% select(date,city,serviceUseElec,cityUseTotal,houseUse,agriUse,industryUse,TotalUse) %>% filter(data2016_2022[2]!="合計")
datasum <- data2016_2022 %>% group_by(date,city) %>% summarise(
  house = mean(houseUse),
  agri = mean(agriUse),
  industry = mean(industryUse),
  Total = mean(TotalUse)
) 

write.csv(datasum,"usesum.csv",fileEncoding = "big-5")

year  <-  rbind(data2016,data2017,data2018,data2019,data2020,data2021) %>% select(date,city,
                    TotalUse)%>% group_by(date,city) %>% filter(city!="合計")
# Libraries
library(ggplot2)
library(dplyr)
windows()
year %>%
  ggplot( aes(x=date, y=TotalUse, group=city, color=city)) +
  geom_line(size=2)+
  labs(
    title = "2021年的用電量折線圖")
heatmap(
  matrixdata,
  Colv = NA,
  Rowv = NA,
  scale = "row",
  col = terrain.colors(256),
  main = "台灣六年來各縣市的用電量熱圖",
  xlab = "日期(年/月)",
  ylab = "縣市"
)
heatmap(
  matrixdata,
  Colv = NA,
  Rowv = NA,
  scale = "column",
  main = "台灣六年來各縣市的用電量熱圖",
  xlab = "日期(年/月)",
  ylab = "縣市"
)
windows()
library(reshape2)
year <- year %>% as.data.frame()
matrixdata <- dcast(year,date~city)
class(matrixdata[1,1])
row.names(matrixdata) <- matrixdata$date
#matrixdata <- matrixdata[,-1] %>% as.matrix()
matrixdata <- t(matrixdata)
write.csv(matrixdata,"heatmaporange.csv",row.names = T,fileEncoding = "UTF-8")
