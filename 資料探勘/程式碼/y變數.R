library(dplyr)
library(ggplot2)
mydata7 %>% summary(.)
p <- ggplot(test, aes(x=test[,1])) + 
  geom_histogram()
p

windows()

ggplot(test, aes(x = test[, 1])) +
  geom_boxplot(
    fill = "slateblue",
    alpha = 0.2,
    size = 0.1,
    outlier.shape = 19,
    outlier.size = 2,outlier.color = "red"
  )+
  labs(x = '每坪價格',
       y = '',
       title = '房價的盒方圖') 

333366-15458
#離群值
test %>% slice_max(total.price.NTD.1,n=5)
outlier <- mydata7 %>% arrange(desc(test[,1])) %>% head(n=5)
outlier
names(mydata7);#brick
test <- mydata7[is.na(mydata7[,4])==F,]
distinct(mydata7[5])
a <- data.frame()
for (i in 1:960) {
  if(test[i,5]=="SRC"){
    a[i,1] <- 1
  }
  if(test[i,5]=="SC"){
    a[i,1] <- 2
  }
  if(test[i,5]=="RC"){
    a[i,1] <- 3
  }
  if(test[i,5]=="加強磚造"){
    a[i,1] <- 4
  }
  if(test[i,5]=="磚造"){
    a[i,1] <- 4
  }
}
for (i in 1:960) {
  if(test[i,6]=="有"){
    a[i,2] <- 1
  }else{
    a[i,2] <- 0
  }
}
#test <- cbind(test,a)
test <- test[,-5]
names(test)[11] <- "materials"
names(test)[12] <- "guard"
#write.csv(test,"目標資料集8.csv",row.names = FALSE)
####new----
mydata10 <- read.csv("目標資料集10.csv")
windows()
ggplot(mydata10, aes(x = mydata10[, 1])) +
  geom_boxplot(
    fill = "slateblue",
    alpha = 0.2,
    size = 0.1,
    outlier.shape = 19,
    outlier.size = 2,outlier.color = "red"
  )+
  labs(x = '每坪價格',
       y = '',
       title = '房價的盒方圖') +
  labs(caption = "註記:為已去除遺失值後960筆")
adjdata <-mydata10 %>%  arrange(mydata10[,1])
adjdata <-adjdata[1:957,]
ggplot(adjdata, aes(x = adjdata[, 1])) +
  geom_boxplot(
    fill = "slateblue",
    alpha = 0.2,
    size = 0.1,
    outlier.shape = 19,
    outlier.size = 2,outlier.color = "red"
  )+
  labs(x = '每坪價格',
       y = '',
       title = '房價的盒方圖') 
summary(mydata10)
distinct(adjdata [11])
#write.csv(adjdata,"目標資料集9.csv",row.names = FALSE)

set.seed(3347)
names(adjdata)
adjdata1 <- adjdata %>% group_by(materials,guard) %>% summarise(price=mean(total.price.NTD.1)) %>%as.data.frame()
adjdata2 <- dcast(adjdata1,materials~guard)
row.names(adjdata2) <- adjdata2$materials
adjdata2 <- adjdata2[,-1]
windows()
class(adjdata2)
adjdata2 <- as.matrix(adjdata2)
# Dummy data
x <- adjdata1[1]
y <- adjdata1[2]
for (i in 1:8) {
  if(adjdata1[i,2]==1){
    adjdata1[i,4]="有保安"
  }
  if(adjdata1[i,2]==0){
    adjdata1[i,4]="沒保安"
  }
}
for (i in 1:8) {
  if(adjdata1[i,1]==1){
    adjdata1[i,5] <- "SRC"
  }
  if(adjdata1[i,1]==2){
    adjdata1[i,5] <- "SC"
  }
  if(adjdata1[i,1]==3){
    adjdata1[i,5] <- "RC"
  }
  if(adjdata1[i,1]==4){
    adjdata1[i,5] <- "磚造"
  }
}
data <- cbind(X=x, Y=y)
data$Z <- adjdata1[,3]
# Color Brewer palette
library(viridis)
library(hrbrthemes)
adjdata1$V5 <- factor(adjdata1$V5, ordered = TRUE, levels = c("SRC", "SC", "RC", "磚造"))
# Give extreme colors:
ggplot(adjdata1, aes(adjdata1$V5, adjdata1$V4 ,fill= adjdata1$price)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  theme_ipsum()+ggtitle("保全與建築類別對房價的影響")




