####資料前置----
library(dplyr)
library(ggplot2)
data <- read.csv("C:/Users/Huang/Desktop/資料探勘/報告/目標資料集10.csv",fileEncoding = "big-5")
names(data)
distinct(data[11])
x=data.frame()
for (i in 1:957) {
  if(data[i,11]==1){
    x[i,1]="SC"
    x[i,2]=18440/7020
  }
  if(data[i,11]==2){
    x[i,1]="SRC"
    x[i,2]=15050/7020
  }
  if(data[i,11]==3){
    x[i,1]="RC"
    x[i,2]=11180/7020
  }
  if(data[i,11]==4){
    x[i,1]="Brick"
    x[i,2]=7020/7020
  }
}
for (i in 1:957) {
  if(data[i,12]==1){
    x[i,3]="有警衛"
  }
  if(data[i,12]==0){
    x[i,3]="沒警衛"
  }
}
####heatmap----
library(reshape2)
names(x) <- c("materials","weights","guard")
allmatrix <- cbind(x,data[,1])
allmatrix <- allmatrix[,-2]
names(allmatrix)[3] <- "y"
allmatrix <- allmatrix %>% select(guard,materials,y) %>% group_by(guard,materials)
allmatrix.dcast <- dcast(allmatrix,materials~guard)
table(allmatrix$guard,allmatrix$materials)

single <- cbind(x,data[,1])
single <- single[ ,-2]#run兩次
names(single)[2] <- "y"
single <- data.frame(single ,hei=rep(1,957)) %>% group_by(materials) %>% summarise(mean=mean(y))
single <- data.frame(single ,hei=rep(1,4))
single$materials <- factor(single$materials, levels = c("SC","SRC","RC","Brick"))

##single單一材料變數
ggplot(single, aes(x = materials, y = hei , fill = mean)) +
  geom_tile(colour = "white", size = 0.25) + # 繪製熱圖
  scale_y_discrete(expand = c(0, 0)) + # 移除多餘空白
  scale_x_discrete(expand = c(0, 0)) + # 移除多餘空白
  coord_fixed() + # 設定 X 與 Y 軸等比例
  scale_colour_brewer(palette = "Greens") + # 設定色盤
  theme(
    legend.text = element_text(face = "bold"),
    # 說明文字用粗體
    axis.ticks = element_line(size = 0.5),
    # 座標軸上的刻度寬度
    plot.background = element_blank(),
    # 移除背景
    panel.border = element_blank(),
    # 移除邊框
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ) # X 軸文字轉向
  )+xlab("建築材料的種類") + ylab("") + labs(title = "建築材料對每坪房價影響的熱圖", caption = "備註:建材價格比 SC:SRC:RC:Brick=2.63:2.14:1.59:1")


goal <- cbind(x,data[,1]);names(goal)[4] <-c("y")
goal <- goal %>% group_by(materials,guard) %>% summarise(mean=mean(y))
#a <- goal[,-3]
goal.melt <- dcast(goal,materials~guard)
rownames(goal.melt) <- goal.melt$materials
#goal.melt <- goal.melt[,-1] %>% as.matrix()
library(RColorBrewer)
##交叉分析----
goal$materials <-
  factor(goal$materials, levels = c("SC", "SRC", "RC", "Brick"))
windows()
text <- c(2, 41, 686, 191, 30, 1, 15, 1)
goaltext <- cbind(goal, text)
names(goaltext)[4] <- "text"
ggplot(goaltext, aes(x = materials , y = guard , fill = mean)) +
  geom_tile(colour = "white", size = 0.25) + # 繪製熱圖
  scale_y_discrete(expand = c(0, 0)) + # 移除多餘空白
  scale_x_discrete(expand = c(0, 0)) + # 移除多餘空白
  coord_fixed() + # 設定 X 與 Y 軸等比例
  scale_fill_gradientn(colours = brewer.pal(7, "YlGnBu")) + # 設定色盤
  theme(
    legend.text = element_text(face = "bold"),
    # 說明文字用粗體
    axis.ticks = element_line(size = 0.5),
    # 座標軸上的刻度寬度
    plot.background = element_blank(),
    # 移除背景
    panel.border = element_blank(),
    # 移除邊框
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ) # X 軸文字轉向
  ) + xlab("建築材料的種類") + ylab("") + labs(title = "建築材料和有無警衛對每坪房價影響的熱圖", caption = "圖中的數字為個數  \n 備註:建材價格比 SC:SRC:RC:Brick=2.63:2.14:1.59:1")+geom_text(label = goaltext$text)




#test----
test <- data %>% select(materials,total.price.NTD.1) %>% group_by(materials) %>% as.data.frame()
test$materials <-
  factor(test$materials, levels = c(1, 2, 3, 4))
output <- aov(total.price.NTD.1~materials,data=test)
summary(output)
