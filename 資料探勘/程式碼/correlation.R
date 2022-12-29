mydata <- mtcars[, c(1,3,4,5,6,7)]
head(mydata)
cormat <- round(cor(mydata),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkgreen", high = "darkred", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

data <- read.csv("目標資料集10.csv")
names(data)
mydata <- data[, c(1,5,6,7,8,9,10)]
head(mydata)
cormat <- round(cor(mydata),2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
upper_tri <- get_upper_tri(cormat)
upper_tri
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+ 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  scale_fill_gradient2(low = "darkgreen", high = "darkred", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

