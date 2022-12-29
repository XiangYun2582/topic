library(dplyr)
library(sf)
getwd()
windows()
newcity.map <-
  st_read("C:/Users/Huang/Desktop/研究/bigdata/mapdata202209220431/TOWN_MOI_1100415.shp") %>% filter(COUNTYNAME ==
                                                                                                     "新北市")
#print(newcity.map)
dim(newcity.map)
names(newcity.map)
plot(newcity.map[1])
plot(st_geometry(newcity.map))
newcity.map$TOWNNAME
my.newcity.map <- newcity.map[c("TOWNNAME", "geometry")]
my.newcity.map$TOWNNAME <- as.character(newcity.map$TOWNNAME)
head(my.newcity.map)
y <-
  mydata10 %>% select(The.villages.and.towns.urban.district, total.price.NTD.1) %>%  group_by(The.villages.and.towns.urban.district) %>% summarise(Pricemedian=median(total.price.NTD.1))
my.taiwan.map.data <- left_join(my.newcity.map,y,
                                by = c("TOWNNAME" = "The.villages.and.towns.urban.district"))
library(ggplot2)
ggplot(data = my.taiwan.map.data) +
  geom_sf(aes(fill = Pricemedian))+
  scale_fill_distiller(palette = "Spectral")+
  ggtitle("新北市房價")+
  theme(plot.title = element_text(hjust = 1))+
  labs(caption = "備註:單位為每坪價格 灰色為無此資料")

