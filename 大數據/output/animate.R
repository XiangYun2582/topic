#bubble效果很差
# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
getwd()
setwd("C:/Users/Huang/Desktop/研究/bigdata/photo")
# Make a ggplot, but add frame=year: one image per year
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
animate(
  p,
  duration = 10,
  fps = 50,
  width = 800,
  height = 500,
  end_pause = 60,
  # 动画中最后一帧的重复次数
  res = 100,
  renderer = gifski_renderer()
)
anim_save("output.gif")

### 设置当前与逆行路径
setwd("E:/R/谭三三R公众号/") # 先设置路径，最后图片会保存在子自定义的路径中

### 载入包，没有安装通过install.packages("gifski")等方法进行安装
library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)
library(gifski)
library(readr)
library(tidyr)
library(viridis)

### 绘制静态气泡图
jingtai1 = gapminder %>%
  ggplot(aes(x=gdpPercap, # 将人均GDP映射到x
             y=lifeExp,  # 将平均寿命映射到y
             fill=continent, # 根据大洲的类别来填充颜色
             size=pop)) + # 控制气泡大小的变量为人口数量
  geom_point(color = "black", # 将气泡框设为黑色
             shape = 21) + # 将气泡设为实心圆(21)
  theme_bw() + # 设置主题
  #theme_fivethirtyeight() + # 也可以用这个主题，来自ggthemes程序包
  scale_size(range = c(1, 16)) + # 设置气泡大小的最小值到最大值范围(这里设为2到16)
  scale_x_log10() + # 对x轴进行以10为底的对数变换，可以让散点(或气泡变得分散点，不然就会全部聚到一边)
  scale_fill_viridis(alpha = .4, # 将气泡透明度设为0.4
                     discrete = TRUE, # 生成离散调色板(默认值：FALSE=生成连续调色板)。
                     option = "C") + # option设置气泡配色方案（有ABCDEFGH8个方案可选，可自行尝试）
  theme_bw() + # 设置图表主题
  labs(title = "平均寿命 vs 人均GDP", # 设置标题等
       x = "人均GDP",
       y = "平均寿命",
       color = "Continent",
       caption = "Source: Gapminder") +
  theme(axis.title = element_text(), # 设置主题
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) 
# scale_color_brewer(palette = "Set2") # 也可以通过这句来设置主题配色，而不是上面的scale_fill_viridis()
jingtai1 # 打印图形

### 绘制动态gif气泡图
# 创建一个animate动图对象
jingtai1.animation = jingtai1 + # 根据前面绘制的静态气泡图jingtai1创建一个动图渲染对象
  transition_time(year) + # 给出观察的时间
  labs(subtitle = "Year: {frame_time}") + # 在图中呈现Year:****
  
  shadow_wake(wake_length = 0.1) # 与总帧数相关的唤醒长度设置为0.1

animate(jingtai1.animation, # 一个gganim对象
        height = 500, # 图高
        width = 800, # 图宽
        fps = 50, # 渲染速度(动画运动速度，一般是100的倍数)
        duration = 10, # 以秒为单位的动画长度
        end_pause = 60, # 动画中最后一帧的重复次数
        res = 100,renderer = gifski_renderer())

anim_save("gapminder graph.gif") # 将绘制的gif动图保存到最开始设置的文件路径中











