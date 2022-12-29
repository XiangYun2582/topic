getwd()
cate <- rep(1:22,72) %>% as.data.frame()
plotyear <- cbind(year,cate)
names(plotyear)[4] <- "cate"
month <- data.frame()
monthbind <- data.frame()
for (i in 1:72) {
  monthbind <- rep(i,22) %>% as.data.frame()
  month <- rbind(monthbind,month)
}
names(month) <- "month"
month <-month %>%  arrange(month)
plotyear <- cbind(plotyear,month)
# Make a ggplot, but add frame=year: one image per year
p <- ggplot(plotyear, aes(cate, month, size = TotalUse, color =city)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(date) +
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



# libraries:
library(ggplot2)
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
p<- ggplot(plotyear, aes(x=city, y=TotalUse, fill=city)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    date,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')+
  # gganimate specific bits:
  labs(title = 'Year-month: {frame_time}', x = '台灣縣市', y = '各地用電量(度)') +
  transition_time(date)

# Save at gif:
animate(
  p,
  duration = 20,
  fps = 60,
  width = 1200,
  height = 500,
  end_pause = 60,
  # 动画中最后一帧的重复次数
  res = 100,
  renderer = gifski_renderer()
)
anim_save("outputbar.gif")




