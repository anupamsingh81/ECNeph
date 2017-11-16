library(gapminder)
library(ggplot2)
theme_set(theme_bw())
library(gganimate)
library(tidyverse)

f= ecneph2 %>%select(date_by_week,activity) %>%  group_by(date_by_week) %>% summarise(n=sum(activity)) %>% filter(n>=50) %>% pull(date_by_week)

ecneph4 = ecneph2 %>% filter(date_by_week%in%f) # select big activity weeks
p <-ecneph4 %>% filter(activity>=10) %>%  ggplot( aes(x=activity, y=log(reply_rt_sum), size = retweet, color = user, frame = date_by_week)) +
  geom_point() +
  scale_x_log10()


?gganimate
gganimate(p)

getwd()
g <- gganimate(p, interval = .1, title_frame = FALSE, "filename.gif")


g <- gganimate(p, interval = .4, title_frame = TRUE, "map.gif")
data(gapminder)
head(gapminder)

?gganimate