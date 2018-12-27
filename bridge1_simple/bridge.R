library(tidyverse)
library(e1071)
library(gganimate)

# parameters for the simulation
ntimes <- 100
nseries <- 20

# construct tibble storing simulation
tbl <- tibble(
  Time = rep(1:ntimes, nseries),
  Horizontal = replicate(nseries, c(0,rbridge(1,ntimes-1))) %>% as.vector(),
  Vertical = replicate(nseries, c(0,rbridge(1,ntimes-1))) %>% as.vector(),
  Series = gl(nseries, ntimes)
)

# gganimate
pic <- tbl %>%
  ggplot(aes(
    x = Horizontal, 
    y = Vertical, 
    colour = Series)) + 
  geom_point(show.legend = FALSE,
             size = 5, alpha = .6) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  transition_time(time = Time)  +
  ease_aes('linear') + 
  shadow_wake(.1)
  
  
# create
pic %>% animate(nframes = 500, fps = 20)
anim_save("~/Desktop/bridge.gif")
