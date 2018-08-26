library(tidyverse)
library(gganimate)

# read the message map
msg_map <- read_csv(here::here("msg_map.csv"))

# create a simple animation
pic <- msg_map %>% 
  ggplot(aes(
    x = xpos + xoff, 
    y = ypos + yoff)) + 
  geom_point() + 
  coord_equal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  transition_states(states = frame,
                    transition_length = 1,
                    state_length = 1)  +
  ease_aes('linear') 

# create simple animation
pic %>% animate()
anim_save(here::here("simple_animation.gif"))