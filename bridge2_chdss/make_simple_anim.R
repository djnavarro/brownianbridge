library(tidyverse)
library(gganimate)

# read the message map
msg_map <- read_csv(here::here("msg_map.csv")) %>%
  mutate(xval = xpos + xoff, yval = ypos + yoff) %>%
  arrange(frame, xval, yval) 

# dot count
dot_count <-  msg_map %>% group_by(frame) %>% 
  summarise(count = n())

# give each dot a unique identity
msg_map$id <- dot_count$count %>%
  map(function(x){1:x}) %>% reduce(c)

# create the base plot
base_pic <- msg_map %>% 
  ggplot(aes(
    x = xval, 
    y = yval)) + 
  geom_point(
    aes(colour = factor(id)), 
    show.legend = FALSE) + 
  coord_equal() +
  theme_void()


anim <- TRUE
if(!anim) {
  
  # draw as a faceted plot
  pic <- base_pic + 
    facet_grid(frame ~ .)
  plot(pic)
  
} else {
  
  # construct animation
  pic <- base_pic +
    transition_states(states = frame,
                      transition_length = .5,
                      state_length = 1)  +
    ease_aes('linear')
  
  # save animation
  pic %>% animate()
  anim_save(here::here("simple_animation.gif"))
  
}




