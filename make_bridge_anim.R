library(tidyverse)
library(e1071)
library(gganimate)

# parameters
nsteps <- 5
scale <- 10
anim <- TRUE

# read the message map
msg_map <- read_csv(here::here("msg_map.csv")) %>%
  mutate(xval = xpos + xoff, yval = ypos + yoff) %>%
  arrange(frame, xval, yval) 

# dot count
dot_count <-  msg_map %>% group_by(frame) %>% 
  summarise(count = n())

# give each dot a unique identity
msg_map$id <- dot_count$count %>%
  map(function(x){paste0("dot", 1:x)}) %>% 
  reduce(c)

# keep only the dot label, position, and frame
msg_map <- msg_map %>% 
  select(id, frame, xval, yval) %>% 
  arrange(id, frame)

# function to convert start and end points to
# a meandering path using brownian bridge
brownian_path <- function(id, xval, yval, nsteps = 5, scale = 20) {
  
  # expand from two end points to a straight line path
  path <- tibble(
    id = id[1],
    frame = 1:nsteps,
    xval = seq(xval[1], xval[2], length.out = nsteps),
    yval = seq(yval[1], yval[2], length.out = nsteps)
  )
  
  # create a meandering path using rbridge
  path$xval <- path$xval + c(0, rbridge(1, nsteps-1)) * scale
  path$yval <- path$yval + c(0, rbridge(1, nsteps-1)) * scale
  
  # return as a single element list
  return(list(p=path))
}

# construct a tibble tracking all 395 points across
# every jump in the brownian bridge from F1 to F2
msg_walk_forward <- msg_map %>% 
  group_by(id) %>%
  summarise(
    path = brownian_path(id, xval, yval, nsteps, scale)
  ) %>% 
  pull(path) %>% 
  reduce(bind_rows)

# do the same thing to walk back
msg_walk_backward <- msg_map %>% 
  mutate(frame = 3-frame) %>%
  arrange(id, frame) %>%
  group_by(id) %>%
  summarise(
    path = brownian_path(id, xval, yval, nsteps, scale)
  ) %>% 
  pull(path) %>% 
  reduce(bind_rows) %>%
  mutate(
    frame = frame + nsteps
  )

# combine
msg_walk <- bind_rows(
  msg_walk_forward, msg_walk_backward
)

# create the base plot
base_pic <- msg_walk %>%
  ggplot(aes(
    x = xval,
    y = yval,
    colour = id)) +
  geom_point(show.legend = FALSE,
             size = 2, alpha = .6) +
  coord_equal() +
  theme_void()

# create the output
if(!anim) {
  
  # draw as a faceted plot
  pic <- base_pic + facet_wrap(~frame)
  plot(pic)
  
} else {
  
  # construct animation
  pic <- base_pic +
    transition_states(
      states = frame,
      transition_length = 2,
      state_length = c(2.5,1,1,1,2.5,2.5,1,1,1,2.5),
      wrap = TRUE
      )  +
    ease_aes('linear') +
    shadow_wake(.1)
  
  # create animation
  pic %>% animate(
    nframes = 200, fps = 20, width = 800, height = 500)
  anim_save(here::here("bridge_animation.gif"))
}






