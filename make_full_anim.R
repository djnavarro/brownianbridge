library(tidyverse)
library(e1071)
library(gganimate)

# parameters
nsteps <- 5
scale <- 15
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

# center the coords
msg_map <- msg_map %>%
  mutate(
    xval = xval - mean(xval),
    yval = yval - mean(yval)
  )

# make the network map
net_map <- here::here("network_map.csv") %>%
  read_csv() %>%
  arrange(xpos, ypos) %>% 
  mutate(
    id = paste0("dot",1:n()),
    frame = 3,
    xval = (xpos - mean(xpos))*1.5,
    yval = (ypos - mean(ypos))*1.5
  ) %>%
  select(-xpos,-ypos)

# make the brain map
brain_map <- here::here("brain_map.csv") %>%
  read_csv() %>%
  mutate(
    id = paste0("dot",id), 
    frame = 4,
    xval = (xpos - mean(xpos))*1.5, 
    yval = (ypos - mean(ypos))*1.5) %>%
  select(-xpos,-ypos)
  
# add the network map & brain maps as frames
msg_map <- bind_rows(msg_map, net_map, brain_map) %>%
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
# every jump in the brownian bridge from...

# frame 1 to frame 3
msg_walk1 <- msg_map %>% 
  filter(frame %in% c(1,3)) %>%
  group_by(id) %>%
  summarise(
    path = brownian_path(id, xval, yval, nsteps, scale)
  ) %>% 
  pull(path) %>% 
  reduce(bind_rows)

# frame 3 to frame 2
msg_walk2 <- msg_map %>% 
  filter(frame %in% c(2,3)) %>%
  mutate(frame = -frame) %>%
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

# frame 2 to frame 4
msg_walk3 <- msg_map %>% 
  filter(frame %in% c(2,4)) %>%  
  arrange(id, frame) %>%
  group_by(id) %>%
  summarise(
    path = brownian_path(id, xval, yval, nsteps, scale)
  ) %>% 
  pull(path) %>% 
  reduce(bind_rows) %>%
  mutate(
    frame = frame + 2*nsteps
  )

# frame 4 to frame 1
msg_walk4 <- msg_map %>% 
  filter(frame %in% c(1,4)) %>%  
  mutate(frame = -frame) %>%
  arrange(id, frame) %>%
  group_by(id) %>%
  summarise(
    path = brownian_path(id, xval, yval, nsteps, scale)
  ) %>% 
  pull(path) %>% 
  reduce(bind_rows) %>%
  mutate(
    frame = frame + 3*nsteps
  )

# combine
msg_walk <- bind_rows(
  msg_walk1, msg_walk2, msg_walk3, msg_walk4
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
  pic <- base_pic + 
    facet_wrap(~frame) + 
    theme_bw()
  plot(pic)
  
} else {
  
  # construct animation
  pic <- base_pic +
    transition_states(
      states = frame,
      transition_length = 2,
      state_length = rep(c(2.5,1,1,1,2.5), 4),
      wrap = TRUE
      )  +
    ease_aes('linear') +
    shadow_wake(.1)
  
  # create animation
  pic %>% animate(
    nframes = 500, fps = 20, width = 800)
  anim_save(here::here("full_animation.gif"))
}






