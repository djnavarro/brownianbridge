library(here)
library(tidyverse)
library(e1071)
library(gganimate)
library(viridis)

ascii_bridge <- "
.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.
.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,.,1,.,.,.,.,1,.,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.
.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.
.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.
.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,.,.,.,.,1,.,.,.,1,1,1,1,1,1,1,1,.,.,.,1,.,.,.,.,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.
.,1,1,1,1,.,.,.,.,.,.,1,.,.,1,.,.,.,.,1,1,1,1,.,1,.,.,.,.,1,.,1,1,1,1,.,.,.,.,1,.,.,1,.,.,.,.,.,.,1,1,1,1,.
1,1,1,1,1,1,.,.,.,1,1,.,.,.,1,.,.,1,1,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,1,1,.,.,1,.,.,.,1,1,.,.,.,1,1,1,1,1,1
1,1,.,.,1,1,.,.,1,.,1,.,.,.,1,1,1,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,1,1,1,.,.,.,1,.,1,.,.,1,1,.,.,1,1
1,1,.,.,1,1,.,1,.,.,1,.,.,1,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,1,.,.,1,.,.,1,.,1,1,.,.,1,1
1,1,.,.,1,1,1,.,.,.,1,.,1,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,.,.,.,1,.,1,.,1,.,.,.,1,1,1,.,.,1,1
1,1,.,.,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,.,.,1,1
1,1,.,.,1,1,.,.,.,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,.,.,.,1,1,.,.,1,1
1,1,1,1,1,1,.,.,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,.,.,1,1,1,1,1,1
1,1,1,1,1,1,.,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,.,1,1,1,1,1,1
1,1,1,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,1,1,1
1,1,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,1,1
1,1,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,1,1
1,1,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,1,1
1,1,1,1,1,1,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,1,1,1,1,1,1
"

# read the bridge
bridge <- read_csv(ascii_bridge, col_names = FALSE) %>%
  mutate(yval = 19:1) %>% 
  gather(key = "xval", value = "value", -yval) %>%
  mutate(xval = xval %>% str_remove("X") %>% as.integer()) %>%
  filter(value != ".") %>% 
  select(-value) %>% 
  mutate(
    frame = 2,
    id = 1:n()
  ) %>%
  arrange(xval, yval)

# everything starts at the bottom
water <- bridge %>% 
  mutate(
    frame = 1,
    yval = 0 
  )

# everything ends at the top
sky <- bridge %>% 
  mutate(
    frame = 3,
    yval = 25 
  )

# put it together
bridge_map <- bind_rows(water, bridge, sky)

# parameters
nwalk <- 30
scale <- c(x = 3, y = .1)
nsteps <- 250
npause <- 10

# function to convert start and end points to
# a meandering path using brownian bridge
brownian_path <- function(id, xval, yval) {
  
  time_start <- npause + sample(nsteps- (2 * npause) - nwalk, 1) 
  time_stop <- time_start + nwalk
  
  # initial
  onset <- tibble(
    id = id[1],
    frame = 1:time_start ,
    xval = xval[1],
    yval = yval[1]
  )
    
  # expand from two end points to a straight line path
  walk <- tibble(
    id = id[1],
    frame = time_start + (1:nwalk),
    xval = seq(xval[1], xval[2], length.out = nwalk),
    yval = seq(yval[1], yval[2], length.out = nwalk)
  )
  
  # locally smoothed
  meander <- function(n) {
    smooth <- function(w) {
      (w + c(w[-1], 0) + c(0,w[-length(w)])) / 3
    }
    w <- c(0, rbridge(1, nwalk-1)) %>%
      smooth() %>%
      smooth()
    return(w)
  }
  
  # create a meandering path using rbridge
  walk$xval <- walk$xval + meander(nwalk) * scale["x"]
  walk$yval <- walk$yval + meander(nwalk) * scale["y"]
  
  # initial
  offset <- tibble(
    id = id[1],
    frame = (time_stop + 1):nsteps,
    xval = xval[2],
    yval = yval[2]
  )
  
  path <- bind_rows(
    onset, walk, offset
  )
  
  # return as a single element list
  return(list(p=path))
}



# construct a tibble tracking all points from water to bridge
bridge_walk_forward <- bridge_map %>% 
  group_by(id) %>% 
  filter(frame %in% 1:2) %>%
  summarise(
    path = brownian_path(id, xval, yval)
  ) %>% 
  pull(path) %>% 
  reduce(bind_rows)

# do the same thing to walk up to the sky
bridge_walk_backward <- bridge_map %>% 
  group_by(id) %>%
  filter(frame %in% 2:3) %>%  
  summarise(
    path = brownian_path(id, xval, yval)
  ) %>% 
  pull(path) %>% 
  reduce(bind_rows) %>%
  mutate(
    frame = frame + nsteps
  )

# combine
bridge_walk <- bind_rows( 
    bridge_walk_forward, bridge_walk_backward
  ) %>% 
  arrange(frame, id, xval, yval) 


background <- "grey90"

# create the base plot
base_pic <- bridge_walk %>%
  ggplot(aes(
    x = xval,
    y = yval,
    colour = id,
    group = id)) +
  geom_point(show.legend = FALSE,
             size = 4, alpha = .6) +
  coord_equal() +
  theme_void() + 
  theme(
    panel.background = element_rect(
      fill = background, colour = background, 
      size = 0.5, linetype = "solid"
    )
  ) + 
  ylim(-.5, 24.5) + 
  scale_color_viridis(begin = .1, end = .9)

# construct animation
pic <- base_pic +
  transition_time(time = frame)  +
  ease_aes('linear') +
  shadow_wake(.035, falloff = "exponential-in-out")
  
# create animation
pic %>% animate(
  nframes = 500, fps = 15, width = 800, 
  bg = background, detail = 3
)
anim_save(here("bridge4_eels", "bridge-of-eels.gif"))

