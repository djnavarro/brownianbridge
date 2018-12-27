library(here)
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
  theme_void() +
  transition_time(time = Time)  +
  ease_aes('linear') + 
  shadow_wake(.1)
  
  
# create
pic %>% animate(nframes = 200, detail = 5, type = "cairo")
anim_save(here("bridge1_simple", "bridge.gif"))
