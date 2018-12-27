library(here)
library(tidyverse)
library(e1071)
library(gganimate)

# take a vector of inputs and return a binary 
# matrix corresponding to the character
read_letter <- function(...) {
  cbind(
    matrix(data = c(...), nrow = 9, ncol = 5, byrow = TRUE),
    matrix(data = rep(0,9), nrow = 9, ncol = 1)
  )
}


# read the character list
source(here("bridge3_wearitpurple","char-list.R"))

# coerce to a tibble
char_table <- char_list %>% 
  imap_dfr(function(x,n) {
    x %>% as.tibble() %>%
      mutate(ypos = 9:1) %>%
      gather(key = "xpos", value = "value", V1, V2, V3, V4, V5, V6) %>%
      mutate(xpos = xpos %>% str_remove("V") %>% as.integer()) %>%
      mutate(char = n)
  }) 


# message to plot
msg <- list(
  frame1 = c("Be Kind", "   Be You", "      Be Awesome"),
  frame2 = c("Wear It Purple", "  31/08/2018")
)

# convert the message to a tidy form
msg_frame <- msg %>% 
  imap_dfr(
    function(m, v) {  # apply to each frame:
      m %>%
        str_split("") %>% # split into a list (one element per "line")
        imap_dfr(
          function(x, v) { # apply to each line
            x %>% 
              as.tibble() %>% 
              rename(char = value) %>%
              mutate(
                xoff = 6 * (1:n()), # scaled x-offset for each character
                yoff = v            # unscaled y-offset for each character
              )
          }) %>% 
        mutate(
          yoff = 10 * (max(yoff) - yoff),  # scale the y-offset
          frame = v %>% str_remove("frame") %>% as.integer() # append frame info
        )
    })

# left join with the character table to produce 
# co-ordinate map for the raw points in each frame
msg_map <- left_join(msg_frame, char_table) %>%
  filter(value == 1)

# read the message map
msg_map <- msg_map %>%
  mutate(xval = xpos + xoff, yval = ypos + yoff) %>%
  arrange(frame, xval, yval) %>%
  select(char, frame, xval, yval)

# dot count
dot_count <-  msg_map %>% group_by(frame) %>% 
  summarise(count = n())
print(dot_count)

# give each dot a unique identity
msg_map$id <- dot_count$count %>%
  map(function(x){1:x}) %>% reduce(c)

# keep only the dot label, position, and frame
msg_map <- msg_map %>% 
  select(id, frame, xval, yval) %>% 
  arrange(id, frame)

# parameters
nsteps <- 6
scale <- 8

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
) %>% arrange(frame,id,xval,yval)

# add lgbt palette
lgbt <- c(
  `1` = "#E70000", # Electric Red 
  `2` = "#FF8C00", # Dark Orange 
  `3` = "#FFEF00", # Canary Yellow 
  `4` = "#00811F", # La Salle Green 
  `5` = "#0044FF", # Blue (RYB) 
  `6` = "#760089"  # Patriarch 
)

# discrete values for the colours
msg_walk <- msg_walk %>%
  mutate(
    base_colour = xval %>% cut(breaks = 6) %>% as.numeric(),
    unbounded_colour = case_when(
      frame <= 6 ~ base_colour + (frame - 1),
      frame > 6 ~ base_colour + (12-frame)
    ),
    colour = case_when(
      unbounded_colour > 6 ~ 6,
      TRUE ~ unbounded_colour
    )
  )

background <- "grey90"

# create the base plot
base_pic <- msg_walk %>%
  ggplot(aes(
    x = xval,
    y = yval,
    colour = factor(colour), 
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
  scale_color_manual(values = lgbt)

# construct animation
pic <- base_pic +
  transition_states(
    states = frame,
    transition_length = 2,
    state_length = c(2.5,0,0,0,0,2.5,2.5,0,0,0,0,2.5),
    wrap = TRUE
  )  +
  ease_aes('sine-in-out') +
  shadow_wake(.1)

# create animation
pic %>% animate(
  nframes = 300, fps = 15, width = 800, 
  bg = background, detail = 3, type = "cairo"
)
anim_save(here("bridge3_wearitpurple","wear-it-purple.gif"))




