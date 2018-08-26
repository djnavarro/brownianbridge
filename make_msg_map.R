library(tidyverse)

# read the character map
char_table <- read_csv(here::here("char_map.csv"))

# message to plot
msg <- list(
  frame1 = c("Summer School:", "Complex Human Data"),
  frame2 = c("Melbourne, Australia", "Dec 9-14, 2018")
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

write_csv(msg_map, here::here("msg_map.csv"))

msg_map %>% group_by(frame) %>% 
  summarise(count = n()) %>% print()


