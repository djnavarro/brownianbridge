library(tidyverse)

char_table <- read_csv(here::here("char_map.csv"))

message <- c("Complex Human Data","Summer School") %>% 
  str_split("") %>% 
  imap_dfr(function(x,v) {
    x %>% 
      as.tibble() %>% 
      rename(char = value) %>%
      mutate(
        xoff = 6 * (1:n()),
        yoff = v
      )
  }) %>% 
  mutate(yoff = 10 * (max(yoff) - yoff))

msg_map <- left_join(message, char_table) %>%
  filter(value == 1)

pic <- msg_map %>% 
  ggplot(aes(
    x = xpos + xoff, 
    y = ypos + yoff)) + 
  geom_point() + 
  coord_equal()

plot(pic)
