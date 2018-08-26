library(tidyverse)

char_table <- read_csv(here::here("char_map.csv"))

msg <- list(
  frame1 = c("Complex Human Data","Summer School"),
  frame2 = c("Melbourne 2018", "December 9-14")
)

msg_frame <- msg$frame2 %>% 
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

msg_map <- left_join(msg_frame, char_table) %>%
  filter(value == 1)

pic <- msg_map %>% 
  ggplot(aes(
    x = xpos + xoff, 
    y = ypos + yoff)) + 
  geom_point() + 
  coord_equal()

plot(pic)
