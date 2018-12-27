library(tidyverse)

brain <- here::here("brain_pic.csv") %>%
  read_csv(col_names = FALSE) %>%
  mutate(ypos = 1:n()) %>%
  gather(key = "xpos", value = "id", -ypos) %>%
  filter(!(is.na(id))) %>%
  mutate(
    xpos = xpos %>% str_remove("X") %>% as.numeric(),
    ypos = 40-ypos) %>%
  arrange(id) %>%
  select(id,xpos,ypos)
  
brain %>% write_csv("brain_map.csv")

brain %>% 
  ggplot(aes(x=xpos, y=ypos)) +
  geom_point(
    aes(colour = factor(id)),
    show.legend = FALSE)

