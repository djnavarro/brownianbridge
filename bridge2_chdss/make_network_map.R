library(tidyverse)

net <- here::here("network_pic.csv") %>%
  read_csv(col_names = FALSE) %>%
  mutate(ypos = 1:n()) %>%
  gather(key = "xpos", value = "value", -ypos) %>%
  filter(value == 1) %>%
  mutate(xpos = xpos %>% str_remove("X") %>% as.numeric) %>%
  select(xpos, ypos)

net %>% write_csv("network_map.csv")

net %>% 
  mutate(id = 1:n()) %>%
  ggplot(aes(x=xpos, y=ypos)) +
  geom_point(
    aes(colour = factor(id)),
    show.legend = FALSE)
