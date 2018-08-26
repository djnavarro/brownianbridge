


char_table <- read_csv(here::here("char_map.csv"))

message <- "HELLO" %>% 
  str_split("") %>% first() %>% 
  as.tibble() %>% 
  rename(char = value) %>%
  mutate(xoff = 6 * (1:n()))

msg_map <- left_join(message, char_table) %>%
  filter(value == 1)

pic <- msg_map %>% 
  ggplot(aes(x=xpos + xoff, y=ypos)) + 
  geom_point(size = 5) + 
  coord_equal() + 
  ylim(0, 10)

plot(pic)
