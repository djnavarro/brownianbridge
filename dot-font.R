library(tidyverse)
library(broom)

# take a vector of inputs and return a binary 
# matrix corresponding to the character
read_letter <- function(...) {
  matrix(data = c(...), nrow = 7, ncol = 5, byrow = TRUE)
}

# for visual clarity only
. <- 0 

# based on BPdots
char_list <- list(
  " " = read_letter(
    .,.,.,.,.,
    .,.,.,.,.,
    .,.,.,.,.,
    .,.,.,.,.,
    .,.,.,.,.,
    .,.,.,.,.,
    .,.,.,.,.),
  "A" = read_letter(
    .,1,1,1,.,
    1,.,.,.,1,
    1,.,.,.,1,
    1,.,.,.,1,
    1,1,1,1,1,
    1,.,.,.,1,
    1,.,.,.,1)
  ,
  "B" = read_letter(
    1,1,1,1,.,
    1,.,.,.,1,
    1,.,.,.,1,
    1,1,1,1,.,
    1,.,.,.,1,
    1,.,.,.,1,
    1,1,1,1,.),
  "C" = read_letter(
    .,1,1,1,.,
    1,.,.,.,1,
    1,.,.,.,.,
    1,.,.,.,.,
    1,.,.,.,.,
    1,.,.,.,1,
    .,1,1,1,.),
  "D" = read_letter(
    1,1,1,1,.,
    1,.,.,.,1,
    1,.,.,.,1,
    1,.,.,.,1,
    1,.,.,.,1,
    1,.,.,.,1,
    1,1,1,1,.),
  "E" = read_letter(
    1,1,1,1,1,
    1,.,.,.,.,
    1,.,.,.,.,
    1,1,1,1,.,
    1,.,.,.,.,
    1,.,.,.,.,
    1,1,1,1,1),
  "F" = read_letter(
    1,1,1,1,1,
    1,.,.,.,.,
    1,.,.,.,.,
    1,1,1,1,.,
    1,.,.,.,.,
    1,.,.,.,.,
    1,.,.,.,.),
  "G" = read_letter(
    .,1,1,1,.,
    1,.,.,.,1,
    1,.,.,.,.,
    1,.,.,.,.,
    1,.,.,1,1,
    1,.,.,.,1,
    .,1,1,1,1)
)

# coerce to a tibble
char_table <- char_list %>% 
  imap_dfr(function(x,n) {
    x %>% as.tibble() %>%
      mutate(ypos = 7:1) %>%
      gather(key = "xpos", value = "value", V1, V2, V3, V4, V5) %>%
      mutate(xpos = xpos %>% str_remove("V") %>% as.integer()) %>%
      mutate(char = n)
  }) 

# write file
write_csv(char_table, here::here("char_map.csv"))

# sanity check
char_table %>% 
  filter(char == "A" & value == 1) %>%
  ggplot(aes(x=xpos, y=ypos)) + 
  geom_point(size = 10) + 
  coord_equal() + 
  xlim(0, 6) + 
  ylim(0, 8)
