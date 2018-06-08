library(dplyr)
library(ggplot2)
library(tidyr)

df <- read.csv("~/Desktop/per_game_2018.csv")

df_clean <- df %>% 
  rename(id = X) %>% 
  mutate(player = as.character(player)) %>% 
  separate(player, into = c("first_name", "last_name"), sep = " ", extra = "merge")

by_position <- df_clean %>% 
  group_by(pos) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-id)

by_team <- df_clean %>% 
  group_by(team_id) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>% 
  select(-id)

easy_graph <- function(data, x, y){
  ggplot(data, aes_string(x = x, y = y)) +
    geom_col(alpha = 0.8)
}

by_team %>% 
  order(age)
  easy_graph("team_id", "age")
