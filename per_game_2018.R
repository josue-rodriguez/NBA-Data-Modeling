pacman::p_load(dplyr, ggplot2, tidyr, DataExplorer,ggfortify, ggrepel)

df <- read.csv("~/Desktop/per_game_2018.csv")

df_clean <- df %>%
  mutate(player = as.character(player),
         pos = as.character(pos),
         pos = case_when(pos == "SF-SG" ~ "SF",
                         pos == "PG-SG" ~ "PG",
                         TRUE ~ pos)) %>%
  separate(player, into = c("first_name", "last_name"), sep = " ", extra = "merge") %>%
  replace(is.na(.), 0) %>% 
  select(-X)
rownames(df_clean) <- paste(df_clean$first_name, df_clean$last_name)
#### ---- Data Explorer
plot_str(df_clean)
plot_histogram(df_clean)
plot_correlation(df_clean)



by_position <- df_clean %>% 
  group_by(pos) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-id)

by_team <- df_clean %>% 
  group_by(team_id) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-id)

easy_graph <- function(data, x, y){
  ggplot(data, aes_string(x = x, y = y)) +
    geom_col(alpha = 0.8)
}
# df_clean %>% 
#   filter_all(any_vars(. == 0)) %>% 
#   View()



##### -------- PCA
num <- df_clean %>% 
  select_if(is.numeric)
pca <- prcomp(num, scale=TRUE)

autoplot(pca, size = 1, 
         loadings = TRUE, loadings.label = TRUE, 
         label = TRUE, label.alpha = 0.4) +
  theme_bw()

var <- pca$sdev^2

var_exp <- var / sum(var)
qplot(seq_along(var_exp), var_exp, geom = "point") +
  geom_line() +
  labs(x = "Principal Component", y = "% Variance Explained") +
  theme_bw()

cum_var_exp <- cumsum(var_exp)
qplot(seq_along(cum_var_exp), cum_var_exp, geom = "point") +
  geom_line() +
  labs(x = "Principal Component", y = "Cumulative % Variance Explained") +
  theme_bw()


###### ------ KMeans

kmeans_wss <- rep(NA, 7)

for (i in 1:7){
  nba_km <- kmeans(pca$x, centers = i, nstart = 20)
  kmeans_wss[i] <- nba_km$tot.withinss
}

qplot(1:7, kmeans_wss, geom = "point") +
  geom_line() +
  labs(x = "# of clusters", y = "Total Within SS") +
  theme_bw()

nba_clusters <- kmeans(pca$x, centers = 4, nstart = 20)

##### --- plotting clusters

plot_pca <- pca$x %>% 
  as_tibble() %>% 
  select(PC1, PC2) %>% 
  mutate(cluster = nba_clusters$cluster,
         cluster = as.factor(cluster), 
         player = rownames(df_clean))

ggplot(plot_pca, aes(x = PC1, y = PC2, col = cluster)) +
  geom_point() +
  geom_text(aes(label = player), size = 3, alpha = 0.6, nudge_y = 0.1) +
  theme_bw()
