#load some packages
library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(lme4)
library(stringr)
dim(df)
#read the data in
df <- read_delim("data/playlist_summary_external.txt", delim = "\t")

df <- df %>% 
  mutate(trending = dau/mau,hot = wau/mau,rising=mau/(mau+mau_previous_month),
         skip=skippers/users,change_rate=n_local_tracks/n_tracks)
View(df)
df %>% 
  group_by(stream30s) %>% 
  count() %>% 
  arrange(stream30s) %>% View()
  
d <- df %>% 
  filter(stream30s > 100)
names(d)
d1 <- d %>% 
  mutate(rank = dense_rank(desc(stream30s))) %>% 
  filter(rank<=500) %>% 
  mutate(tier = ifelse(rank<101, "Top 100",
                     "Top 101-500"))

d1$tokens <- gsub( "[^,a-zA-Z\\s]" , "" , d1$tokens , perl = TRUE )


final <- d1 %>% separate(tokens, c("token_1","token_2","token_3")) 
names(final)
final %>% write_csv("spotify.csv")

