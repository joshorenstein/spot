#load some packages
library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(lme4)
library(stringr)

#read the data in
d <- read_delim("data/playlist_summary_external.txt", delim = "\t")

#rank based on monthly streams over 30 seconds
df <- d %>% 
  mutate(rank = rank(-monthly_stream30s,ties.method="min")) %>% 
  filter(rank<=10000) %>% 
  mutate(tier = ifelse(rank<101, "Top 100",
                ifelse(rank>500&rank<=1000,"Top 501-1000",
                ifelse(rank<501&rank>100,"Top 101-500",
                       "Bottom Tier"))))

#View(df)
#make some stats
names(df)
df <- df %>% 
  mutate(trending = (dau/(mau))*100,
         hot = (wau/(mau))*100,
         rising=(mau/(mau+mau_previous_month))*100,
         mo_chg = ifelse(mau_previous_month!=0,
                         mau-mau_previous_month,NA),
         skip=skippers/users,
         change_rate=n_local_tracks/n_tracks)

df$tokens <- gsub( "[^,a-zA-Z\\s]" , "" , df$tokens , perl = TRUE ) #clean the tokens column

final <- df %>% separate(tokens, c("token_1","token_2","token_3")) #split out the first 3 tokens
#View(final)
final %>% write_csv("spotify.csv") #push to csv

