library(reshape2)
library(broom)
options(warn=-1)
names(final)
final1 <- final %>% 
  filter(mau_previous_month>0) %>%  #remove 0 data
  arrange(desc(monthly_stream30s)) %>% 
  head(10000) #keep top 10000 rows

final.select <- final1 %>% 
  select(playlist_uri,genre_1,mood_1)  #keep genre & mood 1

dummies <- dcast( melt(final.select,id.var="playlist_uri"), playlist_uri ~ value, length) #make dummy data out of genre/mood

#join dummy data back to original
df_d <-  final %>% 
  select(-c(genre_1:mood_3)) %>% 
  inner_join(dummies)

d3 <- df_d %>% 
  select(mau,mau_previous_month,Alternative:Yearning) %>%
   dplyr::rename(#Easy_Listening = `Easy Listening`,
         Childrens = `Children's`,
         Country_Folk = `Country & Folk`,
         Dance_House = `Dance & House`,
         Indie_Rock = `Indie Rock`,
         New_Age = `New Age`,
         R_B = `R&B`,
         Spoken_Audio = `Spoken & Audio`)

# run exploratory regression 
m1 <- lm(log(mau) ~  log(mau_previous_month) + (.- mau_previous_month)^2 ,data=d3)
summary(m1)

#keep signif variables
m1 %>% 
  tidy() %>% 
  filter(p.value < .05) %>%
  write_csv('prelim-model.csv')



