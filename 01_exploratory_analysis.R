#load some packages
library(tidyverse)

library(corrplot)
library(caret)
library(randomForest)
library(lme4)
#read the data in
df <- read_delim("data/playlist_summary_external.txt", delim = "\t")

#do some correlations
cor_df <- df %>% select(streams:skippers, n_tracks:monthly_owner_stream30s)
correlations <- cor(cor_df)
corrplot(correlations,order="hclust") #show corrleations for fastballs
highCorr <- findCorrelation(correlations,cutoff=.7,names=TRUE) #find highly correlated predictors
highCorr #everything is correlated!
summary(df)
hist(df$stream30s) #dependent variable heavily skewed

df1 <- df %>% arrange(desc(stream30s)) %>% head(2000) #take a look at top 1000 rows
hist(df1$stream30s) #still skewed
hist(log(df1$stream30s)) #still skewed
hist(df1$mau) #try monthly actives
hist(log(df1$mau)) #fairly normal

#make some stats
df_1 <- df1 %>% 
  mutate(trending = dau/mau,hot = wau/mau,rising=mau/(mau+mau_previous_month),
         skip=skippers/users,change_rate=n_local_tracks/n_tracks)

#keep independent variables and use mau as dependent variable
df_select <- df_1 %>% 
  select(mau,n_tracks,n_artists,n_albums,tokens:change_rate)

#Run a Mixed Model to Predict MAU

m1 <- lmer(mau ~ n_tracks + n_artists + this_week + this_month 
           + skip + change_rate +
             + (1|genre_1/genre_2/genre_3) +
             (1|mood_1/mood_2/mood_3),data=df_select)
summary(m1) # not a whole lot there
library(ggeffects)
# Extract the prediction data frame - this is pointless but lets just see whats here
pred.mm <- ggpredict(m1)  # this gives overall predictions for the model
df_m <- df_select %>% 
  predict(m1, newdata = .) %>% 
  bind_cols(df_select) %>% 
  rename(fit=...1)

summary(df_m)
df_m %>% 
  filter(fit > 13937) %>% 
  group_by(genre_1) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

df_m %>% 
  filter(fit > 13937) %>% 
  group_by(mood_1) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))
