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
c <- corrplot(correlations,order="hclust") #show corrleations for fastballs
highCorr <- findCorrelation(correlations,cutoff=.7,names=TRUE) #find highly correlated predictors
highCorr #everything is correlated!
pdf(file = "images/corrplot.pdf")

corrplot(c, 
         title = "Spotify Playlist Correlations")

dev.off()
hist(df$stream30s) #dependent variable heavily skewed

df1 <- df %>% arrange(desc(stream30s)) %>% head(2000) #take a look at top 2000 rows
hist(df1$stream30s) #still skewed
hist(log(df1$stream30s)) #still skewed
hist(df1$mau) #try monthly actives
hist(log(df1$mau)) #fairly normal

#make some stats
df_1 <- df1 %>% 
  mutate(trending = dau/(mau-dau),hot = wau/(mau-wau),rising=mau/(mau+mau_previous_month),
         skip=skippers/users,change_rate=n_local_tracks/n_tracks)

#keep independent variables and use mau as dependent variable
df_select <- df_1 %>% 
  select(mau,n_tracks,n_artists,n_albums,tokens:change_rate)
names(df_select)
#Run a Mixed Model to Predict MAU
m1 <- lmer(mau ~ scale(n_tracks) + scale(n_artists) + scale(trending) + 
             scale(rising)  
           + scale(skip) + scale(change_rate) +
             + (1|genre_1) +
             (1|mood_1),data=df_select)
summary(m1) # not a whole lot there
library(ggeffects)
# Extract the prediction data frame - this is pointless but lets just see whats here
pred.mm <- ggpredict(m1)  # this gives overall predictions for the model

#fit the data
df_m <- df_select %>% 
  predict(m1, newdata = .) %>% 
  bind_cols(df_select) %>% 
  rename(fit=...1)
View(df_m)
summary(df_m)

# Not very interesting model fits 
df_m %>% 
  group_by(genre_1) %>% 
  summarise(fit=mean(fit),mau=mean(mau)) %>% 
  mutate(resid=fit-mau) %>% 
  arrange(desc(resid)) %>% 
  View()

df_m %>% 
  group_by(mood_1) %>% 
  summarise(fit=mean(fit),mau=mean(mau)) %>% 
  mutate(resid=fit-mau) %>% 
  arrange(desc(resid)) %>% 
  View()
