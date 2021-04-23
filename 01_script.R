library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)

df <- read_delim("data/playlist_summary_external.txt", delim = "\t")

cor_df <- df %>% select(streams:skippers, n_tracks:monthly_owner_stream30s)
correlations <- cor(cor_df)

corrplot(correlations,order="hclust") #show corrleations for fastballs
highCorr <- findCorrelation(correlations,cutoff=.7,names=TRUE) #find highly correlated predictors
highCorr 
summary(df)

df1 <- df %>% arrange(desc(stream30s)) 
dim(df1)
library(lme4)
df_1 <- df1 %>% 
  mutate(today = dau/mau,this_week = wau/mau,this_month=mau/(mau+mau_previous_month),
         skip=skippers/users,change_rate=n_local_tracks/n_tracks)

names(df_1)
df_select <- df_1 %>% 
  select(mau,n_tracks,n_artists,n_albums,tokens:change_rate)
names(df_select)
m1 <- lmer(mau ~n_tracks + n_artists + this_week + this_month + skip + change_rate +
             + tokens + (1|genre_1/genre_2/genre_3) +
             (1|mood_1/mood_2),data=df_select)
summary(m1)
library(ggeffects)
# Extract the prediction data frame
pred.mm <- ggpredict(m1)  # this gives overall predictions for the model
df_select %>% 
  predict(m1, newdata = .) %>% bind_cols(df_select) %>% View()

natural_ctrl <- trainControl(method = "repeatedcv",number=10,repeats=10,
                             savePredictions = TRUE,allowParallel = TRUE,
                             summaryFunction = twoClassSummary,
)

rfGrid <- data.frame(mtry = c(2, 3, 4, 5))



names(df_select)
df_select$genre_1 <- as.factor(df_select$genre_1)

#tuning controls and then model fits for Random Forest and GAM
rf_fit <- train(mau ~ ., data = df_select, 
                method = "rf", importance=TRUE,ntree=500,
                trControl = natural_ctrl,metric="accuracy")
