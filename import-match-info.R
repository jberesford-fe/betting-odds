library(rjson)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(data.table)
library(tidymodels)

rm(list=ls())

files <- list.files(path = 'raw-data/')%>%head(5000)
match_ids <- str_remove_all(files,'.json')
file_paths<-paste("raw-data",files,sep = '/')

raw_list <- lapply(file_paths,function(x) fromJSON(file = x))

info_all<-sapply(raw_list,'[','info')
names(info_all)<-match_ids


df<-data.table(info_all)%>%
  unnest_wider(info_all)%>%
  unnest_wider(event)%>%
  unnest_wider(officials)%>%
  select(-umpires,-players,-toss,-registry)%>%
  unnest_wider(outcome)%>%
  unnest_wider(by)

df<-df%>%
  mutate(date_start=sapply(df$dates,min),
         date_end=sapply(df$dates,max))%>%
  select(-dates)%>%
  unnest_wider(teams)%>%
  rename('home_team'=`...1`)%>%
  rename('away_team'=`...2`)%>%
  filter(is.na(result))%>%
  mutate(home_wins=winner==home_team)%>%
  mutate(runs=if_else(home_wins,runs,-runs),
         wickets=if_else(home_wins,wickets,-wickets),
         innings=if_else(home_wins,innings,-innings))

splits<-initial_split(df,prop = 0.5,start=home_wins)

df_train <- splits %>% training()

# Get summary stats
home_stats <- df_train %>%
  group_by(home_team)%>%
  summarise(win_rate=mean(home_wins,na.rm=TRUE),
          mean_runs=mean(runs,na.rm=TRUE),
          mean_wickets=mean(wickets,na.rm=TRUE),
          mean_innings=mean(innings,na.rm=TRUE),
          games=n())%>%
  ungroup()


away_sum_stats<-home_stats%>%
  rename(away_team=home_team,
         away_win_rate=win_rate,
         away_mean_runs=mean_runs,
         away_mean_wickets=mean_wickets,
         away_mean_innings=mean_innings)
         

df_test <- splits %>% testing()%>%
  mutate(home_wins=as.factor(home_wins))%>%
  full_join(home_stats,by="home_team")%>%
  full_join(away_sum_stats,by="away_team")

df_train <- df_train %>%
  mutate(home_wins=as.factor(home_wins))%>%
  full_join(home_stats,by="home_team")%>%
  full_join(away_sum_stats,by="away_team")


colnames(df_train)

fitted_logit <- logistic_reg()%>%
  set_engine("glm")%>%
  set_mode("classification")%>%
  fit(data=df_train,home_wins~mean_runs+away_mean_runs+mean_wickets+away_mean_wickets+
        mean_innings+away_mean_innings)

tidy(fitted_logit)

pred_win <- predict(fitted_logit,
                      new_data = df_test,
                      type = "class")

summary_results<-df_test%>%
  select(home_wins)%>%
  bind_cols(pred_win)


conf_mat(data = summary_results, truth = home_wins,
         estimate = .pred_class)
accuracy(summary_results,truth = home_wins,
         estimate = .pred_class)
sens(summary_results,truth = home_wins,
         estimate = .pred_class)
spec(summary_results,truth = home_wins,
     estimate = .pred_class)
precision(summary_results,truth = home_wins,
     estimate = .pred_class)


df%>%
  head()
