rm(list=ls())

all_matches<-readRDS("all_matches_5k.rds")

all_matches <- all_matches%>%
  group_by(home_team)%>%
  filter(n()>25)%>%
  ungroup()
#####Drop unneeded and make factors into dummy variables #####
# Temp, deleted from other bit
backdata_sum_stats<-all_matches%>%
  arrange(date_start)%>%
  group_by(home_team)%>%
  mutate(home_batter_runs_mean_last1=lag(home_batter_runs_mean,1),
         home_batter_runs_sum_last1=lag(home_batter_runs_sum,1),
         away_batter_runs_mean_last1=lag(away_batter_runs_mean,1),
         away_batter_runs_sum_last1=lag(away_batter_runs_sum,1),
         
         home_extra_runs_sum_last1=lag(home_extra_runs_sum,1),
         home_extra_runs_mean_last1=lag(home_extra_runs_mean,1),
         away_extra_runs_sum_last1=lag(away_extra_runs_sum,1),
         away_extra_runs_mean_last1=lag(away_extra_runs_mean,1),
         home_wickets_mean_last1=lag(home_wickets_mean,1),
         home_wickets_sum_last1=lag(home_wickets_sum,1),
         away_wickets_mean_last1=lag(away_wickets_mean,1),
         away_wickets_sum_last1=lag(away_wickets_sum,1))%>%
  
  mutate(home_batter_runs_mean_last5=lag(rollmean(home_batter_runs_mean,5,
                                                  align = "right",na.pad = TRUE),1),
         home_batter_runs_sum_last5=lag(rollmean(home_batter_runs_sum,5,
                                                  align = "right",na.pad = TRUE),1),
         away_batter_runs_mean_last5=lag(rollmean(away_batter_runs_mean,5,
                                                 align = "right",na.pad = TRUE),1),
         away_batter_runs_sum_last5=lag(rollmean(away_batter_runs_sum,5,
                                                 align = "right",na.pad = TRUE),1),
         home_extra_runs_sum_last5=lag(rollmean(home_extra_runs_sum, 5,
                                                  align = "right",na.pad = TRUE),1),
         home_extra_runs_mean_last5=lag(rollmean(home_extra_runs_mean, 5,
                                                  align = "right",na.pad = TRUE),1),
         away_extra_runs_sum_last5=lag(rollmean(away_extra_runs_sum, 5,
                                                  align = "right",na.pad = TRUE),1),
         away_extra_runs_mean_last5=lag(rollmean(away_extra_runs_mean, 5,
                                                  align = "right",na.pad = TRUE),1),
         home_wickets_mean_last5=lag(rollmean(home_wickets_mean, 5,
                                                  align = "right",na.pad = TRUE),1),
         home_wickets_sum_last5=lag(rollmean(home_wickets_sum, 5,
                                                  align = "right",na.pad = TRUE),1),
         away_wickets_mean_last5=lag(rollmean(away_wickets_mean, 5,
                                                  align = "right",na.pad = TRUE),1),
         away_wickets_sum_last5=lag(rollmean(away_wickets_sum, 5,
                                                  align = "right",na.pad = TRUE),1))%>%
  
  mutate(home_batter_runs_mean_last20=lag(rollmean(home_batter_runs_mean,20,
                                                  align = "right",na.pad = TRUE),1),
         home_batter_runs_sum_last20=lag(rollmean(home_batter_runs_sum,20,
                                                 align = "right",na.pad = TRUE),1),
         away_batter_runs_mean_last20=lag(rollmean(away_batter_runs_mean,20,
                                                  align = "right",na.pad = TRUE),1),
         away_batter_runs_sum_last20=lag(rollmean(away_batter_runs_sum,20,
                                                  align = "right",na.pad = TRUE),1),
         home_extra_runs_sum_last5=lag(rollmean(home_extra_runs_sum, 20,
                                                  align = "right",na.pad = TRUE),1),
         home_extra_runs_mean_last5=lag(rollmean(home_extra_runs_mean, 20,
                                                  align = "right",na.pad = TRUE),1),
         away_extra_runs_sum_last5=lag(rollmean(away_extra_runs_sum, 20,
                                                  align = "right",na.pad = TRUE),1),
         away_extra_runs_mean_last5=lag(rollmean(away_extra_runs_mean, 20,
                                                  align = "right",na.pad = TRUE),1),
         home_wickets_mean_last5=lag(rollmean(home_wickets_mean, 20,
                                                  align = "right",na.pad = TRUE),1),
         home_wickets_sum_last5=lag(rollmean(home_wickets_sum, 20,
                                                  align = "right",na.pad = TRUE),1),
         away_wickets_mean_last5=lag(rollmean(away_wickets_mean, 20,
                                                  align = "right",na.pad = TRUE),1),
         away_wickets_sum_last5=lag(rollmean(away_wickets_sum, 20,
                                                  align = "right",na.pad = TRUE),1))%>%
  ungroup()%>%
  arrange(home_team)%>%
  mutate(date_start=ymd(date_start))%>%
  mutate(year=floor_date(date_start,unit='year'))%>%
  select(-home_batter_runs_mean,-home_batter_runs_sum,-home_extra_runs_mean,
         -home_extra_runs_sum,-home_total_runs_mean,-home_total_runs_sum,
         -home_wickets_mean,-home_wickets_sum,-away_batter_runs_mean,
         -away_batter_runs_sum,-away_extra_runs_mean,-away_extra_runs_sum,
         -away_total_runs_mean,-away_total_runs_sum,-away_wickets_sum,
         -away_wickets_mean)

all_input_data<-backdata_sum_stats%>%
  rename(league=name)%>%
  filter(gender=='male')%>%
  select(-league,-venue)%>% # Massive ammount of dummies
  select(-any_of(c('sub_name','stage','group','balls_per_over','city','match_number',
         'match_type_number','match_referees','gender','date_start','date_end',
         'reserve_umpires','tv_umpires','runs','innings','wickets','winner',
         'result','method','eliminator','player_of_match','missing','season',
         'supersubs')))%>%
  mutate(home_wins=as_factor(home_wins))%>%
  mutate(year=factor(year))


###### temp
all_input_data <- all_input_data%>%
  select(-match_id) %>%
  mutate(home_team = as_factor(home_team),
         away_team = as_factor(away_team))


##### Split into test and train #####

splits<-initial_split(all_input_data,prop = 0.8)

df_train <- splits %>% training() %>%
  filter(!is.na(home_wins))

df_test <- splits %>% testing() %>%
  filter(!is.na(home_wins))

rm(all_input_data, all_matches, backdata_sum_stats, splits)

