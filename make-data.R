library(tidymodels)
library(tidyverse)
library(data.table)
library(rjson)
library(zoo)

rm(list=ls())
all_files<-paste('raw-data/',list.files(path = 'raw-data/'),sep="")
usefull_files <- all_files[str_detect(all_files,"^\\d|.json$")] %>% head(1000)


raw_match_data <- lapply(usefull_files,function(x) fromJSON(file = x))
match_id<-str_remove_all(usefull_files,"raw-data/|.json")
names(raw_match_data)<-match_id


# Part 1: Get match info
raw_info <- sapply(raw_match_data,'[','info')
names(raw_info)<-str_remove_all(names(raw_info),".info")

match_level_info<-data.table(raw_info)%>%
  unnest_wider(raw_info)%>%
  unnest_wider(event)%>%
  unnest_wider(officials)%>%
  select(-umpires,-players,-toss,-registry)%>%
  unnest_wider(outcome)%>%
  unnest_wider(by)%>%
  mutate(date_start=sapply(dates,min),
         date_end=sapply(dates,max))%>%
  select(-any_of(c('dates','...1')))%>%
  unnest_wider(teams)%>% # This part is shit and slow, come back to it
  rename('home_team'=`...1`)%>%
  rename('away_team'=`...2`)%>%
  mutate(home_wins=winner==home_team)%>%
  mutate(match_id=match_id)


player_teams<-data.table(raw_info)%>%
  unnest_wider(raw_info)%>%
  select(players)%>%
  unnest_longer(players)%>%
  unnest_longer(players)




# Part 2: Get overs info for all matches
# Get innings info for all matches
raw_innings <- sapply(raw_match_data,'[','innings')                                      
names(raw_innings)<-str_remove_all(names(raw_innings),".innings")
# Get overs info for all matches (can I put an sapply in an lapply instead?)
get_overs <- function(input_list){
  all_overs <- sapply(input_list,'[','overs')
  return(all_overs)
}
all_overs_by_match<-lapply(raw_innings, get_overs)


# Who batted in each innings?
get_team_by_innings <- function(input_list){
  teams_by_innings <- sapply(input_list,'[','team')
  return(teams_by_innings)
}
team_by_innings<-lapply(raw_innings, get_team_by_innings)
# Name the innings list by batting team

for (i in 1:length(all_overs_by_match)){
  names(all_overs_by_match[[i]])<-team_by_innings[[i]]
}

all_overs_by_match_bound<-lapply(all_overs_by_match, function(y) lapply(y, function(x) bind_rows(x)))

for (i in 1:length(all_overs_by_match_bound)){
  for (n in 1:length(all_overs_by_match_bound[[i]])){
    all_overs_by_match_bound[[i]][[n]]<-all_overs_by_match_bound[[i]][[n]]%>%
      mutate(team=names(all_overs_by_match_bound[[i]][n]))
  }
}

unnest_start <- function(list_of_lists){
  list_of_dfs <- list_of_lists %>%
    bind_rows()%>%
    unnest_wider(deliveries)
  return(list_of_dfs)
}

all_overs_partially_unnested<-lapply(all_overs_by_match_bound,unnest_start)  

col_names<-map_depth(all_overs_partially_unnested,1,names)

drop_missing_extras<-lapply(col_names,function(x) sum(str_count(x,"extras")))%>%
  bind_rows(.id='id')%>%
  gather(key=id,value=contains_extra)%>%
  filter(contains_extra==0)%>%
  pull(id)

drop_missing_wickets<-lapply(col_names,function(x) sum(str_count(x,"wickets")))%>%
  bind_rows(.id='id')%>%
  gather(key=id,value=contains_extra)%>%
  filter(contains_extra==0)%>%
  pull(id)

elements_to_drop<-union(drop_missing_extras,drop_missing_wickets)

for (m in elements_to_drop){
  all_overs_partially_unnested[[m]] <- NULL
}

unnest_remaining <- function(list_of_lists){
  list_of_dfs <- list_of_lists %>%
    unnest_wider(extras)%>%
    rename(batter_name=batter)%>%
    unnest_wider(runs)%>%
    unnest_longer(wickets)%>%
    unnest_wider(wickets)%>%
    mutate(wickets=if_else(is.na(kind),0,1))
  
  return(list_of_dfs)
}

all_overs<-lapply(all_overs_partially_unnested,unnest_remaining)


make_summary_stats <- function(tibble_by_match){
  summary_stats <- tibble_by_match %>%
    group_by(over, team)%>%
    rename(bowler_name=bowler,batter_runs=batter,extra_runs=extras,total_runs=total)%>%
    summarise(across(any_of(c('batter_runs','extra_runs','total_runs','wickets')),
              sum,na.rm=TRUE),
              .groups='drop')%>%
    group_by(team)%>%
    summarise(across(any_of(c("batter_runs","extra_runs","total_runs","wickets")),
                     list(mean = mean, sum = sum),
                     na.rm = TRUE, .names = "{col}_{fn}"))
  return(summary_stats)
}

match_level_summary_stats<-lapply(all_overs, make_summary_stats)%>%
  bind_rows(.id='match_id')


home_team_summary_stats<-match_level_info%>%
  select(home_team,match_id)%>%
  full_join(match_level_summary_stats,by="match_id")%>%
  filter(home_team==team)
names(home_team_summary_stats)<-paste("home",names(home_team_summary_stats),sep="_")
home_team_summary_stats<-home_team_summary_stats%>%
  select(-home_home_team,-home_team)%>%
  rename(match_id=home_match_id)


away_team_summary_stats<-match_level_info%>%
  select(home_team,match_id)%>%
  full_join(match_level_summary_stats,by="match_id")%>%
  filter(home_team!=team)
names(away_team_summary_stats)<-paste("away",names(away_team_summary_stats),sep="_")
away_team_summary_stats<-away_team_summary_stats%>%
  select(-away_home_team,-away_team)%>%
  rename(match_id=away_match_id)

all_matches<-match_level_info%>%
  inner_join(home_team_summary_stats,by="match_id")%>%
  inner_join(away_team_summary_stats,by="match_id")

rm(list=setdiff(ls(), "all_matches"))
   