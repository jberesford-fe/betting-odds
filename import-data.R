library(rjson)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(data.table)

rm(list=ls())

files <- list.files(path = 'raw-data/')%>%head(500)
match_ids <- str_remove_all(files,'.json')
file_paths<-paste("raw-data",files,sep = '/')

raw_list <- fromJSON(file = file_paths)

raw_info <- raw_list$info

df <- sapply(raw_innings,'[[','overs')
  bind_rows(.id='id')%>%
  unnest_longer(overs)%>%
  unnest_longer(deliveries)%>%
  unnest_wider(deliveries)%>%
  unnest_wider(extras)%>%
  rename(batter_name=batter)%>%
  unnest_wider(runs)


binder <- function(.){
  bind_rows(.id='id')
}


  

all_innings<-sapply(match_ids, matcher)%>%
  bind_rows(.id='match')

all_innings%>%
  group_by(team,match)%>%
  summarise(score=sum(total))%>%
  arrange(match)










a<-raw_list[[10]]

b%>%
  bind_rows(.id='id')%>%
  unnest_wider(overs)%>%
  unnest_longer(deliveries)%>%
  unnest_wider(deliveries)%>%
  unnest_wider(extras)%>%
  rename(batter_name=batter)%>%
  unnest_wider(runs)
