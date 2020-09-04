## code to prepare `DATASET` dataset goes here
library(readr)
library(dplyr)
draft_data <- 
  read_csv("./data-raw/players.csv") %>% 
  select(team, firstName, lastName, position, playerBestOvr, playRecRating, playerSchemeOvr, physicalGrade,
         capHit:capReleasePenalty, everything()) %>% 
  mutate(youth = pmin(pmax(0, 26-age)),3)

draft_data$team[draft_data$isOnPracticeSquad] <-"PRACTICE_SQUAD" 

OL <- c("RG","LG","C", "LT", "RT")
DL <- c("LE", "RE", "DT")
LB <- c("LOLB", "ROLB", "MLB")
DB <- c("SS", "FS", "CB")
WR <- c("WR", "TE")
HB <- c("HB", "FB")
K <- c("K","P")

draft_data <- 
  draft_data %>% 
  mutate(group_position = position) %>% 
  mutate(group_position = ifelse(position %in% OL, "OL", group_position)) %>% 
  mutate(group_position = ifelse(position %in% DL, "DL", group_position)) %>% 
  mutate(group_position = ifelse(position %in% LB, "LB", group_position)) %>% 
  mutate(group_position = ifelse(position %in% DB, "DB", group_position)) %>% 
  mutate(group_position = ifelse(position %in% WR, "WR", group_position)) %>% 
  mutate(group_position = ifelse(position %in% HB, "HB", group_position)) %>% 
  mutate(group_position = ifelse(position %in% K, "K", group_position))

offense <- c("OL", "WR", "HB", "QB")  
defense <- c("DL", "LB", "DB") 
special_team <- c("K")

draft_data <- 
  draft_data %>% 
  mutate(simple_position = group_position) %>% 
  mutate(simple_position = ifelse(group_position %in% offense, "Offense", simple_position)) %>% 
  mutate(simple_position = ifelse(group_position %in% defense, "Defense", simple_position)) %>% 
  mutate(simple_position = ifelse(group_position %in% special_team, "Special Teams", simple_position)) 

usethis::use_data(draft_data, overwrite = TRUE)
