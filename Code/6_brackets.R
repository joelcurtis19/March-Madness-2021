rm(list=ls())

library(tidyverse)
library(googledrive)
library(collegehoops)

season <- 2021

root_path <- "C:/Users/Joel/Documents/Kaggle/March Madness/Men/2021/Stage_2/"

output_path <- paste(root_path, "Output/", sep = "")
sub_path <- paste(root_path, "Submission/", "sub_1.csv", sep = "")
teams_path <- paste(root_path,"Input/", "MTeams.csv", sep = "")
seeds_path <- paste(root_path, "Input/", "MNCAATourneySeeds.csv", sep = "")

sub_df <- read_csv(sub_path)
teams_df <- read_csv(teams_path)
seeds <- read_csv(seeds_path)

seeds_21 <- seeds %>%
  filter(Season == season)%>%
  mutate(TeamID = as.character(TeamID))%>%
  select(-Season)



options(repr.plot.width = 14, repr.plot.height = 8)
# parse bracket - moving forward predicted winning teams
bracket <- parse_bracket(sub_path, year = '2021')
# print the bracket
print_bracket(bracket)


Game <- c(1:36)

Team1 <- c("1211", "1211", "1328", "1166",
           "1438", "1425", "1425", "1242",
           "1332", "1234", "1276", "1276",
           "1261", "1160", "1199", "1140",
           "1140", "1400", "1163", "1104",
           "1124", "1314", "1437", "1345",
           "1403", "1116", "1196", "1326",
           "1228", "1260", "1397", "1329",
           "1361", "1452", "1155", "1222")

Team2 <- c("1313", "1111", "1281", "1364",
           "1325", "1455", "1179", "1186",
           "1433", "1213", "1411", "1291",
           "1382", "1207", "1422", "1277",
           "1417", "1101", "1268", "1233",
           "1216", "1458", "1457", "1317",
           "1429", "1159", "1439", "1331",
           "1180", "1210", "1333", "1251",
           "1393", "1287", "1353", "1156")

Season <- season
games <- data.frame(Game, Season, Team1, Team2)

# puts team with lower Team ID as TeamA and higher Team ID as TeamB
games$TeamA <- pmin(games$Team1,games$Team2)
games$TeamB <- pmax(games$Team1,games$Team2)
games <- games %>%
  mutate(ID = paste(Season, TeamA, TeamB, sep = "_"),
         Round = "1")%>%
  select(-c(Season, Game, TeamA, TeamB, Team1, Team2))

  
teams_df <- teams_df %>%
  mutate(TeamID = as.character(TeamID))%>%
  select(TeamID, TeamName)

prediction_df <- sub_df %>%
  mutate(Season = str_sub(ID, 1, 4),
         TeamA_ID = str_sub(ID, 6, 9),
         TeamB_ID = str_sub(ID, 11, 14),
         TeamA_Win = ifelse(Pred > 0.5, TRUE, FALSE))%>%
  left_join(teams_df, by = c("TeamA_ID" = "TeamID"))%>%
  mutate(TeamA_Name = TeamName)%>%
  select(-TeamName)%>%
  left_join(teams_df, by = c("TeamB_ID" = "TeamID"))%>%
  mutate(TeamB_Name = TeamName)%>%
  select(-TeamName)%>%
  left_join(seeds_21, by = c("TeamA_ID" = "TeamID"))%>%
  mutate(TeamA_Seed = Seed)%>%
  select(-Seed)%>%
  left_join(seeds_21, by = c("TeamB_ID" = "TeamID"))%>%
  mutate(TeamB_Seed = Seed)%>%
  select(-c(Seed))%>%
  left_join(games, by = c("ID" = "ID"))

prediction_path <- paste(output_path, "predictions3.csv", sep = "")

write_csv(prediction_df, prediction_path)

drive_put(prediction_path,
          path = "March Madness/Men/2021/Output/predictions3.csv",
          type = "spreadsheet")
