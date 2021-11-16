library(tidyverse)

root_path <- "March-Madness-2021/"
sub_path <- paste(root_path, "Submission/sub_1.csv", sep = "")
teams_path <- paste(root_path, "Input/MTeams.csv", sep = "")
seeds_path <- paste(root_path, "Input/MNCAATourneySeeds.csv", sep = "")

submission <- read_csv(sub_path)
teams <- read_csv(teams_path)
seeds <- read_csv(seeds_path)

seeds_21 <- seeds %>%
  filter(Season == 2021)

All_Games <- submission %>% 
  mutate(TeamA = as.numeric(str_sub(ID,6,9)), 
         TeamB = as.numeric(str_sub(ID,11,14)))


# Replace this section with the creation of a data frame called games
# With the Teams in the first round it should have the following column names
#
# Game (a unique integer 1-32) 
# Team1 (team ID of 1st team in specified game) 
# Team2 (team ID of other team)

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

games <- as.data.frame(Game, Team1, Team2)


set.seed(5)

      
# Start loop
      
      
# puts team with lower Team ID as TeamA and higher Team ID as TeamB
games$TeamA <- pmin(games$Team1,games$Team2)
games$TeamB <- pmax(games$Team1,games$Team2)
games <- select(games,Game,TeamA,TeamB)


# Adds our predictions to all the games of the current round
round1 <- left_join(games,All_Games)



#Selects team that predictions predict to win
AWin <- round1 %>%
  filter(Pred>0.5) %>%
  select(Game,TeamA) %>%
  rename(WTeam=TeamA)

BWin <- round1 %>%
  filter(Pred<=0.5) %>%
  select(Game,TeamB) %>%
  rename(WTeam=TeamB)
  
winners <- bind_rows(BWin,AWin)

# Add a bind rows to store winners of current round


# reformats to prepare for next round
winners$Game2 <- winners$Game %% 2
winners$Game <- ceiling(winners$Game/2)

games <- winners %>%
  spread(Game2, WTeam) %>% 
  rename(Team1=0,Team2=1)

