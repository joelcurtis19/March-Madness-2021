rm(list=ls())

library(tidyverse)
library(googledrive)
library(data.table)

root_path <- "March-Madness-2021/"

input_path <- paste(root_path, "Input/", sep = "")
file_names <- list.files(path = input_path, pattern = "*.csv")

output_path <- paste(root_path, "Output/", sep = "")

files <- map(file_names, ~paste(input_path, .x, sep = ""))


file_list <- map(files, read_csv)

file_names <- map(file_names, ~gsub(".csv","" , .x))

names(file_list) <- file_names

#cities <- file_list$Cities
conferences <- file_list$Conferences
conferenceTourneyGames <- file_list$MConferenceTourneyGames
#gameCities <- file_list$MGameCities
#ncaaTourneyCompactResults <- file_list$MNCAATourneyCompactResults
ncaaTourneyDetailedResults <- file_list$MNCAATourneyDetailedResults
#ncaaTourneySeedRoundSlots <- file_list$MNCAATourneySeedRoundSlots
ncaaTourneySeeds <- file_list$MNCAATourneySeeds
#ncaaTourneySlots <- file_list$MNCAATourneySlots
#regularSeasonCompactResults <- file_list$MRegularSeasonCompactResults
regularSeasonDetailedResults <- file_list$MRegularSeasonDetailedResults
#sampleSubmssionStage2 <- file_list$MSampleSubmissionStage2
#seasons <- file_list$MSeasons
#secondaryTourneyCompactResults <- file_list$MSecondaryTourneyCompactResults
#secondaryTourneyTeams <- file_list$MSecondaryTourneyTeams
#teamCoaches <- file_list$MTeamCoaches
teamConferences <- file_list$MTeamConferences
teams <- file_list$MTeams
#teamSpellings <- file_list$MTeamSpellings

ncaaTourneySeeds <- ncaaTourneySeeds %>%
  mutate(Season_TeamID = paste(Season, TeamID, sep = "_"),
         Seed = parse_number(Seed))%>%
  select(-Season, -TeamID)

teamConferences <- teamConferences %>%
  left_join(conferences, by = "ConfAbbrev")%>%
  mutate(Season_TeamID = paste(Season, TeamID, sep = "_"))%>%
  select(-Season, -TeamID)

teams <- teams %>%
  select(TeamID, TeamName)

conferenceTourneyGames <- conferenceTourneyGames %>%
  mutate(Season_Conf_Day = paste(Season, ConfAbbrev, DayNum, sep = "_"),
         Competition = "Conf_Tourney")%>%
  select(Season_Conf_Day, Competition)

#join tourney and regular season results with seeds and teams


clean_detailed_results <- function(.data){
  
  .data <- .data %>%
    mutate(WSeason_TeamID = paste(Season, WTeamID, sep = "_"),
           LSeason_TeamID = paste(Season, LTeamID, sep = "_"))%>%
    left_join(ncaaTourneySeeds, by = c("WSeason_TeamID" = "Season_TeamID"))%>%
    rename(WSeed = Seed)%>%
    left_join(ncaaTourneySeeds, by = c("LSeason_TeamID" = "Season_TeamID"))%>%
    rename(LSeed = Seed)%>%
    left_join(teams, by = c("WTeamID" = "TeamID"))%>%
    rename(WTeamName = TeamName)%>%
    left_join(teams, by = c("LTeamID" = "TeamID"))%>%
    rename(LTeamName = TeamName)%>%
    left_join(teamConferences, by = c("WSeason_TeamID" = "Season_TeamID"))%>%
    rename(WConfAbbrev = ConfAbbrev, WConference = Description)%>%
    left_join(teamConferences, by = c("LSeason_TeamID" = "Season_TeamID"))%>%
    rename(LConfAbbrev = ConfAbbrev, LConference = Description)%>%
    mutate(Seed_Diff = WSeed-LSeed,
           Upset = ifelse(Seed_Diff>=3,TRUE,FALSE))
  
  .data

}


ncaaTourneyDetailedResults <- ncaaTourneyDetailedResults %>%
  clean_detailed_results()%>%
  mutate(Competition = "NCAA_Tourney")
  
regularSeasonDetailedResults <- regularSeasonDetailedResults%>%
  clean_detailed_results()%>%
  mutate(ConfAbbrev = ifelse(WConfAbbrev == LConfAbbrev, WConfAbbrev, NA),
         Season_Conf_Day = paste(Season, ConfAbbrev, DayNum, sep = "_"))%>%
  left_join(conferenceTourneyGames, by = "Season_Conf_Day")%>%
  mutate(Competition = ifelse(is.na(Competition),
                              "Regular_Season", Competition))%>%
  select(-c(Season_Conf_Day, ConfAbbrev))
  


detailedResults <- bind_rows(regularSeasonDetailedResults, ncaaTourneyDetailedResults)

detailed_path <- paste(output_path, "detailedResults.csv", sep = "")

write_csv(detailedResults, detailed_path)

#drive_put(detailed_path,
#          path = "March Madness/Men/2021/Output/detailedResults.csv",
#          type = "spreadsheet")
