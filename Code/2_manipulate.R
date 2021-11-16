rm(list=ls())

library(tidyverse)

root_path <- "March-Madness-2021/Data"
input_path <- paste(root_path, "Output/", sep = "")

detailed_path <- paste(input_path, "detailedResults.csv", sep = "")
kp_path <- paste(input_path, "ken_pom_wid.csv", sep = "")

detailedResults <- read_csv(detailed_path)
kenPom <- read_csv(kp_path)

output_path <- paste(root_path, "Output/", sep = "")


summarize_stats <- function(comp, stats_w = wTeamStats, stats_l = lTeamStats){
  
  df <- stats_w%>%
    bind_rows(stats_l)%>%
    filter(Competition %in% comp)%>%
    select(-c(Competition, Season, TeamID, TeamName, Seed,
              ConfAbbrev, Conference))%>%
    group_by(Season_TeamID)%>%
    summarize_all(list(mean,median,sd))
  
  names(df) <- names(df)%>%
    gsub("*_fn1", "_mean",.)%>%
    gsub("*_fn2", "_median",.)%>%
    gsub("*_fn3", "_sd",.)
  
  df <- df %>%
    mutate(FGP_mean = FGM_mean/FGA_mean,
           FGP3_mean = FGM3_mean/FGA3_mean,
           FTP_mean = FTM_mean/FTA_mean,
           FGP_median = FGM_median/FGA_median,
           FGP3_median = FGM3_median/FGA3_median,
           FTP_median = FTM_median/FTA_median,
           FGP_sd = FGM_sd/FGA_sd,
           FGP3_sd = FGM3_sd/FGA3_sd,
           FTP_sd = FTM_sd/FTA_sd)
  
  return(df)  
}

kenPom <- kenPom %>%
  select(yearTeamID, Year, TeamID, AdjEM, AdjO, AdjD, AdjT, Luck,
         SoS_AdjEM, SoS_OppO, SoS_OppD, NCSOS_AdjEM)

wTeamStats <- detailedResults %>%
  rename(WScoreFor = WScore, WScoreAgainst = LScore)%>%
  select(Season, Competition, starts_with("W"), -WLoc)
  
w_detailed_col_names <- as.list(names(wTeamStats[1:2]))

w_detailed_col_names <- as.character(append(w_detailed_col_names,
                                   substring(names(
                                     wTeamStats[3:ncol(
                                       wTeamStats)]),2)))

names(wTeamStats) <- w_detailed_col_names

lTeamStats <- detailedResults %>%
  rename(LScoreFor = LScore, LScoreAgainst = WScore)%>%
  select(Season, Competition, starts_with("L"))

l_detailed_col_names <- as.list(names(lTeamStats[1:2]))

l_detailed_col_names <- as.character(append(l_detailed_col_names,
                                            substring(names(
                                              lTeamStats[3:ncol(
                                                lTeamStats)]),2)))

names(lTeamStats) <- l_detailed_col_names

#regularSeasonStats <- summarize_stats(comp = "Regular_Season")
#ncaaTourneyStats <- summarize_stats(comp = "NCAA_Tourney")
#confTourneyStats <- summarize_stats(comp = "Conf_Tourney")
regularConfTourneyStats <- summarize_stats(comp = c("Regular_Season",
                                                    "Conf_Tourney"))
allCompStats <- summarize_stats(comp = c("Regular_Season",
                                         "Conf_Tourney",
                                         "NCAA_Tourney"))

#winning team averages
gamePredictors <- detailedResults %>%
  select(Season, WSeason_TeamID, LSeason_TeamID, Competition,
         WTeamID, LTeamID, WScore, LScore, NumOT, WConference,
         LConference, Seed_Diff, Upset)%>%
  left_join(regularConfTourneyStats, by = c("WSeason_TeamID" = "Season_TeamID"))


#winning team colnames
gp_names <- names(gamePredictors[1:13])

gp_names <- append(gp_names,
      paste("W",names(gamePredictors[14:ncol(gamePredictors)]), sep =""))

names(gamePredictors) <- gp_names


#losing team averages
gamePredictors <- gamePredictors %>%
  left_join(regularConfTourneyStats, by = c("LSeason_TeamID" = "Season_TeamID"))

#losing team colnames
gp_names <- names(gamePredictors[1:67])

gp_names <- append(gp_names,
                   paste("L",names(gamePredictors[68:ncol(gamePredictors)]), sep =""))

names(gamePredictors) <- gp_names


#begin calculate difference of winner vs loser
#between mean, median, and sd of metrics
wTeamSumStats <- gamePredictors %>%
  select(starts_with("W"))%>%
  select(-c(WSeason_TeamID, WTeamID, WScore, WConference))

lTeamSumStats <- gamePredictors %>%
  select(starts_with("L"))%>%
  select(-c(LSeason_TeamID, LTeamID, LScore, LConference))

statDiff <- wTeamSumStats - lTeamSumStats

names(statDiff) <- names(statDiff) %>%
  gsub("W*", "",.)

names(statDiff) <- paste(names(statDiff), "diff", sep = "_")
  
gamePredictors <- gamePredictors %>%
  bind_cols(statDiff)

#end calculate difference of winner vs loser
#between mean, median, and sd of metrics
wGamePredictors <- gamePredictors %>%
  select(Season, Competition, NumOT, Seed_Diff, Upset,
         starts_with("W"))

names(wGamePredictors) <- names(wGamePredictors) %>%
  gsub("W*", "",.)

wGamePredictors <- wGamePredictors %>%
  mutate(Game_Win = TRUE)

lGamePredictors <- gamePredictors %>%
  select(Season, Competition, NumOT, Seed_Diff, Upset,
         starts_with("L"))%>%
  mutate(Game_Win = FALSE)

names(lGamePredictors) <- names(lGamePredictors) %>%
  gsub("L*", "",.)

binaryGamePredictors <- wGamePredictors %>%
  bind_rows(lGamePredictors)


allCompStats <- allCompStats %>%
  left_join(kenPom, by = c("Season_TeamID" = "yearTeamID"))
regularConfTourneyStats <- regularConfTourneyStats %>%
  left_join(kenPom, by = c("Season_TeamID" = "yearTeamID"))


all_path <- paste(output_path, "allCompStats.csv", sep = "")
regular_path <- paste(output_path, "regularConfTourneyStats.csv", sep = "")
predictor_path <- paste(output_path, "gamePredictors.csv", sep = "")
binary_predictor_path <- paste(output_path, "binaryGamePredictors.csv", sep = "")


write_csv(regularConfTourneyStats, regular_path)
write_csv(allCompStats, all_path)
write_csv(gamePredictors, predictor_path)
write_csv(binaryGamePredictors, binary_predictor_path)
