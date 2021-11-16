rm(list=ls())

library(tidyverse)

root_path <- "March-Madness-2021/"


output_path <- paste(root_path, "Output/", sep = "")

regular_stats_path <- paste(root_path, "Output/regularConfTourneyStats.csv", sep = "")
samp_sub_path <- paste(root_path, "Input/MSampleSubmissionStage2.csv", sep = "")

regularConfTourneyStats <- read_csv(regular_stats_path)
samp_sub <- read_csv(samp_sub_path)


stats_drop <- c("Year",
                "TeamID")

teamAStats <- regularConfTourneyStats%>%
  select(-all_of(stats_drop))
  
teamBStats <- regularConfTourneyStats%>%
  select(-all_of(stats_drop))

names(teamAStats) <- paste("TeamA", names(teamAStats), sep = "_")
names(teamBStats) <- paste("TeamB", names(teamBStats), sep = "_")


predict_df <- samp_sub %>%
  mutate(Year = str_sub(ID, start = 1L, end = -11L),
         TeamA_ID = str_sub(ID, start = 6L, end = -6L),
         TeamB_ID = str_sub(ID, start = 11L, end = -1L),
         TeamA_Season_TeamID = paste(Year, TeamA_ID, sep = "_"),
         TeamB_Season_TeamID = paste(Year, TeamB_ID, sep = "_"))%>%
  left_join(teamAStats, by = c("TeamA_Season_TeamID" = "TeamA_Season_TeamID"))%>%
  left_join(teamBStats, by = c("TeamB_Season_TeamID" = "TeamB_Season_TeamID"))%>%
  select(-ends_with("_median"), -ends_with("_sd"))%>%
  distinct_at("ID", .keep_all = TRUE)

##############################################################
#### calculate difference between Team A and Team B stats ####
##############################################################

predict_drop <- c("ID",
                  "Pred",
                  "Year",
                  "TeamA_ID",
                  "TeamA_Season_TeamID",
                  "TeamB_ID",
                  "TeamB_Season_TeamID")

predictA <- predict_df %>%
  select(starts_with("TeamA"),
         -all_of(predict_drop),
         -ends_with("_median"),
         -ends_with("_sd"))

predictB <- predict_df %>%
  select(starts_with("TeamB"),
         -all_of(predict_drop),
         -ends_with("_median"),
         -ends_with("_sd"))

predict_diff <- predictA - predictB

names(predict_diff) <- names(predict_diff) %>%
  gsub("TeamA_*", "",.)%>%
  gsub("*_mean", "", .)

predict <- predict_df%>%
  select(ID, Year, TeamA_ID, TeamB_ID, Pred,
         TeamA_Season_TeamID, TeamB_Season_TeamID)

predict <- predict %>%
  bind_cols(predict_diff)

#####export predict.csv

predict_path <- paste(output_path, "predict.csv", sep = "")

write_csv(predict, predict_path)
