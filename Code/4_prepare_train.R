rm(list=ls())

library(tidyverse)

root_path <- "March-Madness-2021/"

input_path <- paste(root_path, "Output/", sep = "")
output_path <- paste(root_path, "Output/", sep = "")

detailed_path <- paste(input_path, "detailedResults.csv", sep = "")
all_comp_path <- paste(input_path, "allCompStats.csv",
                               sep = "")

detailedResults <- read_csv(detailed_path)
allCompStats <- read_csv(all_comp_path)

allCompStats <- allCompStats %>%
  select(-c(Year, TeamID))

allCompStatsA <- allCompStats
allCompStatsB <- allCompStats

names(allCompStatsA) <- paste("TeamA", names(allCompStatsA), sep = "_")
names(allCompStatsB) <- paste("TeamB", names(allCompStatsB), sep = "_")



####################################################
###### randomly sample winning teams to be Team B ##
###### and losing teams to be Team A ###############
####################################################

seed <- 74

set.seed(seed)

obs <- nrow(detailedResults)
mid <- round(obs/2,0)

detailedResults <- detailedResults %>%
  mutate(RandomSort = rnorm(obs, mean = 0, sd = 1))%>%
  arrange(RandomSort)%>%
  select(-WLoc)

detailedResultsA <- detailedResults[1:(mid-1),]
detailedResultsB <- detailedResults[mid:obs,]

names(detailedResultsA) <- names(detailedResultsA) %>%
  gsub("W+", "TeamA_", .)%>%
  gsub("L+", "TeamB_", .)

names(detailedResultsB) <- names(detailedResultsB) %>%
  gsub("L+", "TeamA_", .)%>%
  gsub("W+", "TeamB_", .)

detailedResultsA <- detailedResultsA %>%
  mutate(TeamA_Win = 1)
detailedResultsB <- detailedResultsB %>%
  mutate(TeamA_Win = 0)

col_keep <- c("Season",
              "TeamA_TeamID",
              "TeamB_TeamID",
              "TeamA_Win")


results <- detailedResultsA %>%
  bind_rows(detailedResultsB)%>%
  select(all_of(col_keep))%>%
  mutate(TeamA_Season_TeamID = paste(Season, TeamA_TeamID, sep = "_"),
         TeamB_Season_TeamID = paste(Season, TeamB_TeamID, sep = "_"))%>%
  left_join(allCompStatsA, by =
              c("TeamA_Season_TeamID" = "TeamA_Season_TeamID"))%>%
  left_join(allCompStatsB, by =
              c("TeamB_Season_TeamID" = "TeamB_Season_TeamID"))


##############################################################
#### calculate difference between Team A and Team B stats ####
##############################################################

resultsA_drop <- c("TeamA_TeamID",
                   "TeamA_Win",
                   "TeamA_Season_TeamID")

resultsB_drop <- c("TeamB_TeamID",
                   "TeamB_Season_TeamID")
  
  
resultsA <- results %>%
  select(starts_with("TeamA"),
         -all_of(resultsA_drop),
         -ends_with("_median"),
         -ends_with("_sd"))

resultsB <- results %>%
  select(starts_with("TeamB"),
         -all_of(resultsB_drop),
         -ends_with("_median"),
         -ends_with("_sd"))

results_diff <- resultsA - resultsB

names(results_diff) <- names(results_diff) %>%
  gsub("TeamA_*", "",.)%>%
  gsub("*_mean", "", .)

model_train <- results%>%
  select(Season, TeamA_TeamID, TeamB_TeamID, TeamA_Win,
         TeamA_Season_TeamID, TeamB_Season_TeamID)

model_train <- model_train %>%
  bind_cols(results_diff)


#####export model_train

model_train_path <- paste(output_path, "model_train.csv", sep = "")

write_csv(model_train, model_train_path)
