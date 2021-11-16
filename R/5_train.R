rm(list=ls())

library(tidyverse)

root_path <- "March-Madness-2021/"


output_path <- paste(root_path, "Output/", sep = "")

model_train_path <- paste(root_path, "Output/model_train.csv", sep = "")
predict_path <- paste(root_path, "Output/predict.csv", sep = "")

model_train <- read_csv(model_train_path)
predict_df <- read_csv(predict_path)

model_train <- model_train %>%
  mutate(ID = paste(Season, TeamA_TeamID, TeamB_TeamID, sep = "_"))
  
train <- model_train %>%
  filter(Season < 2015)%>%
  mutate(TeamA_Win = as.factor(TeamA_Win))


##### sub 1 #####
m1 <- glm(TeamA_Win ~ ScoreAgainst + FTP +
             FGP + AdjT + AdjO + AdjD +
             Luck + SoS_AdjEM,
           data = train,
           family = "binomial")

predict_df$Pred <- predict(m1, newdata = predict_df, type = "response")

submission <- predict_df%>%
  select(ID, Pred)%>%
  mutate(Pred = ifelse(ID == "2021_1180_1228", 0, Pred), #1 seed
         Pred = ifelse(ID == "2021_1124_1216", 1, Pred), #1 seed
         Pred = ifelse(ID == "2021_1276_1291", 1, Pred), #1 seed
         Pred = ifelse(ID == "2021_1276_1411", 1, Pred), #1 seed
         Pred = ifelse(ID == "2021_1111_1211", 0, Pred), #1 seed
         Pred = ifelse(ID == "2021_1211_1313", 1, Pred)) #1 seed

sub_path <- paste(root_path, "Submission/", "sub_1.csv", sep = "")

write_csv(submission, sub_path)

##### sub 2 #####
m2 <- glm(TeamA_Win ~ ScoreAgainst + FTP +
            FGP + AdjT + AdjO + AdjD +
            Luck + SoS_AdjEM,
          data = train,
          family = "binomial")

predict_df$Pred <- predict(m2, newdata = predict_df, type = "response")

submission <- predict_df%>%
  select(ID, Pred)%>%
  mutate(Pred = ifelse(ID == "2021_1180_1228", 0, Pred), #1 seed
         Pred = ifelse(ID == "2021_1124_1216", 1, Pred), #1 seed
         Pred = ifelse(ID == "2021_1276_1291", 1, Pred), #1 seed
         Pred = ifelse(ID == "2021_1276_1411", 1, Pred), #1 seed
         Pred = ifelse(ID == "2021_1111_1211", 0, Pred), #1 seed
         Pred = ifelse(ID == "2021_1211_1313", 1, Pred), #1 seed
         Pred = ifelse(ID == "2021_1326_1331", 0.95, Pred), #2 seed
         Pred = ifelse(ID == "2021_1156_1222", 0.05, Pred), #2 seed
         Pred = ifelse(ID == "2021_1213_1234", 0.1, Pred), #2 seed
         Pred = ifelse(ID == "2021_1104_1233", 0.95, Pred), #2 seed
         Pred = ifelse(ID == "2021_1156_1222", 0.05, Pred),
         Pred = ifelse(ID == "2021_1213_1234", 0.10, Pred),
         Pred = ifelse(ID == "2021_1325_1438", 0.25, Pred),
         Pred = ifelse(ID == "2021_1101_1400", 0.25, Pred),
         Pred = ifelse(ID == "2021_1317_1345", 0.25, Pred),
         Pred = ifelse(ID == "2021_1333_1397", 0.30, Pred),
         Pred = ifelse(ID == "2021_1251_1329", 0.25, Pred),
         Pred = ifelse(ID == "2021_1314_1458", 0.51, Pred),
         Pred = ifelse(ID == "2021_1163_1268", 0.49, Pred),
         Pred = ifelse(ID == "2021_1179_1425", 0.25, Pred),
         Pred = ifelse(ID == "2021_1281_1328", 0.65, Pred),
         Pred = ifelse(ID == "2021_1160_1207", 0.60, Pred),
         Pred = ifelse(ID == "2021_1166_1364", 0.75, Pred),
         Pred = ifelse(ID == "2021_1116_1159", 0.75, Pred),
         Pred = ifelse(ID == "2021_1437_1457", 0.65, Pred),
         Pred = ifelse(ID == "2021_1199_1422", 0.75, Pred)) 
         

sub_path <- paste(root_path, "Submission/", "sub_2.csv", sep = "")

write_csv(submission, sub_path)
