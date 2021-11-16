rm(list=ls())

library(tidyverse)
library(rvest)
library(googledrive)

root_path <- "March-Madness-2021/Data"
output_path <- paste(root_path, "Output/", sep = "")


years <- c(2002:2021)

kp_scrape <- function(x, year){
  
  
  url <- ifelse(year == 2021,"https://kenpom.com/index.php",
                paste("https://kenpom.com/index.php?y=", year, sep = ""))
  page <- read_html(url)
  df <- html_table(page, fill = TRUE)[[1]]
  
  df_names <- as.character(paste(names(df), df[1,], sep = ""))
  
  df_names <- df_names%>%
    gsub("Strength of Schedule", "SoS_", .)%>%
    gsub("NCSOS", "NCSOS_", .)
  
  df_names[4] <- "WL"
  
  names(df) <- df_names
  
  df <- df[,-c(7,9,11,13,15,17,19,21)]
  
  df[c(1,5:13)] <- df[,c(1,5:13)]%>%
    map_df(as.numeric)
    
  df <- df%>%
    drop_na()%>%
    separate(WL, into = c("W", "L"), sep = "-")%>%
    mutate(Year = year)
  
  df$Team <- str_trim(gsub('[0-9]+', '', df$Team))
  
  
  x <- bind_rows(x, df)
  
  return(x)

}

kp_df <- NULL

for(i in years){
  
  kp_df <- kp_scrape(kp_df, i)
  
}


kp_path <- paste(output_path, "ken_pom.csv", sep = "")


write_csv(kp_df, kp_path)

#drive_put(kp_path,
#          path = "March Madness/Men/2021/Output/ken_pom.csv",
#          type = "spreadsheet")

