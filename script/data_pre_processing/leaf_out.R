library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

file_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Datasheets_filled/greenhouse_phenology-springFULL_D&H.xlsx"

spring_data <- read_excel(file_path, sheet = "phenology", skip = 1)

dates <- read_excel(file_path, sheet = "phenology")

dates <- c("20.03.2025", "09.04.2025", "14.04.2025", "18.04.2025", "22.04.2025", "28.04.2025",
           "02.05.2025", "05.05.2025", "09.05.2025", "13.05.2025", "19.05.2025")


dates_iso <- format(as.Date(dates, format = "%d.%m.%Y"), "%Y-%m-%d")

tidy_senescence <- function(i){
  
  small_df <- cbind(spring_data[,1], spring_data[,2*i], spring_data[,2*i + 1])
  colnames(small_df) <- c("ID", "upper", "lower")
  
  small_df$upper <- as.numeric(as.character(small_df$upper))
  small_df$lower <- as.numeric(as.character(small_df$lower))
  
  long_small_df <- small_df |>
    pivot_longer(
      cols = -ID
    ) |>
    rename(location = name,
           senescence = value) |>
    mutate(date = ymd(dates_iso[i]))
  
  return(long_small_df)
}

final_df <- NULL

for(i in 1:11){
  tmp_df <-NULL
  tmp_df <- tidy_senescence(i)
  final_df <- rbind(final_df,tmp_df)
  print(i)
}

write.table(final_df,, 
            file = "../../processed data/processed_data/leaf_out_processed.csv", 
            sep = ",", 
            row.names = FALSE)

# tidy height and diameter

h_d_data <- read_excel(file_path, sheet = "diameter and height", range = "A2:M242")

dates <- c("20.03.2025", "13.05.2025", "31.07.2025", "05.09.2025", "30.09.2025", "07.11.2025")


dates_iso <- format(as.Date(dates, format = "%d.%m.%Y"), "%Y-%m-%d")

tidy_h_d <- function(i){
  
  small_df <- cbind(h_d_data[,1], h_d_data[,2*i], h_d_data[,2*i + 1])
  colnames(small_df) <- c("ID", "diameter", "height")
  
  small_df$diameter <- as.numeric(as.character(small_df$diameter))
  small_df$height <- as.numeric(as.character(small_df$height))
  small_df$date <-  ymd(dates_iso[i])
  
  return(small_df)
  
}

h_d_df <- NULL

for(i in 1:6){
  tmp_df <-NULL
  tmp_df <- tidy_h_d(i)
  h_d_df <- rbind(h_d_df,tmp_df)
  print(i)
}

write.table(h_d_df,
            file = "../../processed data/processed_data/height_and_diameter_processed.csv", 
            sep = ",", 
            row.names = FALSE)

