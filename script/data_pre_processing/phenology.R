
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

file_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Datasheets_filled/greenhouse_senescence.xlsx"

sen_data <- read_excel(file_path, sheet = "senescence", skip = 1)


dates <- c("2025-09-16", "2025-09-25", "2025-10-01", "2025-10-08", "2025-10-15", 
           "2025-10-21", "2025-10-27", "2025-11-04", "2025-11-12", "2025-11-21", "2025-11-25")


tidy_senescence <- function(i){
  
  small_df <- cbind(sen_data[,1],sen_data[,2*i],sen_data[,2*i +1])
  
  colnames(small_df) <- c("ID","upper","lower")
  
  long_small_df <- small_df |>
    pivot_longer(
      cols = -ID
    ) |>
    rename(location = name,
           senescence  = value) |>
    mutate(date = ymd(dates[i]))
  
  return(long_small_df)
  
  }

final_df <- NULL

for(i in 1:11){
  
  tmp_df <- tidy_senescence(i)
  final_df <- rbind(final_df,tmp_df)
  print(i)
}

final_df <- rbind(final_df,tmp_df)


# !!! Crititcal error, after the first 100 in reached, the data are no more collected

final_df |> 
  drop_na() |>
  mutate(condition  = substr(ID,1,5)) |>
  mutate(condition = paste0(condition, "_", location)) |> 
  group_by(condition, date) |>
  summarise(senescence = mean(senescence, na.rm=T)) |>
  ggplot(aes(x = date, y = senescence, color = condition, group = condition)) + 
  geom_line()

print(nrow(final_df |> 
             drop_na()))


final_df_better <- final_df %>%
  arrange(ID, location, date) %>%
  group_by(ID, location) %>%
  mutate(
    
    senescence_temp = replace(senescence, is.na(senescence), 0),
    reached_100 = cumsum(senescence_temp >= 100) > 0,
    senescence = ifelse(reached_100 & is.na(senescence), 100, senescence)
    
  ) %>%
  select(-senescence_temp, -reached_100) %>%
  ungroup()

final_df_better |> 
  drop_na() |>
  mutate(condition  = substr(ID,1,5)) |>
  mutate(condition = paste0(condition, "_", location)) |> 
  group_by(condition, date) |>
  summarise(senescence = mean(senescence, na.rm=T)) |>
  ggplot(aes(x = date, y = senescence, color = condition, group = condition)) + 
  geom_line()

print(nrow(final_df_better |> 
             drop_na()))

write.table(final_df_better,, 
            file = "../../processed data/processed_data/phenology_processed.csv", 
            sep = ",", 
            row.names = FALSE)
