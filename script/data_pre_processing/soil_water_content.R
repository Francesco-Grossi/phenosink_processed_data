library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
# Load the raw data
file_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/soil/32 Rikka PK520-01_Table10Min.csv"

df <- read_csv(file_path, skip = 1)

df <- df[3:nrow(df),]

# skip the first two row for tidying

new_df <- df %>%
  mutate(
    date = ymd(substr(TIMESTAMP, 1, 10)),
    time = hms(substr(TIMESTAMP, 12, 19)),
    time = hour(time) + minute(time) / 60 + second(time) / 3600
  )  |>
  pivot_longer(
    cols = matches("^(SWC|Temp)\\(\\d+\\)$"),
    names_to = c("variable", "ID"),
    names_pattern = "(SWC|Temp)\\((\\d+)\\)",
    values_to = "value"
  ) %>%
  mutate(
    ID = as.integer(ID),
    value = na_if(value, "NAN"),
    value = as.numeric(value)
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  rename(
    SWC = SWC,
    temperature = Temp
  ) %>%
  select(date, time, SWC, temperature, ID)

write.table(x = new_df, 
            file = "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/soil_wsc_and_temperature_processed.csv", 
            sep = ",", 
            row.names = FALSE)

