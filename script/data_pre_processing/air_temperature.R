library(here)
library(dplyr)
library(tidyr)
library(lubridate)


base_path <- "../../Raw Data/"

list.files(base_path)

## leaf temperature

air_path <- paste0(base_path,"air/")

list.files(air_path)

# !! RUN IT ONCE (already done it)
#unzip("Air temperature.zip", exdir = air_path)

# done the first

air_path_1 <- paste0(air_path,"Air temperature")

list.files(air_path_1)

txt_files <- list.files(air_path_1, pattern = "\\.txt$", full.names = TRUE)
all_air_1  <- data.frame()



for(file_path in txt_files) {
  

  # if there are 95 charachter, 1 digits, otherwise 2
  identidier <- as.integer((substr(file_path,49,49)))
  
  df <- read.table(file_path, 
                   header = TRUE, 
                   sep = ",", 
                   fill = TRUE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "latin1",
                   check.names = FALSE)
  
  df_2 <- df %>%
    mutate(
      date = as.Date(Time),
      time = hour(Time) + minute(Time)/60,
      temp = as.numeric(`Celsius(°C)`) + 273.15,
      sensor = identidier,
      humidity = `Humidity(%rh)` / 100
    )%>%
    rename(dew_point = `Dew Point(°C)`) |>
    select(sensor, date, time, temp, humidity, dew_point)
  
  all_air_1 <- bind_rows(all_air_1, df_2)
}


# -------------
# August

air_path_2 <- paste0(air_path,"Aug")

list.files(air_path_2)

txt_files <- list.files(air_path_2, pattern = "\\.txt$", full.names = TRUE)
all_air_2 <- data.frame()



for(file_path in txt_files) {
  
  
  # if there are 95 charachter, 1 digits, otherwise 2
  identidier <- as.integer((substr(file_path,37,37)))
  
  df <- read.table(file_path, 
                   header = TRUE, 
                   sep = ",", 
                   fill = TRUE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "latin1",
                   check.names = FALSE)
  
  df_2 <- df %>%
    mutate(
      date = as.Date(Time),
      time = hour(Time) + minute(Time)/60,
      temp = as.numeric(`Celsius(°C)`) + 273.15,
      sensor = identidier,
      humidity = `Humidity(%rh)` / 100
    )%>%
    rename(dew_point = `Dew Point(°C)`) |>
    select(sensor, date, time, temp, humidity, dew_point)
  
  all_air_2 <- bind_rows(all_air_2, df_2)
}


# -------------
# november

air_path_3 <- paste0(air_path,"Nov")

list.files(air_path_3)

txt_files <- list.files(air_path_3, pattern = "\\.txt$", full.names = TRUE)
all_air_3 <- data.frame()



for(file_path in txt_files) {
  
  
  # if there are 95 charachter, 1 digits, otherwise 2
  identidier <- as.integer((substr(file_path,37,37)))
  
  df <- read.table(file_path, 
                   header = TRUE, 
                   sep = ",", 
                   fill = TRUE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "latin1",
                   check.names = FALSE)
  
  df_2 <- df %>%
    mutate(
      date = as.Date(Time),
      time = hour(Time) + minute(Time)/60,
      temp = as.numeric(`Celsius(°C)`) + 273.15,
      sensor = identidier,
      humidity = `Humidity(%rh)` / 100
    )%>%
    rename(dew_point = `Dew Point(°C)`) |>
    select(sensor, date, time, temp, humidity, dew_point)
  
  all_air_3 <- bind_rows(all_air_3, df_2)
}

#------
# December


air_path_4 <- paste0(air_path,"Dec")

list.files(air_path_4)

txt_files <- list.files(air_path_4, pattern = "\\.txt$", full.names = TRUE)
all_air_4 <- data.frame()



for(file_path in txt_files) {
  
  
  # if there are 95 charachter, 1 digits, otherwise 2
  identidier <- as.integer((substr(file_path,37,37)))
  
  df <- read.table(file_path, 
                   header = TRUE, 
                   sep = ",", 
                   fill = TRUE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "latin1",
                   check.names = FALSE)
  
  df_2 <- df %>%
    mutate(
      date = as.Date(Time),
      time = hour(Time) + minute(Time)/60,
      temp = as.numeric(`Celsius(°C)`) + 273.15,
      sensor = identidier,
      humidity = `Humidity(%rh)` / 100
    )%>%
    rename(dew_point = `Dew Point(°C)`) |>
    select(sensor, date, time, temp, humidity, dew_point)
  
  all_air_4 <- bind_rows(all_air_4, df_2)
}

final_air <- bind_rows(all_air_1,all_air_2,all_air_3,all_air_4)

write.table(x = final_air, 
            file = "../../processed data/processed_data/air_temperature_processed.csv", 
            sep = ",", 
            row.names = FALSE)









