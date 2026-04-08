library(here)
library(dplyr)
library(tidyr)
library(lubridate)


base_path <- "../../Raw Data/"

list.files(base_path)

## leaf temperature

air_path <- paste0(base_path,"leaf temperature/")

# first leaf temperature

air_path_1 <- paste0(air_path,"Leaf temperature_202506_to_202508/Leaf temperature/")

list.files(air_path_1)

txt_files <- list.files(air_path_1, pattern = "\\.txt$", full.names = TRUE)
all_leaves <- data.frame()


for(file_path in txt_files) {

  leaf_num <- sub(".*ps_leaf(\\d+)\\.txt.*", "\\1", basename(file_path))
  
  # if there are 95 charachter, 1 digits, otherwise 2
  identidier <- ifelse(nchar(file_path) == 95,as.integer(substr(file_path,91,91)),as.integer(substr(file_path,91,92)))
  
  df <- read.table(file_path, 
                   header = TRUE, 
                   sep = ",", 
                   fill = TRUE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "latin1",
                   check.names = FALSE)
  
  leaf_data <- df %>%
    mutate(
      date = as.Date(Time),
      time = hour(Time) + minute(Time)/60,
      temp = as.numeric(`Celsius(°C)`) + 273.15,
      leaf = identidier  
    )%>%
    select(leaf, date, time, temp)
  
  all_leaves <- bind_rows(all_leaves, leaf_data)
}

## second leaves temperature

air_path_2 <- paste0(air_path,"Leaf_temperatures_from_202508/")

list.files(air_path_2)

txt_files <- list.files(air_path_2, pattern = "\\.txt$", full.names = TRUE)
all_leaves_2 <- data.frame()


for(file_path in txt_files) {
  
  leaf_num <- sub(".*ps_leaf(\\d+)\\.txt.*", "\\1", basename(file_path))
  
  # smae as above
  identidier <- ifelse(nchar(file_path) == 74,as.integer(substr(file_path,70,70)),as.integer(substr(file_path,70,71)))
  
  df <- read.table(file_path, 
                   header = TRUE, 
                   sep = ",", 
                   fill = TRUE,
                   stringsAsFactors = FALSE,
                   fileEncoding = "latin1",
                   check.names = FALSE)
  
  leaf_data <- df %>%
    mutate(
      date = as.Date(Time),
      time = hour(Time) + minute(Time)/60,
      temp = as.numeric(`Celsius(°C)`) + 273.15,
      leaf = identidier  
    )%>%
    select(leaf, date, time, temp)
  
  all_leaves_2 <- bind_rows(all_leaves_2, leaf_data)
}

laves_final <-  bind_rows(all_leaves, all_leaves_2)

write.table(x = laves_final, 
            file = "../../processed data/processed_data/leaf_temperature_processed.csv", 
            sep = ",", 
            row.names = FALSE)

