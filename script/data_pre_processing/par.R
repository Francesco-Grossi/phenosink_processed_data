library(here)
library(dplyr)
library(tidyr)
library(lubridate)

base_path <- "../../Raw Data/"

list.files(base_path)

## Leaf temperature is messy, I createa function that tidy everything based only on path

tidy_leaf_temperature <- function(path, condition){
  
  df_tmp <- read.csv(path)
  
  # split date in hour and day, divide in three dataset and then merge again
  # three possibilites: 21, 22, 23
  
  # 21 == date with 1 digit, hour with 1 digit
  
  df_tmp$ncar <- nchar(df_tmp$Date)
  
  
  # 21 == date with 1 digit, hour with 1 digit
  
  df_tmp$ncar <- nchar(df_tmp$Date)
  
  df_tmp_21 <- df_tmp |>
    filter(ncar == 21) |>
    mutate(month_str = substr(Date,1,3),
           day_str = substr(Date,5,5),
           year_str = substr(Date,7,10),
           hour_str = substr(Date,12,12),
           minutes_str = substr(Date,14,15),
           AM_PM = substr(Date,20,21)) |>
    mutate(time = as.integer(hour_str) + as.integer(minutes_str) / 60 +
             ifelse(AM_PM == "PM",12,0),
           month = ifelse(month_str == "Aug",8,
                          ifelse(month_str == "Sep",9,
                                 ifelse(month_str == "Oct",10,
                                        ifelse(month_str == "Nov",11,
                                               ifelse(month_str == "Dec",12,0)
                                        )
                                 )
                                 
                                 
                          )
           ),
           day = as.integer(day_str)) |>
    mutate(date = date(
      paste0(year_str,"-", 
             ifelse(month < 10,paste0(0,month), month),"-",
             ifelse(day < 10,paste0(0,day),day)
      )
    )
    ) |>
    select(date, time,PAR.µmol.m.s.)
  
  
  # 22 == date with 2 digits and hour with 1 digits or vicecersa
  # need to split in 2 for simplicity
  
  
  df_tmp_22 <- df_tmp %>%
    filter(nchar(Date) == 22) %>%
    mutate(
      month_str = substr(Date, 1, 3),
      rest = substr(Date, 5, nchar(Date)),
      day_str = ifelse(grepl("^[0-9] ", rest), substr(Date, 5, 5), substr(Date, 5, 6)),
      year_start = ifelse(nchar(day_str) == 1, 7, 8),
      year_str = substr(Date, year_start, year_start + 3),
      hour_start = ifelse(nchar(day_str) == 1, 12, 13),
      hour_str = substr(Date, hour_start, 13),
      minutes_str = substr(Date, 15,16),
      AM_PM = substr(Date, nchar(Date) - 1, nchar(Date))) |>
    mutate(time = as.integer(hour_str) + as.integer(minutes_str) / 60 +
             ifelse(AM_PM == "PM",12,0),
           month = ifelse(month_str == "Aug",8,
                          ifelse(month_str == "Sep",9,
                                 ifelse(month_str == "Oct",10,
                                        ifelse(month_str == "Nov",11,
                                               ifelse(month_str == "Dec",12,0)
                                        )
                                 )
                                 
                                 
                          )
           ),
           day = as.integer(day_str)) |>
    mutate(date = date(
      paste0(year_str,"-", 
             ifelse(month < 10,paste0(0,month), month),"-",
             ifelse(day < 10,paste0(0,day),day)
      )
    )
    ) |>
    select(date, time,PAR.µmol.m.s.)
  
  
  # 23 == date with 2 digits, hour with 2 digits
  
  
  df_tmp_23 <- df_tmp |>
    filter(ncar == 23) |>
    mutate(month_str = substr(Date,1,3),
           day_str = substr(Date,5,6),
           year_str = substr(Date,8,11),
           hour_str = substr(Date,13,14),
           minutes_str = substr(Date,16,17),
           AM_PM = substr(Date,22,23)) |>
    mutate(time = as.integer(hour_str) + as.integer(minutes_str) / 60 +
             ifelse(AM_PM == "PM",12,0),
           month = ifelse(month_str == "Aug",8,
                          ifelse(month_str == "Sep",9,
                                 ifelse(month_str == "Oct",10,
                                        ifelse(month_str == "Nov",11,
                                               ifelse(month_str == "Dec",12,0)
                                        )
                                 )
                                 
                                 
                          )
           ),
           day = as.integer(day_str)) |>
    mutate(date = date(
      paste0(year_str,"-", 
             ifelse(month < 10,paste0(0,month), month),"-",
             ifelse(day < 10,paste0(0,day),day)
      )
    )
    ) |>
    select(date, time,PAR.µmol.m.s.)
  
  # combine all
  df_final <- rbind(df_tmp_21, df_tmp_22, df_tmp_22)
  
  df_final$condition = condition
  
  return(df_final)
  
}


## leaf temperature

df_final <- NULL

par_path <- paste0(base_path,"PAR/")

list.files(par_path)

par_path_1 <- paste0(par_path,"Nov")


txt_files <- list.files(par_path_1, pattern = "\\.csv$", full.names = TRUE)

df_tmp <- read.csv(txt_files[1])

df <- tidy_leaf_temperature(txt_files[1], "control")

df_final <- rbind(df,df_final)

df <- tidy_leaf_temperature(txt_files[2], "T1")

df_final <- rbind(df,df_final)

df <- tidy_leaf_temperature(txt_files[3], "T2")

df_final <- rbind(df,df_final)

df <- tidy_leaf_temperature(txt_files[4], "T3")

df_final <- rbind(df,df_final)




par_path_2 <- paste0(par_path,"Dec")


txt_files <- list.files(par_path_2, pattern = "\\.csv$", full.names = TRUE)

df_tmp <- read.csv(txt_files[1])

df <- tidy_leaf_temperature(txt_files[1], "control")

df_final <- rbind(df,df_final)

df <- tidy_leaf_temperature(txt_files[2], "T1")

df_final <- rbind(df,df_final)

df <- tidy_leaf_temperature(txt_files[3], "T2")

df_final <- rbind(df,df_final)

df <- tidy_leaf_temperature(txt_files[4], "T3")

df_final <- rbind(df,df_final)

write.table(x = df_final, 
            file = "../../processed data/processed_data/par_processed.csv", 
            sep = ",", 
            row.names = FALSE)