library(here)
library(dplyr)
library(tidyr)
library(lubridate)


base_path <- "../../Raw Data/"

list.files(base_path)


npq_path <- paste0(base_path,"NPQ_extraction_data/")


txt_files <- list.files(npq_path, pattern = "\\computedValue.txt$", full.names = TRUE)

df <- read.table(txt_files[1], 
                 skip = 4,     
                 header = TRUE,
                 sep = "\t",     
                 fill = TRUE,
                 nrows = 7164)

