library(jsonlite)
library(tidyverse)
library(lubridate)

# Load the raw data
file_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/NPQ_extraction_data/Fluoropen1_20250820_json.txt"

json_raw <- readChar(file_path, file.info(file_path)$size)

# 2. Clean non-standard JSON values
# Replace nan.0 with null so jsonlite can parse it [cite: 10571, 10581]
json_clean <- gsub("nan\\.0", "null", json_raw)

# 3. Parse JSON into a tibble
raw_df <- fromJSON(json_clean) %>% as_tibble()

tidy_npq <- raw_df %>%
  filter(type == "npq1") %>%
  # Fix: Use parse_date_time to handle the double space between time and date
  mutate(time = parse_date_time(time, orders = "HMS dmy")) %>% 
  select(time, fo, fm, fp, matches("_(l|d)[1-4ss]+")) %>%
  pivot_longer(
    cols = -c(time, fo, fm, fp),
    names_to = "parameter_phase",
    values_to = "value"
  ) %>%
  separate(parameter_phase, into = c("metric", "phase"), sep = "_")

head(tidy_npq)

write.table(x = tidy_npq, 
            file = "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/npq_processed.csv", 
            sep = ",", 
            row.names = FALSE)
