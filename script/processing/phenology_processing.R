library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(broom)
library(purrr)
library(stringr)


df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/phenology_processed.csv")


df |> 
  drop_na() |>
  mutate(condition = paste0(substr(ID, 1, 8), "_", location)) |>
  group_by(condition, date) |>
  summarise(senescence = mean(senescence, na.rm=T)) |>
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |> 
  mutate(condition = str_remove_all(condition, "\\.")) |>
  ggplot( aes(x = diff_date, color = condition, group = condition)) +
  geom_point(aes(y = senescence), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Senescence progression",
    x = "Days since start",
    y = "Senescence",
    color = "Condition"
  ) +
  facet_wrap(~condition) + theme(legend.position = "none")

# !! Here change the grouping
fit_sigmoid <- df |> 
  drop_na() |>
  mutate(ID = str_remove_all(ID, "\\.")) |>
  mutate(ID = substr(ID,1,8)) |>
  mutate(ID = ifelse(substr(ID, 7, 7) == "C", 
                     substr(ID, 1, 7), 
                     ID)) |>
  mutate(condition = paste0(ID, "_", location)) |> 
  group_by(condition, date) |>
  summarise(senescence = mean(senescence, na.rm = TRUE) / 100) |> 
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |> 
  ungroup()

head(fit_sigmoid)


  



###---------




# Define the new formula with a base (starting value)
# (1 - base) ensures the top asymptote remains at 1.0
sigmoid_base_formula <- senescence ~ base + (1 - base) / (1 + exp(-i * (diff_date - t50)))

fit_data_fixed <- fit_sigmoid |>
  group_nest(condition) |>
  mutate(model = map(data, ~ {
    tryCatch(
      nls(sigmoid_base_formula, 
          data = .x, 
          # base = 0.2 (initial senescence)
          # i = 0.1 (rate)
          # t50 = 30 (inflection point)
          start = list(base = 0.2, i = 0.1, t50 = 30)),
      error = function(e) return(NULL)
    )
  })) |>
  filter(!map_lgl(model, is.null)) |>
  mutate(augmented = map2(model, data, ~augment(.x, data = .y))) |>
  unnest(augmented)

ggplot(fit_data_fixed, aes(x = diff_date, color = condition)) +
  geom_point(aes(y = senescence), alpha = 0.6) +
  geom_line(aes(y = .fitted), linewidth = 1) +
  facet_wrap(~condition) +
  theme_minimal() +
  labs(title = "Sigmoid Fit with Baseline Offset") +
  theme(legend.position = "none")


fitted_parameters_df <- fit_data_fixed |>
  group_nest(condition) |>
  mutate(model = map(data, ~ {
    tryCatch(
      nls(senescence ~ base + (1 - base) / (1 + exp(-i * (diff_date - t50))), 
          data = .x, 
          start = list(base = 0.2, i = 0.1, t50 = 30)),
      error = function(e) return(NULL)
    )
  })) |>
  mutate(params = map(model, ~ if(!is.null(.x)) tidy(.x) else NULL)) |>
  unnest(params) |>
  select(condition, term, estimate, std.error, p.value)

final_params_wide <- fitted_parameters_df |>
  select(condition, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate)




###-----------

# leaf out


df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/leaf_out_processed.csv")

# split stage and percentage
# there is one outlier (wrong fromat in original fine)

df <- df |>
  mutate(senescence = ifelse(senescence < 5, senescence, 4.7)) |>
  mutate(stage = as.integer(senescence),
         condition = substr(ID,1,nchar(ID)-2)) |>
  mutate(condition = str_remove_all(condition, "\\."))


# see the temporal distribution

temp_leaf <- df |>
  group_by(condition, date) |>
  summarise(leaf_out = mean(senescence, na.rm = T)) |>
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
  ungroup()

ggplot(temp_leaf, aes(x = diff_date, color = condition, group = condition)) +
  geom_point(aes(y = leaf_out), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Leaf out progression",
    x = "Days since start",
    y = "Leaf out",
    color = "Condition"
  ) +
  facet_wrap(~condition) + theme(legend.position = "none")

sigmoid_base_formula <- leaf_out ~ base + (max_val - base) / (1 + exp(-i * (diff_date - t50)))

fit_data_fixed <- temp_leaf |>
  group_nest(condition) |>
  mutate(model = map(data, ~ {
    tryCatch(
      nls(sigmoid_base_formula, 
          data = .x, 
          start = list(base = 0.1, max_val = 4.5, i = 0.1, t50 = 30),
          control = nls.control(maxiter = 100)),
      error = function(e) return(NULL)
    )
  })) |>
  filter(!map_lgl(model, is.null)) |>
  mutate(augmented = map2(model, data, ~augment(.x, data = .y))) |>
  unnest(augmented)

ggplot(fit_data_fixed, aes(x = diff_date, color = condition)) +
  geom_point(aes(y = leaf_out), alpha = 0.4) +
  geom_line(aes(y = .fitted), linewidth = 1) +
  facet_wrap(~condition) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Sigmoid fit of leaf out Progression",
    x = "Days since start",
    y = "Leaf out"
  )

fitted_parameters_df_leaf <- fit_data_fixed |>
  group_nest(condition) |>
  mutate(model = map(data, ~ {
    tryCatch(
      # Ensure data and start are INSIDE nls()
      nls(leaf_out ~ base + (max_val - base) / (1 + exp(-i * (diff_date - t50))), 
          data = .x, 
          start = list(base = 0.1, max_val = 4.5, i = 0.1, t50 = 30)),
      error = function(e) NULL
    )
  })) |>
  mutate(params = map(model, ~ if(!is.null(.x)) tidy(.x) else NULL)) |>
  unnest(params, keep_empty = TRUE) |> 
  select(condition, term, estimate, std.error, p.value)


final_params_wide_leaf <- fitted_parameters_df_leaf |>
  select(condition, term, estimate) |>
  pivot_wider(names_from = term, values_from = estimate)



#-------------

df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/height_and_diameter_processed.csv")

df <- df |>
  mutate(condition = substr(ID,1,nchar(ID)-2)) |>
  mutate(condition = str_remove_all(condition, "\\."))

diameter <- df |>
  select(-height) |>
  group_by(condition,date) |>
  summarise(diameter = mean(diameter, na.rm = T)) |>
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
  ungroup()


ggplot(diameter, aes(x = diff_date, color = condition, group = condition)) +
  geom_point(aes(y = diameter), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Diameter progression",
    x = "Days since start",
    y = "Diameter",
    color = "Condition"
  ) +
  facet_wrap(~condition) + theme(legend.position = "none")

height <- df |>
  select(-diameter) |>
  group_by(condition,date) |>
  summarise(height = mean(height, na.rm = T)) |>
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
  ungroup()

ggplot(height, aes(x = diff_date, color = condition, group = condition)) +
  geom_point(aes(y = height), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Height progression",
    x = "Days since start",
    y = "Height",
    color = "Condition"
  ) +
  facet_wrap(~condition) + theme(legend.position = "none")

#-------------

# calculate biomass from 2004

params_df <- data.frame(
  specie = c("BE", "OA"),
  a = c(0.0501, 0.0384),
  b = c(2.0526, 2.1245),
  c = c(0.8718, 0.8542)
)

biomass_df <- left_join(df |>
                          drop_na() |>
                          mutate(specie = substr(ID,1,2)),
                        params_df,
                        by = "specie"
) |>
  mutate(biomass = a * ((diameter)^b) * (height^c))

biomass_df |>
  group_by(condition, date) |>
  summarise(biomass = mean(biomass,na.rm = T)) |>
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
  ungroup() |>
  ggplot( aes(x = diff_date, color = condition, group = condition)) +
  geom_point(aes(y = biomass), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "Fake biomass progression",
    x = "Days since start",
    y = "Fake biomass",
    color = "Condition"
  ) +
  facet_wrap(~condition) + theme(legend.position = "none")



better_table <-  data.frame(
  specie = c("Betula pendula", "Quercus petraea", "Quercus robur", "Quercus rubra"),
  a = c(0.37119, 0.5274, 0.67311, 0.10626),
  b = c(0.87982, 0.81213, 0.85202, 1.09349)
)

better_table_2 <-  data.frame(
  specie = c("BE", "OA", "Quercus robur", "Quercus rubra"),
  a = c(0.37119, 0.5274, 0.67311, 0.10626),
  b = c(0.87982, 0.81213, 0.85202, 1.09349)
)

better_table_3 <-  data.frame(
  specie = c("BE", "Quercus petraea", "Quercus robur", "OA"),
  a = c(0.37119, 0.5274, 0.67311, 0.10626),
  b = c(0.87982, 0.81213, 0.85202, 1.09349)
)


left_join(df |>
            drop_na() |>
            mutate(specie = substr(ID,1,2)),
          better_table_2,
          by = "specie") |>
  mutate(biomass = a * ((diameter*height)^b)) |>
  group_by(condition, date) |>
  summarise(biomass = mean(biomass,na.rm = T)) |>
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
  ungroup() |>
  ggplot( aes(x = diff_date, color = condition, group = condition)) +
  geom_point(aes(y = biomass), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "biomass 1 progression",
    x = "Days since start",
    y = "biomass",
    color = "Condition"
  ) +
  facet_wrap(~condition) + theme(legend.position = "none")


left_join(df |>
            drop_na() |>
            mutate(specie = substr(ID,1,2)),
          better_table_3,
          by = "specie") |>
  mutate(biomass = a * ((diameter*height)^b)) |>
  group_by(condition, date) |>
  summarise(biomass = mean(biomass,na.rm = T)) |>
  mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
  ungroup() |>
  ggplot( aes(x = diff_date, color = condition, group = condition)) +
  geom_point(aes(y = biomass), alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(
    title = "biomass 2 progression",
    x = "Days since start",
    y = "biomass",
    color = "Condition"
  ) +
  facet_wrap(~condition) + theme(legend.position = "none")


##------------

# len of season

sen_df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/phenology_processed.csv")

sen_start_date = min(sen_df$date)

sed_end_date <- final_params_wide |>
  mutate(t50_i = as.integer(t50)) |>
  mutate(end_date = ymd(sen_start_date) + t50_i) |>
  select(condition, end_date)


leaf_df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/leaf_out_processed.csv")

leaf_start_date = min(leaf_df$date)

leaf_start_date <- final_params_wide_leaf |>
  mutate(t50_i = as.integer(t50)) |>
  mutate(start_date = ymd(leaf_start_date) + t50_i) |>
  select(condition, start_date)


seas_len <- left_join(
  
  sed_end_date |>
    mutate(condition = substr(condition,1,8)) |>
    mutate(condition = str_remove_all(condition, "\\_")) |>
    group_by(condition) |>
    summarise(end_date = mean(end_date)) |>
    ungroup(),
  leaf_start_date,
  by = "condition"
  ) |> 
  mutate(season_len = end_date - start_date)


# biomass vs len season

biomass <- left_join(df |>
                       drop_na() |>
                       mutate(specie = substr(ID,1,2)),
                     better_table_2,
                     by = "specie") |>
  mutate(biomass = a * ((diameter*height)^b)) |>
  group_by(condition, date) |>
  summarise(biomass = mean(biomass,na.rm = T)) |>
  ungroup() |>
  filter(date == "2025-11-07")

mass_len <- left_join(
  seas_len |> mutate(season_len = as.integer(season_len)),
  biomass, by = "condition"
)

ggplot(mass_len, aes(x = biomass, y = season_len, color = condition)) + geom_point()









