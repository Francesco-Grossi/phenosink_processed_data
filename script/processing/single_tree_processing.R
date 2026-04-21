library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(broom)
library(purrr)
library(stringr)
library(ggExtra)

df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/phenology_processed.csv")


# !! Here change the grouping
fit_sigmoid <- df |> 
  drop_na() |>
  mutate(ID = str_remove_all(ID, "\\.")) |>
  mutate(ID = substr(ID,1,8)) |>
  mutate(ID = ifelse(substr(ID, 7, 7) == "C", 
                     substr(ID, 1, 7), 
                     ID)) |>
  mutate(condition = paste0(ID, "_", location)) |> 
  group_by(condition, date) 

conditions <- unique(fit_sigmoid$condition)

for (condition_id in conditions) {
  
  # 1. Prepare the data first
  plot_data <- df |> 
    drop_na() |> 
    mutate(condition = paste0(substr(ID, 1, 8), "_", location)) |> 
    mutate(condition = str_remove_all(condition, "\\.")) |> 
    filter(condition == condition_id)
  
  if (nrow(plot_data) > 0) {
    p <- plot_data |> 
      mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |> 
      ggplot(aes(x = diff_date, color = ID, group = ID)) +
      geom_point(aes(y = senescence), alpha = 0.6, size = 2) +
      theme_minimal() +
      labs(
        title = paste0("Senescence of ", condition_id),
        x = "Days since start",
        y = "Senescence",
        color = "Condition"
      ) +
      facet_wrap(~ ID) + 
      theme(legend.position = "none")
    
  } else {
    p <- ggplot() + 
      theme_void() + 
      geom_text(aes(0,0, label = paste("No observations for\n", condition_id))) +
      labs(title = condition_id)
  }
  
  ggsave(
    filename = paste0("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/phenosink_processed_data/tmp_png/raw_sen/", condition_id, "_raw_data.png"),
    plot = p
  )
  
}

###---------

head(df, n = 50)


# Define the new formula with a base (starting value)
# (1 - base) ensures the top asymptote remains at 1.0
sigmoid_base_formula <- senescence ~ base + (1 - base) / (1 + exp(-i * (diff_date - t50)))

for(condition_id in conditions){
  
  fit_data_fixed <- df |>
    drop_na() |>
    mutate(condition = paste0(substr(ID, 1, 8), "_", location)) |> 
    mutate(condition = str_remove_all(condition, "\\.")) |> 
    filter(condition == condition_id) |>
    mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
    mutate(senescence = senescence / 100)
  
  if (nrow(fit_data_fixed) > 0) {
    
    fit_data_fixed <- fit_data_fixed |>
    group_nest(ID) |>
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
  
  
  
  if (nrow(fit_data_fixed) > 0) {

    p <- ggplot(fit_data_fixed, aes(x = diff_date, color = ID)) +
      geom_point(aes(y = senescence), alpha = 0.6) +
      geom_line(aes(y = .fitted), linewidth = 1) +
      facet_wrap(~ID) +
      theme_minimal() +
      labs(title = paste0("Sigmoid Fit of",condition_id, "with Baseline Offset")) +
      theme(legend.position = "none")
    
  }else {
    
    p <- ggplot() + 
      theme_void() + 
      geom_text(aes(0,0, label = paste("No observations for\n", condition_id))) +
      labs(title = condition_id)
  }
  
  ggsave(
    filename = paste0("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/phenosink_processed_data/tmp_png/fitted_sen/", condition_id, "_fitted_data.png"),
    plot = p
  )
  }
}

##----------- PLot with i50 and t50 all trees at once
df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/phenology_processed.csv")


# !! Here change the grouping
fit_sigmoid <- df |> 
  drop_na() |>
  mutate(condition = paste0(ID,"_",location)) 

fit_sigmoid <- fit_sigmoid |>
  mutate(ID_plot = paste0(
    substr(condition, 1, 1),
    
    substr(condition, 4, 4),
    
    ifelse(substr(condition, 7, 7) == "C", 
           "0",
           ifelse(substr(condition, 8, 8) == ".",
                  substr(condition, 9, 9),
                  substr(condition, 8, 8))
           ),
    
    "_",
    
    toupper(substr(condition,nchar(condition)-4,nchar(condition)-4))),
    
    last_num = as.integer(str_extract(ID, "(?<=\\.)\\d+$"))) |>
  
  mutate(ID_plot = paste0(ID_plot,last_num))


IDs <- unique(fit_sigmoid$ID_plot)

final_param <- NULL

for(ids in IDs){
  
  print(ids)
  
  fitted_parameters <- fit_sigmoid |>
    filter(ID_plot == ids) |>
    ungroup() |>
    mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
    mutate(senescence = senescence / 100) |>
    ungroup() |>
    group_nest(ID_plot) |>
    mutate(model = map(data, ~ {
      tryCatch(
        nls(senescence ~ base + (1 - base) / (1 + exp(-i * (diff_date - t50))), 
            data = .x, 
            start = list(base = 0.2, i = 0.1, t50 = 30)),
        error = function(e) return(NULL)
      )
    })) |>
    mutate(params = map(model, ~ if(!is.null(.x)) tidy(.x) else NULL)) |>
    unnest(params)
  
  if (nrow(fitted_parameters)> 0){
    
    fitted_parameters <- fitted_parameters |>
      select(ID_plot, term, estimate, std.error, p.value)
    
    final_params_wide_leaf <- fitted_parameters |>
      select(ID_plot, term,estimate) |>
      pivot_wider(names_from = term, values_from = estimate)
    
    final_param <- rbind(final_param,final_params_wide_leaf)
 
    }
}

df_plot <- final_param |>
  mutate(specie = substr(ID_plot,1,1),
         region = substr(ID_plot,2,2),
         treatment = substr(ID_plot,3,3),
         location = substr(ID_plot,5,5),
         n_tree = ifelse(nchar(ID_plot) == 6,
                         substr(ID_plot,6,6),
                         substr(ID_plot,6,7))
         )

ggplot(df_plot, aes(x = i, y = t50)) + 
  geom_point() +
  ggtitle("overall fitted parameter distribution for senescence")

##---- highlisth different specei

p <- ggplot(df_plot, aes(x = i, y = t50, color = specie, fill = specie)) + 
  geom_point(alpha = 0.6, size = 2) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(title = "Parameter Distribution by Specie",
       x = "i", 
       y = "t50")

final_plot <- ggMarginal(p, 
                         type = "density", 
                         groupFill = TRUE, 
                         alpha = 0.4)
print(final_plot)

##--specie and provenience

df_B <- df_plot |>
  filter(specie == "B")

p <- ggplot(df_B, aes(x = i, y = t50, color = region, fill = region)) + 
  geom_point(alpha = 0.6, size = 2) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(title = "Betulla by region",
       x = "i", 
       y = "t50")

final_plot <- ggMarginal(p, 
                         type = "density", 
                         groupFill = TRUE, 
                         alpha = 0.4)
print(final_plot)


df_O <- df_plot |>
  filter(specie == "O")

p <- ggplot(df_O, aes(x = i, y = t50, color = region, fill = region)) + 
  geom_point(alpha = 0.6, size = 2) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(title = "Oak by region",
       x = "i", 
       y = "t50")

final_plot <- ggMarginal(p, 
                         type = "density", 
                         groupFill = TRUE, 
                         alpha = 0.4)

print(final_plot)

##---- different treatment for all betulla

p <- ggplot(df_B, aes(x = i, y = t50, color = treatment, fill = treatment)) + 
  geom_point(alpha = 0.6, size = 2) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(title = "Betulla by region",
       x = "i", 
       y = "t50")

final_plot <- ggMarginal(p, 
                         type = "density", 
                         groupFill = TRUE, 
                         alpha = 0.4)
print(final_plot)

##------ upper and lower by treatment
# control == 0

for(t in unique(df_plot$treatment)){
  
  df_B_T <- df_B |>
    filter(treatment == t)
  
  p <- ggplot(df_B_T, aes(x = i, y = t50, color = location, fill = location)) + 
    geom_point(alpha = 0.6, size = 2) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(title = paste0("Upper and lower betulla in treatment ",t),
         x = "i", 
         y = "t50")
  
  final_plot <- ggMarginal(p, 
                           type = "density", 
                           groupFill = TRUE, 
                           alpha = 0.4)
  print(final_plot)
}


for(t in unique(df_plot$treatment)){
  
  df_O_T <- df_O |>
    filter(treatment == t)
  
  p <- ggplot(df_O_T, aes(x = i, y = t50, color = location, fill = location)) + 
    geom_point(alpha = 0.6, size = 2) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(title = paste0("Upper and lower oak in treatment ",t),
         x = "i", 
         y = "t50")
  
  final_plot <- ggMarginal(p, 
                           type = "density", 
                           groupFill = TRUE, 
                           alpha = 0.4)
  print(final_plot)
}

## looking adn the difference (delta within the same tree)
# if the value are far from 0 = good

df_delta <- NULL

df_plot_2 <- df_plot |>
  mutate(trees = paste0(specie,region,treatment,n_tree))

for(tree in unique(df_plot_2$trees)){
  
  df_tmp <- df_plot_2 |>
    filter(trees == tree)
  
  if(nrow(df_tmp) > 1){
    print(tree)
  }
  
  
}



df_deltas <- df_plot_2 |>
  # We only need the ID/Group info and the values we want to subtract
  select(trees, specie, region, treatment, n_tree, location, i, t50) |>
  
  # Pivot L and U into the same row
  # This creates columns: i_L, i_U, t50_L, t50_U
  pivot_wider(
    id_cols = c(trees, specie, region, treatment, n_tree), 
    names_from = location, 
    values_from = c(i, t50),
    names_glue = "{.value}_{location}"
  ) |>
  
  # Calculate the differences (Delta)
  # Positive Delta means Upper is larger than Lower
  mutate(
    delta_i = i_U - i_L,
    delta_t50 = t50_U - t50_L
  ) |>
  drop_na(delta_i, delta_t50)

# betulla deltas better

df_deltas_B <- df_deltas |>
  filter(specie == "B")

df_deltas_O <- df_deltas |>
  filter(specie == "O")

#filter by condition and tree

for(t in unique(df_plot$treatment)){
  
  df_B_T <- df_deltas_B |>
    filter(treatment == t)
  
  p_b <- ggplot(df_B_T, aes(x = delta_i, y = delta_t50, color = n_tree, shape = region)) + 
    geom_point(alpha = 0.6, size = 2) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(title = paste0("Betulla difference in treatment ",t),
         x = "delta i", 
         y = "delta t50") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted")
  
  #print(p_b)
  
  
  df_O_T <- df_deltas_O |>
    filter(treatment == t)
  
  p_o <- ggplot(df_O_T, aes(x = delta_i, y = delta_t50, color = n_tree, shape = region)) + 
    geom_point(alpha = 0.6, size = 2) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(title = paste0("Oak difference in treatment ",t),
         x = "delta i", 
         y = "delta t50") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted")
  
  #print(p_o)
  
  ggsave(
    filename = paste0("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/phenosink_processed_data/tmp_png/delta_fitted/betulla_treatment_", t, "_delta_fitted.png"),
    plot = p_b,
    units = "cm",
    width = 16,
    height = 10
  )
  
  ggsave(
    filename = paste0("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/phenosink_processed_data/tmp_png/delta_fitted/oak_treatment_", t, "_delta_fitted.png"),
    plot = p_o,
    units = "cm",
    width = 16,
    height = 10
  )
  
  }
  
#---------

# boxplot divided by species, treatment and locatoin
# need to run the fit first 

df <- read.csv("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/phenology_processed.csv")


# !! Here change the grouping
fit_sigmoid <- df |> 
  drop_na() |>
  mutate(condition = paste0(ID,"_",location)) 

fit_sigmoid <- fit_sigmoid |>
  mutate(ID_plot = paste0(
    substr(condition, 1, 1),
    
    substr(condition, 4, 4),
    
    ifelse(substr(condition, 7, 7) == "C", 
           "0",
           ifelse(substr(condition, 8, 8) == ".",
                  substr(condition, 9, 9),
                  substr(condition, 8, 8))
    ),
    
    "_",
    
    toupper(substr(condition,nchar(condition)-4,nchar(condition)-4))),
    
    last_num = as.integer(str_extract(ID, "(?<=\\.)\\d+$"))) |>
  
  mutate(ID_plot = paste0(ID_plot,last_num))


IDs <- unique(fit_sigmoid$ID_plot)

final_param <- NULL

for(ids in IDs){
  
  print(ids)
  
  fitted_parameters <- fit_sigmoid |>
    filter(ID_plot == ids) |>
    ungroup() |>
    mutate(diff_date = as.integer(ymd(date) - min(ymd(date)))) |>
    mutate(senescence = senescence / 100) |>
    ungroup() |>
    group_nest(ID_plot) |>
    mutate(model = map(data, ~ {
      tryCatch(
        nls(senescence ~ base + (1 - base) / (1 + exp(-i * (diff_date - t50))), 
            data = .x, 
            start = list(base = 0.2, i = 0.1, t50 = 30)),
        error = function(e) return(NULL)
      )
    })) |>
    mutate(params = map(model, ~ if(!is.null(.x)) tidy(.x) else NULL)) |>
    unnest(params)
  
  if (nrow(fitted_parameters)> 0){
    
    fitted_parameters <- fitted_parameters |>
      select(ID_plot, term, estimate, std.error, p.value)
    
    final_params_wide_leaf <- fitted_parameters |>
      select(ID_plot, term,estimate) |>
      pivot_wider(names_from = term, values_from = estimate)
    
    final_param <- rbind(final_param,final_params_wide_leaf)
    
  }
}

df_plot <- final_param |>
  mutate(specie = substr(ID_plot,1,1),
         region = substr(ID_plot,2,2),
         treatment = substr(ID_plot,3,3),
         location = substr(ID_plot,5,5),
         n_tree = ifelse(nchar(ID_plot) == 6,
                         substr(ID_plot,6,6),
                         substr(ID_plot,6,7))
  )

plot_box_b <- df_plot |>
  filter(specie == "B")


ggplot(plot_box_b, aes(x = treatment, y = t50, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "T50 in Fagus",
    x = "Treatment",
    y = "t50",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


ggplot(plot_box_b, aes(x = treatment, y = i, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "i in Fagus",
    x = "Treatment",
    y = "i",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


plot_box_o <- df_plot |>
  filter(specie == "O")


ggplot(plot_box_o, aes(x = treatment, y = t50, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "T50 in Oak",
    x = "Treatment",
    y = "t50",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


ggplot(plot_box_o, aes(x = treatment, y = i, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "i in Oak",
    x = "Treatment",
    y = "i",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



df_final <- df_plot |>
  mutate(
    t10 = t50 - (log((1 - 0.1) / (0.1 - base)) / i),
    
    t90 = t50 - (log((1 - 0.9) / (0.9 - base)) / i)
  )

plot_box_b <- df_final |>
  filter(specie == "B")


ggplot(plot_box_b, aes(x = treatment, y = t90, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "t90 in Fagus",
    x = "Treatment",
    y = "t90",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

ggplot(plot_box_b, aes(x = treatment, y = t10, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "t10 in Fagus",
    x = "Treatment",
    y = "t10",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



plot_box_o <- df_final |>
  filter(specie == "O")


ggplot(plot_box_o, aes(x = treatment, y = t90, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "t90 in Oak",
    x = "Treatment",
    y = "t90",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


ggplot(plot_box_o, aes(x = treatment, y = t10, fill = location)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), 
    alpha = 0.6,
    size = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "t10 in Oak",
    x = "Treatment",
    y = "t10",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

