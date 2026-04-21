library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(broom)
library(purrr)
library(stringr)
library(ggExtra)
library(readr)
library(purrr)

# combine all the variables

base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data"

processed_data <-  list.files(base_path, pattern = "\\.csv$", full.names = TRUE)

# air temperature

air <- read_csv(processed_data[1])

#h_d <- read_csv(processed_data[2])

leaf_t <- read_csv(processed_data[4])

#licor <- read_csv(processed_data[5])

#npq <- read_csv(processed_data[6])

par <- read_csv(processed_data[7])

soil <- read_csv(processed_data[9])

# adjust the values based on  https://www.notion.so/Phenosink-Experiments-11b565043f778053be99ea94c61c6b00?p=2f4565043f7780ea814ac0377e00418d&pm=s

leaf_sensor_map <- c(

  "BE CH T1.6"   = 12,
  "BE CH T1.10"  = 26,
  "BE CH T3.6"   = 22,
  "BE CH T3.10"  = 9,
  "BE CH C.15"    = 1,
  "BE CH 11"     = 2,

  "BE PO T1.10"  = 32,
  "BE PO T.11"   = 28,
  "BE PO T2.6"   = 24,
  "BE PO T2.10"  = 23,
  "BE PO T3.6"   = 16,
  "BE PO T3.10"  = 15,
  "BE PO C.6"     = 7,
  "BE PO C.10"    = 8,

  "OA CH T1.6"   = 30,
  "OA CH T1.10"  = 29,
  "OA CH T2.6"   = 25,
  "OA CH T2.10"  = 21,
  "OA CH T3.6"   = 14,
  "OA CH T3.10"  = 13,
  "OA CH C.6"     = 6,
  "OA CH C.10"    = 5,

  "OA PO T1.10"  = 27,
  "OA PO T1.14"  = 34,
  "OA PO T2.6"   = 20,
  "OA PO T2.10"  = 19,
  "OA PO C.6"     = 4,
  "OA PO C.10"    = 3
)

leaf_no_18 <- leaf_t  |> filter(leaf != 18)

lookup <- names(leaf_sensor_map)
names(lookup) <- leaf_sensor_map

leaf_no_18$tree <- lookup[as.character(leaf_no_18$leaf)]


# attach air sensor to the correct treatment

air_sensor <- c("C" = 1, 
                "OA T3" = 2, 
                "OA T2" = 3, 
                "BE T2" = 4, 
                "BE T1" = 5, 
                "BE T2" = 6)

lookup <- names(air_sensor)
names(lookup) <- air_sensor

air$tree <- lookup[as.character(air$sensor)]

# soil water content and temperature 

soil_sensor <- c("OA CH T1.2" = 1, 
                "OA CH T1.10" = 2, 
                "BE PO T1.2" = 3, 
                "BE PO T1.10" = 4, 
                "BE CH T1.2" = 5, 
                "BE CH T1.10" = 6,
                "OA PO T1.2" = 7, 
                "OA PO T1.10" = 8, 
                "BE CH T2.2" = 9, 
                "BE CH T2.10" = 10, 
                "OA PO T2.2" = 11, 
                "OA PO T2.10" = 12,
                "BE PO T2.2" = 13, 
                "BE PO T2.10" = 14,
                "BE CH T3.2" = 15, 
                "BE CH T3.10" = 16, 
                "OA PO T3.2" = 17, 
                "OA PO T3.10" = 18, 
                "OA PO T3.2" = 19, 
                "OA PO T3.10" = 20,
                "OA CH T3.2" = 21, 
                "OA CH T3.10" = 22, 
                "BE PO T3.10" = 23, 
                "BE PO T3.2" = 24, 
                "BE CH C.4" = 25, 
                "BE CH C.5" = 26,
                "OA PO C.2" = 27, 
                "OA PO C.10" = 28,
                "OA CH C.2" = 29, 
                "OA CH C.10" = 30,
                "BE PO C.2" = 31, 
                "BE PO C.10" = 32
                )

lookup <- names(soil_sensor)
names(lookup) <- soil_sensor

soil$tree <- lookup[as.character(soil$ID)]


# create daily data for P model
# combine lear T, air T, PAR and soil T

# first check the date range of soil and leaf T

print(paste0("starting leaf T date: ",min(leaf_no_18$date)))
print(paste0("ending leaf T date: ",max(leaf_no_18$date)))

print(paste0("starting soil T date: ",min(soil$date)))
print(paste0("ending soil T date: ",max(soil$date)))

start_date <- max(min(soil$date),min(leaf_no_18$date))
end_date <- min(max(soil$date),max(leaf_no_18$date))

print(paste0("interval of date: from ",start_date, " to ",end_date))


leaf_f <- leaf_no_18 |>
  filter(date >= start_date & date <= end_date) |>
  rename(leaf_t = temp) |>
  mutate(leaf_t = leaf_t -273.15)

soil_f <- soil |>
  filter(date >= start_date & date <= end_date)|>
  rename(soil_t = temperature)

# now check the plant who has the sensor in both

tree_leaf <- unique(leaf_f$tree)

soil_leaf <- unique(soil_f$tree)


match <- tree_leaf[tree_leaf %in% soil_leaf]
matc2 <- soil_leaf[soil_leaf %in% tree_leaf]

# these two are indentical

leaf_f <- leaf_f[leaf_f$tree %in% match,]

soil_f <- soil_f[soil_f$tree %in% match,]


# add air temeperature, measured by treatment

print(unique(air$tree))

# for simplicity now, we take the mean of T3

air_f <- air |>
  filter(date >= start_date & date <= end_date) |>
  rename(air_t = temp) |>
  mutate(air_t = air_t - 273.15) |>
  mutate(tree = ifelse(nchar(tree) == 1,tree,substr(tree,4,5))) |>
  group_by(date,time,tree) |>
  summarise(air_t = mean(air_t,na.rm = T),
            humidity = mean(humidity,na.rm = T)) |>
  ungroup() |>
  rename(treatment = tree)

# add PAR

par_f <- par |>
  filter(date >= start_date & date <= end_date) |>
  rename(par = PAR.µmol.m.s.) |>
  mutate(par = as.double(par)) |>
  mutate(condition = ifelse(condition == "control","C",condition)) |>
  rename(treatment = condition)



# resize to dailiy value and then combined to run the P model

#for filter now fixed sunrise and sunset
sunrise <- 6
sunset <- 20

leaf_d <- leaf_f |>
  filter(time > sunrise & time < sunset) |>
  group_by(tree,date) |>
  summarise(leaf_t = mean(leaf_t,na.rm = T)) |>
  ungroup()

soil_d <- soil_f |>
  filter(time > sunrise & time < sunset) |>
  group_by(tree,date) |>
  summarise(soil_t = mean(soil_t,na.rm = T)) |>
  ungroup()

air_d <- air_f |>
  filter(time > sunrise & time < sunset) |>
  group_by(treatment,date) |>
  summarise(air_t = mean(air_t,na.rm = T),
            humidity = mean(humidity,na.rm = T)) |>
  ungroup()

par_d <- par_f |>
  filter(time > sunrise & time < sunset) |>
  group_by(treatment,date) |>
  summarise(par = mean(par,na.rm = T)) |>
  ungroup()

# start by combining leaf and soil

combined <- full_join(leaf_d, soil_d) |> drop_na()

# add treatment column to add par and air

combined <- combined |>
  mutate(treatment = substr(tree,7,8)) |>
  mutate(treatment = str_remove_all(treatment, "\\."))

combined <- full_join(combined,air_d) |> drop_na()

combined <- full_join(combined,par_d) |> drop_na()

combined <- combined |>
  mutate()


calc_pmodel <- function(Tair, Tleaf, Tsoil, PAR, RH, Ca = 415, u = 0.1, d = 0.05, elevation = 408) {
  
  # --- 1. Constants ---
  cp  <- 1005     # Specific heat of dry air (J/kg/K) 
  MW  <- 0.018    # Molar mass of water (kg/mol)
  R   <- 8.314    # Universal gas constant (J/mol/K)
  beta <- 146     # Stocker P-model cost ratio (dimensionless)
  
  # --- 2. Atmospheric Pressure (kPa) ---
  Patm    <- 101.325 * (1 - 2.2569e-5 * elevation)^5.2553  # kPa
  
  # --- 3. Humidity & VPD (all in kPa) ---
  es_air  <- 0.6108 * exp((17.27 * Tair)  / (Tair  + 237.3))  # kPa
  ea      <- es_air * RH                                      # kPa
  VPD_air <- es_air - ea                                      # kPa
  es_leaf <- 0.6108 * exp((17.27 * Tleaf) / (Tleaf + 237.3))  # kPa
  
  # --- 4. Energy Balance ---
  rho_a    <- (Patm * 1000) / (287.05 * (Tair + 273.15))  # kg/m³; Patm×1000 = Pa
  lambda_v <- (2.501 - 0.002361 * Tair) * 1e6              # J/kg (latent heat of vaporisation)
  
  # Aerodynamic (boundary layer) conductance
  # NOTE: these empirical coefficients give ga in m/s for typical leaf geometry
  gv_forced <- 0.00662 * sqrt(u / d)                            # m/s
  gv_free   <- 0.0021  * ((abs(Tleaf - Tair) / d)^0.25)        # m/s
  ga_ms     <- gv_forced + gv_free                              # m/s
  ga_mol    <- ga_ms * (Patm * 1000) / (R * (Tair + 273.15))   # mol/m²/s
  
  # Net radiation from PAR (PAR/4.6 converts µmol/m²/s → W/m² for broadband solar;
  # this is an approximation assuming PAR represents full shortwave input)
  Rn <- (PAR / 4.6) * (1 - 0.23)   # W/m² (albedo = 0.23)
  G  <- 0.1 * Rn                    # W/m² (ground heat flux, ~10% of Rn)
  H  <- rho_a * cp * (Tleaf - Tair) * ga_ms  # W/m²: (kg/m³)(J/kg/K)(K)(m/s) = W/m²
  
  lambdaE <- max(Rn - G - H, 0.1)  # W/m²; floor prevents negative E
  
  # Transpiration rate: W/m² ÷ (J/kg × kg/mol) = mol/m²/s
  E_mol <- lambdaE / (lambda_v * MW)
  
  # Total conductance to water vapour:
  # g = E / (Δe / Patm), with Δe and Patm both in kPa → kPa cancel, result in mol/m²/s
  gtot  <- (E_mol * Patm) / (es_leaf - ea)   # mol/m²/s
  
  # Stomatal conductance (series resistance formula):
  # Guard against near-zero or negative denominator when gtot ≤ ga_mol
  inv_gs <- (1 / gtot) - (1 / ga_mol)
  gs_w   <- if (inv_gs > 0) 1 / inv_gs else 0.001  # mol/m²/s (conductance to H₂O)
  
  # --- 5. P-Model ---
  Tk <- Tleaf + 273.15  # K
  
  # CO₂ compensation point (Pa)
  gamma_star_pa <- 4.22  * exp(37830 * (Tk - 298.15) / (298.15 * R * Tk))  # Pa
  
  # Michaelis-Menten constants — all unified in Pa for K_eff
  Kc_pa <- 39.97  * exp(79430 * (Tk - 298.15) / (298.15 * R * Tk))  # Pa  (CO₂)
  Ko_pa <- 27480  * exp(36380 * (Tk - 298.15) / (298.15 * R * Tk))  # Pa  (O₂); 27.48 kPa × 1000
  Oa_pa <- 0.209476 * Patm * 1000                                     # Pa  (O₂ partial pressure)
  # NOTE: Oa/Ko ratio is dimensionless (Pa/Pa), K_eff_pa ends up in Pa
  K_eff_pa <- Kc_pa * (1 + Oa_pa / Ko_pa)                             # Pa
  
  # Viscosity correction (dimensionless relative to 25 °C)
  eta_star <- exp(-3.719 + 580 / (Tleaf + 138)) /
    exp(-3.719 + 580 / (25    + 138))
  
  # Ca as partial pressure (Pa): ppm × (Patm_kPa/1000) → Pa
  Ca_pa  <- Ca * (Patm / 1000)   # Pa
  
  # VPD in Pa for the xi equation
  VPD_pa <- VPD_air * 1000       # Pa
  
  # Optimal Ci/Ca (xi in √Pa, √VPD_pa in √Pa → chi dimensionless)
  xi  <- sqrt((beta * (K_eff_pa + gamma_star_pa)) / (1.6 * eta_star))
  chi <- (gamma_star_pa / Ca_pa) +
    (1 - gamma_star_pa / Ca_pa) * (xi / (xi + sqrt(VPD_pa)))
  
  # Gross assimilation (µmol/m²/s):
  # gs_w/1.6 = conductance to CO₂ (mol/m²/s)
  # Ca in µmol/mol = mole fraction ×10⁻⁶ → product is µmol/m²/s
  A_gross <- (gs_w / 1.6) * Ca * (1 - chi)
  
  return(list(
    Patm_kPa          = Patm,
    VPD_air_kPa       = VPD_air,
    gs_water_mol_m2_s = gs_w,
    A_gross_umol_m2_s = A_gross,
    Ci_Ca_ratio       = chi,
    E_mol_m2_s        = E_mol
  ))
}

combined_results <- combined %>%
  mutate(
    pmodel_results = pmap(
      # Map the dataframe columns to the exact argument names of the function
      .l = list(
        Tair = air_t,
        Tleaf = leaf_t,
        Tsoil = soil_t,
        PAR = par,
        RH = humidity 
      ),
      .f = calc_pmodel
    )
  ) %>%
  # Unpack the list into individual columns
  unnest_wider(pmodel_results)

# View the result
head(combined_results)





