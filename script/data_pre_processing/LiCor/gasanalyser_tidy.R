# create the function used to tidy the xlsx
library(gasanalyzer)
library(dplyr)
library(ggplot2)

tidy_licor_data <- function(path, name){
  
  dat <- read_6800_xlsx(path)
  
  gasex <- dat |>
    select(
      obs     = SysObs.Obs,
      date    = SysObs.Date,
      A       = GasEx.A,
      E       = GasEx.E,
      gsw     = GasEx.gsw,
      Ci      = GasEx.Ci,
      Ca      = GasEx.Ca,
      VPDleaf = GasEx.VPDleaf,
      CO2_s   = Meas.CO2s,       # was Meas.CO2s
      CO2_r   = Meas.CO2r,       # was Meas.CO2r
      H2O_s   = Meas.H2Os,       # was Meas.H2Os
      H2O_r   = Meas.H2Or,       # was Meas.H2Or
      Flow    = Meas.Flow,
      Pa      = Meas.Pa,
      Tleaf   = Meas.Tleaf,
      Tair    = Meas.Tair,
      Q       = LeafQ.Qin       
    )

  gasex$condition = name
  
  return(gasex)
  
}

# list file in Raw Data/Li-Cor/2025-09-24_Phenosink_GH


base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-09-24_Phenosink_GH/"

xlsx <- list.files(base_path, pattern = "\\.xlsx$", full.names = TRUE)

final_df <- NULL

for(i in xlsx){
  
  
  name <- substr(i,157,164)
  
  tmp_df <- tidy_licor_data(i,name)
  final_df <- rbind(final_df, tmp_df)
  
  print(name)
  }
