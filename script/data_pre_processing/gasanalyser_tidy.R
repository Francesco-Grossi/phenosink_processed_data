# create the function used to tidy the xlsx
library(gasanalyzer)
library(dplyr)

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

tidy_fluo_data <- function(path, name){
  
  dat <- read_6800_xlsx(path)
  
  flr <- dat |>
    select(
      obs     = SysObs.Obs,
      date    = SysObs.Date,
      PhiPS2  = FLR.phiPS2,    # PSII operating efficiency
      ETR     = FLR.ETR,       # electron transport rate, µmol m-2 s-1
      NPQ     = FLR.NPQ,       # non-photochemical quenching
      qP      = FLR.qP,        # photochemical quenching
      FvFm    = `FLR.Fv_Fm`   # max PSII efficiency (dark-adapted)
    )
  
  flr$condition = name
  
  return(flr)
  
}

extract_fluo_from_folder <- function(folder_path, start_name, end_name,final_fluo){
  
  xlsx <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  for(i in xlsx){
    
    
    name <- substr(i,start_name,end_name)
    
    tmp_fluo <- tidy_fluo_data(i,name)
    final_fluo <- rbind(final_fluo, tmp_fluo)
    print(name)
    
  }
  
  return(final_fluo)
  
}



extract_licor_from_folder <- function(folder_path, start_name, end_name,final_licor){
  
  xlsx <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  for(i in xlsx){
    
    
    name <- substr(i,start_name,end_name)
    
    tmp_licor <- tidy_licor_data(i,name)
    final_licor <- rbind(final_licor, tmp_licor)
    print(name)
    
  }
  
  return(final_licor)
  
}


base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-09-24_Phenosink_GH/"

final_licor <- NULL
final_fluo <- NULL

final_licor <- extract_licor_from_folder(base_path,157,164,final_licor)

final_fluo <- extract_fluo_from_folder(base_path,157,164,final_fluo)


base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-07-28-phenosink-greenhouse"


final_licor <- extract_licor_from_folder(base_path,154,161,final_licor)

final_fluo <- extract_fluo_from_folder(base_path,154,161,final_fluo)


base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-06-26_phenosink_greenhouse"


final_licor <- extract_licor_from_folder(base_path,154,161,final_licor)

final_fluo <- extract_fluo_from_folder(base_path,154,161,final_fluo)


base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-07-08_phenosink_greenhouse_calvin"


final_licor <- extract_licor_from_folder(base_path,161,168,final_licor)

final_fluo <- extract_fluo_from_folder(base_path,161,168,final_fluo)


base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-08-20-phenosink-greenhouse"


final_licor <- extract_licor_from_folder(base_path,154,161,final_licor)

final_fluo <- extract_fluo_from_folder(base_path,154,161,final_fluo)



base_path <- "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-08-20-phenosink-greenhouse_benson"


final_licor <- extract_licor_from_folder(base_path,161,168,final_licor)

final_fluo <- extract_fluo_from_folder(base_path,161,168,final_fluo)


write.table(x = final_licor, 
            file = "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/licor_processed.csv", 
            sep = ",", 
            row.names = FALSE)


# write.table(x = final_fluo, 
#             file = "G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/processed data/processed_data/licor_fluo_processed.csv", 
#             sep = ",", 
#             row.names = FALSE)



