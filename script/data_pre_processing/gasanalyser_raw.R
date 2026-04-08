# workflow_gasanalyzer.R
# Reading and recalculating LI-6800 data using the gasanalyzer package.
#
# gasanalyzer reads the xlsx file directly, extracts the embedded Excel
# equations, recalculates all derived quantities (A, E, gsw, Ci, etc.)
# in R, and returns a tidy tibble with standardised column names.
#
# Install once:
#   install.packages("gasanalyzer")

library(gasanalyzer)
library(dplyr)
library(ggplot2)
library(units)

# ── 1. Import ─────────────────────────────────────────────────────────────────
# read_li6800_xlsx() parses the file, applies instrument calibration,
# recalculates all GasEx/FLR columns using the equations stored in the xlsx,
# and returns a tibble. Column names follow the gasanalyzer standard:
# Category.variable (e.g. GasEx.A, Meas.CO2s, FLR.phiPS2).

dat <- read_6800_xlsx("G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-09-24_Phenosink_GH/2025-09-24-BE CH C14.xlsx")

# Quick overview
glimpse(dat)
cat("\nColumn categories:\n")
print(unique(sub("\\..*", "", names(dat))))

# ── 2. Inspect key gas exchange columns ───────────────────────────────────────
# These are the columns that were formula-strings before; gasanalyzer
# has now computed them numerically.

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
print(gasex)

# ── 3. Inspect fluorescence columns (requires 6800-01A head) ─────────────────
flr <- dat |>
  select(
    obs     = SysObs.Obs,
    PhiPS2  = FLR.phiPS2,    # PSII operating efficiency
    ETR     = FLR.ETR,       # electron transport rate, µmol m-2 s-1
    NPQ     = FLR.NPQ,       # non-photochemical quenching
    qP      = FLR.qP,        # photochemical quenching
    FvFm    = `FLR.Fv_Fm`   # max PSII efficiency (dark-adapted)
  )

print(flr)

# ── 4. Quality control checks ─────────────────────────────────────────────────
# Check leak percentage — values > 10% indicate a poor chamber seal
cat("\nLeak check (should be < 10%):\n")
dat |> select(obs = SysObs.Obs, LeakPct = Leak.LeakPct) |> print()

# Check match correction — large values suggest the IRGAs need matching
cat("\nCO2 match correction (µmol/mol; ideally < 1):\n")
dat |> select(obs = SysObs.Obs, co2_match = MchEvent.CO2pMatch) |> print()

# ── 5. Recalculate with a corrected constant (example) ────────────────────────
# Suppose you discover the leaf area used was wrong (e.g., 5 cm² not 6 cm²).
# gasanalyzer lets you update constants and recompute without touching the file.

# dat_corrected <- dat |>
#   mutate(Const.S = set_units(5, cm^2)) |>   # correct leaf area
#   recalculate()                              # rerun all GasEx equations
# 
# cat("\nA with original 6 cm² vs corrected 5 cm² leaf area:\n")
# bind_cols(
#   dat     |> select(A_original  = GasEx.A),
#   dat_corrected |> select(A_corrected = GasEx.A)
# ) |> print()

# ── 6. Export tidy data ───────────────────────────────────────────────────────
write.csv(gasex, "BE_CH_C7_gasanalyzer.csv", row.names = FALSE)
cat("\nWritten: BE_CH_C7_gasanalyzer.csv\n")

# ── 7. Quick plot (useful once you have a full curve) ─────────────────────────
# With only 2 points this is illustrative; with a full A-Ci or A-Q dataset
# replace gasex with your full data frame.

# ggplot(gasex, aes(x = Ci, y = A)) +
#   geom_point(size = 3, colour = "#2c7bb6") +
#   geom_line(colour = "#2c7bb6", linetype = "dashed") +
#   labs(
#     title = "A-Ci response — BE_CH_C7",
#     x     = expression(C[i] ~ (µmol ~ mol^{-1})),
#     y     = expression(A ~ (µmol ~ m^{-2} ~ s^{-1}))
#   ) +
#   theme_bw()

#ggsave("BE_CH_C7_ACi_gasanalyzer.pdf", width = 6, height = 4)
cat("Written: BE_CH_C7_ACi_gasanalyzer.pdf\n")