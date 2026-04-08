# tidy_BE_CH_C7.R
# Tidies a LI-COR LI-6800 photosynthesis system Excel export.
#
# Measurements sheet structure (1-indexed Excel rows):
#   Rows 1-13  : instrument constants / metadata (skipped)
#   Row  14    : category labels (SysObs, GasEx, Leak, FLR, Meas, ...)
#   Row  15    : variable names  (obs, time, E, A, CO2_s, ...)
#   Row  16    : units           (s, mol m-2 s-1, ...)
#   Row  17+   : data observations
#
# Computed (GasEx, FLR …) columns contain unresolved Excel formulas when the
# file is read outside Excel/LibreOffice; those columns are flagged and dropped.
# Raw sensor columns (Meas.*) always contain numeric values.
#
# Output files:
#   BE_CH_C7_tidy.csv    – one row per observation, one column per variable
#   BE_CH_C7_units.csv   – column <-> unit lookup table
#   BE_CH_C7_remarks.csv – instrument metadata from the Remarks sheet

library(readxl)
library(dplyr)
library(stringr)

input_file <- ""G:/.shortcut-targets-by-id/1zbxQei-0Me66EEDZIcStCy775kBgSNti/Phenosink/Experiment1_working_in_greenhouse/Raw Data/Li-Cor/2025-09-24_Phenosink_GH/2025-09-24-BE CH C14.xlsx""

# ── 1. Read raw sheet – no header, all cells as text ─────────────────────────
raw <- read_excel(
  input_file,
  sheet     = "Measurements",
  col_names = FALSE,
  col_types = "text"
)

# ── 2. Pull the three header rows (R rows 14-16 -> data frame rows 14-16) ─────
category_row <- as.character(unlist(raw[14, ]))
varname_row  <- as.character(unlist(raw[15, ]))
units_row    <- as.character(unlist(raw[16, ]))

# ── 3. Forward-fill the category label across its span ────────────────────────
fill_fwd <- function(x) {
  current <- NA_character_
  vapply(x, function(v) {
    if (!is.na(v) && v != "NA") current <<- v
    current
  }, character(1))
}
category_filled <- fill_fwd(category_row)

# ── 4. Build column names as "Category.variable" ──────────────────────────────
col_names <- ifelse(
  !is.na(category_filled) & !is.na(varname_row) & varname_row != "NA",
  paste0(category_filled, ".", varname_row),
  NA_character_
)

# ── 5. Extract data rows (R row 17 onwards) ───────────────────────────────────
data_raw <- raw[17:nrow(raw), ]

# Drop fully empty rows
not_empty <- apply(data_raw, 1, function(r) any(!is.na(r) & r != "NA"))
data_raw  <- data_raw[not_empty, , drop = FALSE]

# Drop rows with no obs number in column 1
data_raw <- data_raw[!is.na(data_raw[[1]]) & data_raw[[1]] != "NA", , drop = FALSE]

if (nrow(data_raw) == 0) stop("No data rows found in Measurements sheet.")

# ── 6. Assign column names; keep only named columns ───────────────────────────
names(data_raw) <- col_names
keep_cols <- !is.na(col_names)
data_raw  <- data_raw[, keep_cols, drop = FALSE]
units_kept <- units_row[keep_cols]

# ── 7. Drop columns that contain only unresolved Excel formula strings ─────────
is_formula_col <- function(col) {
  non_na <- col[!is.na(col) & col != "NA"]
  length(non_na) > 0 && all(str_detect(non_na, "^="))
}
formula_flags <- vapply(data_raw, is_formula_col, logical(1))
if (any(formula_flags)) {
  message(sprintf(
    "Note: %d column(s) contain uncomputed Excel formulas and are dropped.\n",
    sum(formula_flags)
  ))
}
data_raw   <- data_raw[, !formula_flags, drop = FALSE]
units_kept <- units_kept[!formula_flags]

# ── 8. Coerce to numeric where possible ───────────────────────────────────────
coerce_col <- function(x) {
  nums <- suppressWarnings(as.numeric(x))
  non_na_x    <- x[!is.na(x)]
  non_na_nums <- nums[!is.na(x)]
  if (length(non_na_x) > 0 && sum(!is.na(non_na_nums)) / length(non_na_x) >= 0.5) {
    nums
  } else {
    x
  }
}
data_tidy <- as.data.frame(
  lapply(data_raw, coerce_col),
  stringsAsFactors = FALSE
)

# ── 9. Build units lookup table ────────────────────────────────────────────────
units_df <- data.frame(
  column = names(data_tidy),
  units  = units_kept,
  stringsAsFactors = FALSE
)

# ── 10. Read Remarks sheet ────────────────────────────────────────────────────
remarks_raw <- read_excel(input_file, sheet = "Remarks", col_names = FALSE,
                          col_types = "text")
remarks_df  <- setNames(
  as.data.frame(remarks_raw, stringsAsFactors = FALSE),
  c("key", "value")
)
remarks_df  <- remarks_df[!is.na(remarks_df$key), ]

# ── 11. Write outputs ─────────────────────────────────────────────────────────
write.csv(data_tidy,   "BE_CH_C7_tidy.csv",    row.names = FALSE)
write.csv(units_df,    "BE_CH_C7_units.csv",   row.names = FALSE)
write.csv(remarks_df,  "BE_CH_C7_remarks.csv", row.names = FALSE)

# ── 12. Console summary ───────────────────────────────────────────────────────
cat(sprintf(
  "\nDone.\n  Observations  : %d\n  Columns kept  : %d  (formula cols dropped: %d)\n",
  nrow(data_tidy), ncol(data_tidy), sum(formula_flags)
))
cat("\nColumn categories present:\n")
cats <- unique(sub("\\..*", "", names(data_tidy)))
cat(paste0("  ", cats), sep = "\n")

cat("\nPreview: raw sensor columns (Meas.*):\n")
meas_cols <- names(data_tidy)[str_starts(names(data_tidy), "Meas\\.")]
if (length(meas_cols) > 0) {
  print(data_tidy[, meas_cols, drop = FALSE])
} else {
  cat("  (none found)\n")
}