# data_load.r

## Source code ------------------------
source("helper_scripts/packages_attach.r")

## Load data ------------------------
data_dir <- file.path("data")
files <- list.files(data_dir, pattern = "^MoSI_.*\\.xlsx$", full.names = TRUE)
files <- sort(files)
df <- read_excel(files[1])
df_effort <- read_excel(files[2])

## Filter out TREEs ------------------------
df <- df %>%
    filter(Station != "CT")
df_effort <- df_effort %>%
    filter(Station != "CT")
