# data_load.r

## Source code ------------------------
source("helper_scripts/packages_attach.r")

## Load data ------------------------
data_dir <- file.path("data")
find_data_file <- function(pattern) {
    matches <- list.files(data_dir, pattern = pattern, full.names = TRUE)
    if (length(matches) != 1) {
        stop(
            "Expected exactly one file in '", data_dir, "' matching '", pattern,
            "' but found ", length(matches), ": ", paste(basename(matches), collapse = ", ")
        )
    }
    matches
}
df <- read_excel(find_data_file("^MoSI_Bird Banding Database.*\\.xlsx$"))
df_effort <- read_excel(find_data_file("^MoSI_Bird Banding Effort.*\\.xlsx$"))

## Filter out TREEs ------------------------
df <- df %>%
    filter(Station != "CT")
df_effort <- df_effort %>%
    filter(Station != "CT")
