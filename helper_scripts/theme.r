# theme.r

## Define station order ------------------------
station_levels <- c("BRL1", "FWC1", "CBS2", "RCR2", "TPPL", "CT")
station_labels <- c(
    "Black Rock", "Freshwater Creek",
    "Cockscomb Basin", "Runaway Creek",
    "TIDE Lands", "Toucan Ridge"
)
station_names <- setNames(station_labels, station_levels)

## Define color palettes ------------------------
palette <- c("green3", "purple3", "orange2", "skyblue2", "hotpink3", "#008080")
