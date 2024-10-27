# Packages
library(sf)
library(units)

# Conversion factor: km to miles
km_mi <- 0.6213712

# List GPX files
gpx_files_list <- list.files(pattern = "*.gpx")

# Read GPX files
gpx_list <- lapply(gpx_files_list, function(x) {
  st_read(x, layer = "tracks")
})
names(gpx_list) <- gsub("\\.gpx$", "", gpx_files_list)

# Calculate lengths of GPX files
lapply(gpx_list, function(x) {
  x_length <- drop_units(st_length(x))
  x_length_km <- x_length * 0.001
  x_length_mi <- x_length_km * km_mi

  return(c("km" = x_length_km, "mi" = x_length_mi))
})
