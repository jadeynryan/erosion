library(googledrive)
library(dplyr)
library(purrr)
library(terra)

# Download all slrc rasters from Google Drive (which were
# exported from Google Earth Engine)

# Create folder to save rasters to
output_dir <- "data/slrc"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# List files in folder
slrc_id <- drive_ls("erosion/slrc")

# Save each file to the folder created above
walk2(
  slrc_id$id,
  slrc_id$name,
  \(x, y) drive_download(
    as_id(x),
    path = file.path("data/slrc/", y),
    overwrite = TRUE
  )
)

# Load files as raster collection
slrc_rasters <- map(list.files(output_dir, full.names = TRUE), rast)

# Flatten all rasters into one with multiple bands and set names to month abbr
slrc <- rast(slrc_rasters)
names(slrc) <- paste0(month.abb)

# Plot the rasters
panel(
  slrc,
  axes = FALSE,
  col = colorspace::sequential_hcl(
    n = 40,
    h = c(140, 80),
    c = c(50, NA, 10), 
    l = c(40, 97), 
    power = c(0.7, 1.8)
  )
)

# Summary
summary(slrc)
