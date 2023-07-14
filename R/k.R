library(googledrive)
library(dplyr)
library(purrr)
library(terra)

# Download all wf rasters from Google Drive (which were
# exported from Google Earth Engine)

# Create folder to save rasters to
output_dir <- "data/k"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# List files in folder
k_id <- drive_ls("erosion/k")

# Save each file to the folder created above
walk2(k_id$id,
      k_id$name,
      \(x, y) drive_download(
        as_id(x),
        path = file.path("data/k", y),
        overwrite = TRUE
      ))

# Load files as raster collection
raster_files <- list.files(output_dir, full.names = TRUE)
k_rasters <- map(raster_files, rast)

k <- terra::vrt(raster_files)

# Plot the rasters
plot(k, axes = FALSE)

# Write combined raster
writeRaster(k, paste0("output/k.tif"), overwrite = TRUE)

# Summary
summary(k)
