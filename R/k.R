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

# Get k raster
k <- drive_get("erosion/k.tif")

drive_download(as_id(k$id),
  file.path("data/psl/", k$name),
  overwrite = TRUE
)

k <- rast(file.path("data/k/k.tif"))

# Plot
plot(
  k,
  col = colorspace::sequential_hcl(
    n = 40,
    h = 0,
    c = c(0, NA, NA), 
    l = c(35, 100), 
    power = 1.5, 
    rev = TRUE
  ),
  axes = FALSE,
  type = "continuous"
)

# Summary
summary(k)
