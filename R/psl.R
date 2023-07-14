library(googledrive)
library(dplyr)
library(purrr)
library(terra)

## Download all wind soil loss rasters from Google Drive (which were
## exported from Google Earth Engine)

# Create folder to save rasters to
output_dir <- "data/psl"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


## Instead of saving a different image for each month,
## I compressed all images into separate bands.
## This code is commented out in case I change 
## my mind about this approach.
## 
# List files in folder
# psl_id <- drive_ls("erosion/psl")
# 
# # Save each file to the folder created above
# walk2(psl_id$id,
#       psl_id$name,
#       \(x, y) drive_download(
#         as_id(x),
#         path = file.path("data/psl/", y),
#         overwrite = TRUE
#       ))

# ## Load files as raster collection
# psl_rasters <- map(list.files(output_dir, full.names = TRUE), rast)
# 
# walk(psl_rasters, plot)

# Monthly raster (one band per month) ------------
psl_monthly <- drive_get("psl_monthly_2018.tif")
drive_download(as_id(psl_monthly$id),
               file.path("data/psl/", psl_monthly$name),
               overwrite = TRUE)
psl_monthly <- rast(file.path("data/psl/psl_monthly_2018.tif"))

# Change band names
names(psl_monthly) <- paste0(month.abb)

# Plot the rasters
panel(
  psl_monthly,
  axes = FALSE,
  col = rev(colorspace::heat_hcl(50, h = c(36, 90)))
)

# Get summary of pixels
summary(psl_monthly)

# Annual raster ----------
psl_annual <- drive_get("psl_annual_2018.tif")
drive_download(as_id(psl_annual$id),
               file.path("data/psl/", psl_annual$name),
               overwrite = TRUE)
psl_annual <- rast(file.path("data/psl/psl_annual_2018.tif"))

# Change band names
names(psl_annual) <- "2018 Annual Sum"

# Plot the raster
plot(
  psl_annual,
  main = expression("Potential Soil Loss in 2018 (kg/m"^2*")"),
  col = rev(colorspace::heat_hcl(50, h = c(36, 90))),
  axes = FALSE,
  type = "continuous"
)

# Get summary of pixels
summary(psl_annual)
