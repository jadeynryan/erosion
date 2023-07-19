library(googledrive)
library(dplyr)
library(purrr)
library(terra)

# Download all wf rasters from Google Drive (which were
# exported from Google Earth Engine)

# Create folder to save rasters to
output_dir <- "data/wf"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# List files in folder
wf_id <- drive_ls("erosion/wf")

# Save each file to the folder created above
walk2(
  wf_id$id,
  wf_id$name,
  \(x, y) drive_download(
    as_id(x),
    path = file.path("data/wf/", y),
    overwrite = TRUE
  )
)

# Load files as raster collection
wf_rasters <- map(list.files(output_dir, full.names = TRUE), rast)

# Flatten all rasters into one with multiple bands and set names to month abbr
wf <- rast(wf_rasters)
names(wf) <- paste0(month.abb)

# Plot the rasters
panel(
  wf,
  axes = FALSE,
  col = colorspace::sequential_hcl(
    n = 50,
    h = c(300, 200),
    c = c(60, NA, 0),
    l = c(25, 95),
    power = c(0.7, 1.3),
    rev = TRUE
  )
)

# Summary
summary(wf)

# Histogram
par(
  mfrow = c(3, 4),
  mar = c(2, 1.5, 1.5, 1.5)
)
hist(wf,
  col = "#6D3285", family = "serif",
  xlab = NULL, ylab = NULL
)
