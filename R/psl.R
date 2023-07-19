library(googledrive)
library(dplyr)
library(tidyr)
library(parzer)
library(ggplot2)
library(ggthemes)
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
psl_monthly <- drive_get("erosion/psl_monthly_2018.tif")
drive_download(as_id(psl_monthly$id),
  file.path("data/psl/", psl_monthly$name),
  overwrite = TRUE
)
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
monthly_summary <- summary(psl_monthly) |>
  as.data.frame() |>
  separate_wider_delim(Freq, delim = ":", names = c("stat", "value")) |>
  mutate(
    stat = trimws(stat),
    value = as.numeric(value)
  ) |>
  subset(stat %in% c("Mean", "Median")) |>
  select(month = Var2, stat, value)

ggplot(monthly_summary,
  mapping = aes(month,
    value,
    group = stat,
    col = stat
  )
) +
  geom_point() +
  geom_line() +
  labs(
    y = bquote("Potential Soil Loss "(kg / m^2)),
    x = NULL,
    col = NULL
  ) +
  scale_y_continuous(breaks = seq(0, 0.35, 0.05)) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "bottom",
    axis.ticks = element_blank(),
    plot.margin = unit(rep(0.2, 4), "in")
  )

ggsave("./output/timeseries.png",
  width = 6, height = 4, units = "in"
)

# Annual raster ----------
psl_annual <- drive_get("erosion/psl_annual_2018.tif")
drive_download(as_id(psl_annual$id),
  file.path("data/psl/", psl_annual$name),
  overwrite = TRUE
)
psl_annual <- rast(file.path("data/psl/psl_annual_2018.tif"))

# Change band names
names(psl_annual) <- "2018 Annual Sum"

# Set font family to serif (Times New Roman)
par(family = "serif")

# Plot the raster
plot(
  psl_annual,
  col = rev(colorspace::heat_hcl(50, h = c(36, 90))),
  axes = FALSE,
  type = "continuous"
)

# Get summary of pixels
summary(psl_annual)

# Data validation with Pi et al. (2020) -----------------------
Pi_weps <- read.csv("./data/Pi_et_al_2020_WEPS.csv", check.names = TRUE)

weps_clean <- Pi_weps |>
  # Convert from degree minutes to decimal degrees
  mutate(
    Lon = -parse_lon(Longitude..E.),
    Lat = parse_lat(Latitude..N.)
  ) |>
  select(Station, Lat, Lon, Soil_Loss) |>
  # New column for symbol type
  mutate(pch = ifelse(Soil_Loss > 0, 19, 1))


write.csv(weps_clean, "./data/Pi_et_al_2020_WEPS_clean.csv")

weps_pts <- vect(weps_clean,
  geom = c("Lon", "Lat"),
  crs = "epsg:4326"
)

# Add points to annual psl plot
points(weps_pts, pch = weps_pts$pch)

# Label points that are > 0
weps_pts |>
  subset(weps_pts$pch == 19) |>
  text(label = "Soil_Loss", pos = 3, halo = TRUE)

# Create comparison table
tbl <- extract(psl_annual, weps_pts) |>
  cbind(weps_clean) |>
  select(Station, Lat, Lon, Soil_Loss, Potential_Soil_Loss = swep) |>
  mutate(
    Potential_Soil_Loss = round(Potential_Soil_Loss, 1),
    Difference = Potential_Soil_Loss - Soil_Loss
  )

write.csv(tbl, "./output/comparison_table.csv")

# Histogram
par(
  mfrow = c(3, 4),
  mar = c(2, 1.5, 1.5, 1.5)
)
hist(psl_monthly, col = "#BC6200", family = "serif")
