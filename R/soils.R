## This script gets soils data from gSSURGO with the soilDB package.

## Currently, the script is set up for a function to loop through each
## county within the Columbia Plateau.

## To run interactively and see outputs, go through and uncomment the
## code lines that summarize, map, and plot data. There are comments
## above these lines of code with a trail of ****** to make them
## easier to find.

## Jadey Ryan | July 1, 2023

## Attach packages and data ---------------------------

# Load packages
library(dplyr)
library(purrr)
library(skimr)
library(tictoc)

library(soilDB) # followed examples in http://ncss-tech.github.io/AQP/soilDB/WCS-demonstration-01.html and http://ncss-tech.github.io/soilDB/articles/wcs-ssurgo.html#thematic-mapping-1

library(sf)
library(terra)

# Load shapefiles
cp <- st_read("data/columbia_plateau_shp") |>
  st_transform(5070)

cp_county <- st_read("data/columbia_plateau_county_clip_shp") |>
  filter(!COUNTY_NM == "Grant") |>
  st_transform(5070)

# Must do Grant county separately since its extent has too many
# pixels for the SDA query
grant <- st_read("data/grant_2part_shp") |>
  st_transform(5070)

## Create function to loop through counties of interest ------------

get_soils <- function(sf, aoi) {
  # Error if aoi not found
  if (!aoi %in% sf$COUNTY_NM) {
    stop(paste0("Could not find '", aoi, "'."))
  }

  message("Starting ", aoi)
  # Start timer
  tic()

  # Filter to county of interest
  aoi_spatial <- sf |>
    filter(COUNTY_NM %in% aoi)

  ## Get soil properties -----------------------------------

  message("Querying WCS for map units")
  mu <- mukey.wcs(aoi_spatial, db = "gSSURGO")

  # Show map units *************************************
  # plot(mu,
  #   legend = FALSE,
  #   axes = FALSE,
  #   main = attr(mu, "layer name")
  # )

  message("\nGetting raster attribute table")
  rat <- cats(mu)[[1]]

  message("Getting soil properties from the SDA web-service")

  # Horizon level variables needed (using representative values)
  h_vars <- c(
    "sandtotal_r", # total sand (%)
    "silttotal_r", # total silt (%)
    "claytotal_r", # total clay (%)
    "caco3_r", # calcium carbonate (%)
    "om_r" # organic matter (%)
  )

  # Get variables using dominant component depth-weighted average
  h_tbl <- get_SDA_property(
    property = h_vars,
    method = "Dominant Component (Numeric)",
    mukeys = rat$mukey,
    top_depth = 0,
    bottom_depth = 30
  )

  # Get broad overview of data ***********************************
  # skim(h_tbl)

  # Component level variables needed
  c_vars <- c(
    "weg", # wind erodibility group
    "wei" # wind erodibility index (tons/ac/yr)
  )

  # Get variables using dominant component average
  c_tbl <- get_SDA_property(
    property = c_vars,
    method = "Dominant Component (Category)",
    mukeys = rat$mukey,
    top_depth = 0,
    bottom_depth = 30
  )

  # Get broad overview of data ********************************
  # skim(c_tbl)

  # Combine component and horizon properties
  soils_tbl <- left_join(h_tbl, c_tbl,
    by = join_by(
      areasymbol,
      musym,
      muname,
      mukey
    )
  )

  message("Calculating EF and SCF")

  # Calculate EF ---------------------------------------------
  calculate_ef <- function(sand, silt, clay, om, caco3) {
    ssc_f <- (0.31 * sand) + (0.17 * silt) + (0.33 * (sand / clay))
    om_f <- 2.59 * om
    caco3_f <- (0.95 * caco3)

    return((29.09 + ssc_f - om_f - caco3_f) / 100)
  }

  soils_tbl <- soils_tbl |>
    mutate(ef = calculate_ef(
      sand = sandtotal_r,
      silt = silttotal_r,
      clay = claytotal_r,
      om = om_r,
      caco3 = caco3_r
    ))

  # Calculate SCF ------------------------------------------------
  calculate_scf <- function(clay, om) {
    (1 / (1 + 0.0066 * (clay)^2 + 0.021 * (om)^2))
  }

  soils_tbl <- soils_tbl |>
    mutate(scf = calculate_scf(claytotal_r, om_r))

  # Get broad overview of data ***********************************
  # skim(soils_tbl)

  message("Mapping soil properties and calculated factors")

  # Set raster categories
  vars <- c(h_vars, c_vars, "ef", "scf")

  # Convert mapunit grid + RAT to stack of property grids
  levels(mu) <- soils_tbl[, c("mukey", vars)]

  # Convert mukey grid + RAT to stack of numerical grids
  soils <- catalyze(mu)

  # Mask to only aoi
  soils <- mask(soils, vect(aoi_spatial))

  # View maps ***************************************************
  # plot(soils,
  #   main = paste0(
  #     names(soils),
  #     " (0-30 cm)\nDominant Component"
  #   ),
  #   axes = FALSE
  # )

  # View correlation plot
  # soils_numeric_vars <- soils_tbl |>
  #   dplyr::select(where(is.numeric), -mukey)
  #
  # cor(soils_numeric_vars, use = "complete.obs") |>
  #   corrplot(type = "upper")

  message("Writing raster to output folder")
  aoi_no_space <- sub(" ", "", aoi)
  writeRaster(soils, paste0("output/", aoi_no_space, ".tif"),
    overwrite = TRUE
  )

  # End timer
  message("Finished ", aoi)
  toc()
}

## Map function to each county in cp_county ----------------
counties <- cp_county$COUNTY_NM
tic()
walk(counties,
  \(county) get_soils(cp_county, county),
  .progress = TRUE
)

# Map function to two part Grant
grants <- grant$COUNTY_NM
walk(grants,
  \(county) get_soils(grant, county),
  .progress = TRUE
)
toc()
# Combine two Grant county rasters into SpatRasterCollection
grant1 <- rast("output/Grant1.tif")
grant2 <- rast("output/Grant2.tif")
grant <- sprc(grant1, grant2) |>
  terra::merge()

# Write raster
writeRaster(grant, paste0("output/Grant.tif"),
  overwrite = TRUE
)

# Remove temp split Grant rasters
file.remove(c("output/Grant1.tif", "output/Grant2.tif"))

## Combine all counties into one raster ---------------------------
## I can't get this to work. I think the combined raster is too
## large so it crashes R.

# raster_files <- list.files("./output/", full.names = TRUE)
# all_rasters <- lapply(raster_files, rast)
# cp_soils <- sprc(all_rasters) |>
#   terra::merge()
#
# writeRaster(cp_soils, paste0("output/ColumbiaPlateauSoils.tif"),
#             overwrite = TRUE
# )

