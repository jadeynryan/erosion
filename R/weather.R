# Attach packages
library(ncdf4)
library(purrr)
library(tidyr)
library(dplyr)
library(terra)

# List all NetCDF files from the same year.
file_list <- list.files("./data/gldas",
                        pattern = "^.*nc",
                        full.names = TRUE)

# Create a df of files and isolate date and time
file_df <- data.frame(cbind(file_list, file_list)) |>
  rename(file_name = file_list, copy = file_list.1) |>
  separate_wider_position(cols = copy,
                          widths = c(
                            31,
                            year = 4,
                            month = 2,
                            day = 2,
                            1,
                            time = 4,
                            16
                          ))

# Split list so each month has its own tibble
files_by_month <- split(file_df, file_df$month)

# Select only the file name and convert to list
files_by_month <- files_by_month |>
  map(\(month) as.list(month$file_name))

# This function combines all functions needed to extract from a single
# nc file for use with purrr::map() then list_bindr().
process_nc <- function(nc_file) {
  # Open nc
  nc <- nc_open(nc_file)
  
  # Get coordinates and time
  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  
  time_m <- ncvar_get(nc, "time")
  
  # Convert time to human-readable format
  time <- as.POSIXct((time_m * 60), # Convert to seconds
                     origin = "2000-01-01 03:00:00",
                     tz = "UTC") |>
    format(format = "%Y/%m/%d %H:%M:%S")
  
  # Get variable names
  var_names <- attributes(nc$var)$names
  
  # Remove time_bnds
  var_names <- var_names[!var_names == "time_bnds"]
  
  # Get variable attributes
  var_attr <- set_names(var_names) |>
    map(\(var_name) {
      ncatt_get(nc, var_name) |>
        as.data.frame()
    }) |>
    list_rbind()
  
  # Create a function to get variable data then map to each var
  get_var <- function(nc, variable) {
    data <- ncvar_get(nc, variable)
    fill_value <- ncatt_get(nc, variable, "_FillValue")
    # Replace fill values with NA
    data[data == fill_value$value] <- NA
    return(data)
  }
  
  nc_array <- set_names(var_names) |>
    map(\(var_name) get_var(nc, var_name))
  
  # Build dataframe
  
  # Create matrix of long, lat, and time
  lon_lat_time <- as.data.frame(expand.grid(lon, lat, time))
  
  # Reshape variables into long vector
  nc_vec <- set_names(var_names) |>
    map(\(var_name) as.vector(nc_array[[var_name]])) |>
    as.data.frame()
  
  # Create dataframe
  nc_df <- cbind(lon_lat_time, nc_vec) |>
    rename("Lon" = "Var1",
           "Lat" = "Var2",
           "DateTime" = "Var3")
  
  # Check the dataframe
  return(nc_df)
}

# Do this month by month or RStudio will get hung up
jan_nc <- map(files_by_month[1],
              \(month) {
                map(month, \(file) process_nc(file))
              },
              .progress = TRUE) |>
  bind_rows()