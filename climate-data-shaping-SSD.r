
### Preparatory steps

#...................................      
## Starting setup

# Clean up from previous code / runs
rm(list=ls(all=TRUE) )

# Set font
# windowsFonts(Arial=windowsFont("Arial"))

# Set working directory to where this file is stored
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

#...................................      
## Install or load required R packages

# List of required packages
x1 <- c("stringr", "sf","RColorBrewer", "raster", "dplyr", "csv", "openxlsx", "purrr", "testthat", "microbenchmark")
# Install any packages not ye1t installed
x2 <- x1 %in% row.names(installed.packages())
if (any(x2 == FALSE)) { install.packages(x1[! x2]) }

# May need to follow installation instructions in the terminal
if (!"rgdal" %in% installed.packages()) install.packages("rgdal")
if (!"psych" %in% installed.packages()) install.packages("psych")

# No longer necessary but leaving for now for reference (you can remove)
# if(!"mapview" %in% installed.packages()){
#   # Install mapview dependencies
#   # May need to follow installation instructions in the terminal, ie for "units" package
#   if (!"crosstalk" %in% installed.packages()) install.packages("crosstalk")
#   # Units github: https://github.com/r-quantities/units
#   if (!"units" %in% installed.packages()) install.packages("units")
#   # SF instructions: https://r-spatial.github.io/sf/
#   if (!"sf" %in% installed.packages()) install.packages("sf")
# 
#   remotes::install_github("r-spatial/mapview")
# }

# Load all packages    
lapply(c(x1, "rgdal", "sf"), library, character.only = TRUE)


# ===================
# FUNCTIONS
# ===================

load_raster <- function(file_name){
  # need to set the correct directory for this to work
  file_name <- file_name
  climate <- raster::raster(file_name)
  climate
}

create_cropped_raster <- function(pop_raster, filtered_geo_shapefile) {
  cropped_pop_raster <- raster::crop(pop_raster, filtered_geo_shapefile)
  cropped_pop_raster <- raster::mask(cropped_pop_raster, filtered_geo_shapefile)
  return(cropped_pop_raster)
}

add_mean_to_shapefile <- function(pop_raster, geo_area, year, month) {
  clim <- raster::extract(pop_raster, geo_area, fun = mean, na.rm = TRUE, cellnumbers = T)
  geo_area$mean_spi <- clim
  geo_area$mean_spi <- as.numeric(geo_area$mean_spi)
  geo_area$year <- year
  geo_area$month <- month
  
  return(geo_area)
}


process_shapefile  <- function(filename){
  climate <- load_raster(filename)
  cropped_climate <- create_cropped_raster(climate, aggshape)
  year = str_sub(filename, 12,15)
  month = as.numeric(str_sub(filename, 17,18))
  climate_agg <- add_mean_to_shapefile(cropped_climate, aggshape, year, month)
  return(climate_agg)
}

shapefile_to_df <- function(shapef){
  df <- data.frame(shapef$year, shapef$month, shapef$ADM1_EN, shapef$ADM2_EN, shapef$mean_spi)
  return(df)
}

# =================
# SETUP
# ================

# SOM - ADMIN LEVEL 2 - DISTRICT

#area_file <- "C:/Users/Sev/Documents/GAM_SAM prediction/South Sudan/Data/Admin2_Shapefile/ssd_admbnda_adm2_Abyei_imwg_nbs_20180817.shp"
# area_file <- "SS_Admin2_2011.shp"
# area_file  <- "ssd_admbnda_imwg_nbs_shp/ssd_.shp

grep("\\.shp", list.files("ssd_admbnda_adm2_imwg_nbs_20180817/"), value = T)

area_file <- file.path("ssd_admbnda_adm2_imwg_nbs_20180817/ssd_admbnda_adm2_imwg_nbs_20180817.shp")
aggshape <- sf::read_sf(area_file)


# ===================
# RUN
# ===================

# CREATE CSV
# raw_data_dir <- "J:/Mortality_estimation_crises/Nigeria/New Data Structure/Predictor data/As received/not_cleaned/Rainfall 2017-2018/"
raw_data_dir <- "C:/Users/sever/OneDrive/Documents/GAM_SAM prediction/South Sudan/Data/Climate/Ssd_Climate Engine/"
# setwd(raw_data_dir)
files <- list.files()
file_names <- files %>% str_subset(".tif")

# aggregate rasterized spi by shape for all files (returns a list of shapefiles)
agg_shapes <- lapply(file_names, process_shapefile)
# convert shapefils to df
clim_dfs <- lapply(agg_shapes, shapefile_to_df)
# combine all dfs
combined_df <-  do.call("rbind", clim_dfs)


# format combined dfs for csv output
names(combined_df)
names(combined_df) <- c("y", "m", "state", "county", "mean_spi")
names(combined_df)

head(combined_df)
combined_df <- select(combined_df, c("y", "m", "state", "county", "mean_spi"))
combined_df <- as.data.frame(combined_df)

# Save Csv
filename <- paste0(raw_data_dir,  "/SPI_Chirps_2014-2018_Standardized_From_1981_2019.csv")
write.csv(combined_df, filename)

dir.create("outputs", showWarnings = F)
write.csv(combined_df, file.path("outputs", "SPI_Chirps_2014-2018_Standardized_From_1981_2019.csv"))



