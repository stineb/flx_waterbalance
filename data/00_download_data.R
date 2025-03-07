

library(readr)
library(dplyr)
library(tidyverse)
library(terra)
install.packages("remotes")
library(remotes)
remotes::install_github("https://github.com/geco-bern/FluxDataKit")
library(FluxDataKit)

flx_fullyearsequence <- read_csv("data/fdk_site_fullyearsequence.csv")
View(flx_fullyearsequence)

#load sitenames, lon, lat, height and p_over_pet:
flx_all_data <- read_csv("data/fdk_site_info.csv")

#Marthews netCDF data as a raster:
cti_all_data <- rast("/data/archive/gti_marthews_2015/data/ga2.nc")

# extract needed Value from cti_all_data
extracted_cti <- terra::extract(
  cti_all_data,
  flx_all_data |> dplyr::select(lon, lat)
)
print(head(extracted_cti))

# create maintable and add cti column
table_merged <- flx_all_data |>
  dplyr::select(sitename, lon, lat, elv, canopy_height, whc, mat, p_over_pet) |>
  dplyr::mutate(cti = extracted_cti[[2]])


