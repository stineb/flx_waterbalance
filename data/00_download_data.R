

library(readr)
library(dplyr)
library(tidyverse)
library(terra)
install.packages("remotes")
library(remotes)
remotes::install_github("https://github.com/geco-bern/FluxDataKit")
library(FluxDataKit)

flx_fullyearsequence <- read_csv("data/fdk_site_fullyearsequence.csv")

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


#involve condensation:


# create maintable and add cti column
# whc= water holding capacity / mat= mean annual temp / elv = elevation
table_merged <- flx_all_data |>
  dplyr::select(sitename, year_start, year_end, lon, lat, elv, canopy_height, whc, mat, p_over_pet) |>
  dplyr::mutate(cti = extracted_cti[[2]]) |>
  left_join(
    flx_fullyearsequence, by="sitename"
  ) |>
#  left_join(
#    flx_all_data|>
#      dplyr::select(sitename, igbp_land_use, whc, mat, p_over_pet),# get additionals from meta-data per site
#    by = "sitename") |>
#  filter(!drop_lecorr) |>  # where no full year sequence was found
#  filter(nyears_lecorr >= 3)



#-------------------------------------------------------------------------------------
# get MAT and PAT from meta-data from sites ->>> CAUTION: nweicht von MAT aus fdk.site-info ab!!!


# sammeln der Daten aus /data_2/FluxDataKit/v3.4/zenodo_upload/fluxnet -> aller FLUXDATAKIT_FULLSET_DD* Dateien
# ZIEL: P. AET. PET per site, um an Budyko anwenden zu können


path <- "/data_2/FluxDataKit/v3.4/zenodo_upload/fluxnet/"
read_onesite <- function(site, path){
  filename <- list.files(path = path,
                         pattern = paste0("FLX_", site, "_FLUXDATAKIT_FULLSET_DD"),
                         full.names = TRUE
  )
  out <- read_csv(filename) |>
    mutate(sitename = site)
  return(out)
}

#read daily data

#install.packages('lubridate')
#library(lubridate)

site_df <- purrr::map_dfr(
  table_merged$sitename,
  ~read_onesite(., path)
)
site_df <- site_df |>
  left_join(
    table_merged |>
      select(
        sitename,
        year_start = year_start_lecorr,
        year_end = year_end_lecorr),
    by = join_by(sitename)
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  filter(year >= year_start & year <= year_end) |>
  select(-year_start, -year_end, -year)

site_df <- dplyr::bind_rows(site_df)
#site_df <- as.data.frame(site_df) # convert from list to dataframe! otherwise leftjoin below will not work
#




