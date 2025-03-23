

install.packages('remotes')
library(remotes)

install_github("geco-bern/cwd")
library(cwd)


# function: from latent energy calculate water mass flow
lenergy_to_evapotrans <- function(lat_energy, temperature, atmo_pressure){
  1000 * 60 * 60 * 24 * lat_energy / (cwd::calc_enthalpy_vap(temperature) * cwd::calc_density_h2o(temperature, atmo_pressure))
}


site_annual_mean <- site_df |>
  mutate(
    # Convert latent heat flux (LE) into evapotranspiration (ET) in mm/day
    lenergy_in_mm = lenergy_to_evapotrans(LE_F_MDS, TA_F_MDS, PA_F),
    # Compute Potential Evapotranspiration (PET) using the cwd::pet function
    # pet converts PET from mm per second to mm per day
    PET_day = 60 * 60 * 24 * cwd::pet(NETRAD, TA_F_MDS, PA_F)
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  group_by(sitename, year) |>
  summarise(
    P_day = sum(P_F),
    AET_day = sum(lenergy_in_mm),
    PET_day = sum(PET_day)
  ) |>
  ungroup() |>
  group_by(sitename) |>
  summarise(
    P_mean = mean(P_day, na.rm = TRUE),
    AET_mean = mean(AET_day, na.rm = TRUE),
    PET_mean = mean(PET_day, na.rm = TRUE)
  )

plot(site_annual_mean)
