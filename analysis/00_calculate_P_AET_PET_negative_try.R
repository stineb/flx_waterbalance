
install.packages('cwd')
library(cwd)


# function: from latent energy calculate water mass flow
#lenergy_to_evapotrans <- function(lat_energy, temperature, atmo_pressure){
#  1000 * 60 * 60 * 24 * lat_energy / (cwd::calc_enthalpy_vap(temperature) * cwd::calc_density_h2o(temperature, atmo_pressure))
#}

#function: from latent energy calculate actual evapotransipration / water mass flow
lenergy_to_evapotrans <- function(lat_energy, temperature, precip) {
  # define constants
  lenergy_of_vap <- 2.501 - (0.002361 * temperature)  # Latent heat of vaporization (MJ/kg)
  lenergy_of_vap <- lenergy_of_vap * 10^6  # Convert MJ/kg to J/kg

  # Convert LE (W/m²) to ET (mm/day)
  AET <- (lat_energy / lenergy_of_vap) * (60 * 60 * 24)  # Convert from W/m² to mm/day
  return(AET)
}

# Get mean annual for further use in Budyko:
site_annual_mean <- site_df |>
    # convert latent heat flux into mass flux in mm day-1
    #lenergy_in_mm = lenergy_to_evapotrans(LE_F_MDS, TA_F_MDS, PA_F), #LE.. = latent heatflow for ET / TA..=Airtemp / PA..=Airpressure

  mutate(
    lenergy_in_mm = lenergy_to_evapotrans(LE_F_MDS, TA_F_MDS, PA_F)
    ) |>
  mutate(
    PET_penman <- ET.PenmanMonteith(
      net_radiation = NETRAD,   # in W/m²
      temp = TA_F_MDS,          # Air temperature
      pressure = PA_F / 1000    # convert to kPa
    )
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  group_by(sitename, year) |>
  summarise(
    P = sum(P_F),
    AET = sum(lenergy_in_mm),
    PET = sum(PET_penman)
  ) |>
  ungroup() |>
  group_by(sitename) |>
  summarise(
    P = mean(P, na.rm = TRUE),
    AET = mean(AET, na.rm = TRUE),
    PET = mean(PET, na.rm = TRUE)
  )

---------------------------
# As CWD cannot be installed, with given data about NETRAD, temperature, and pressure
# I chose the Penman-Montheit Model to calculate PET

  install.packages('Evapotranspiration') # additionally loaded zoo package
  library(Evapotranspiration)


PET_penman <- ET.PenmanMonteith(
  net_radiation = NETRAD,   # W/m²
  temp = TA_F_MDS,          # Air temperature (°C)
  pressure = PA_F / 1000    # Convert PA_F from Pa to kPa
)

mytest = 0.5

lenergy_to_evapotrans <- function(lat_energy, temperature, precip) {
  # define constants
  lenergy_of_vap <- 2.501 - (0.002361 * temperature)  # Latent heat of vaporization (MJ/kg)
  lenergy_of_vap <- lenergy_of_vap * 10^6  # Convert MJ/kg to J/kg

  # Convert LE (W/m²) to ET (mm/day)
  AET <- (lat_energy / lenergy_of_vap) * (60 * 60 * 24)  # Convert from W/m² to mm/day
  return(AET)
}


  # Add PET to site_annual_mean
site_annual_mean <- site_df |>
  mutate(
    lenergy_in_mm = lenergy_to_evapotrans(LE_F_MDS, TA_F_MDS, PA_F),
    PET = PET_penman
  )





------------------------

  site_annual_mean <- site_df |>
  mutate(
    # Convert latent heat flux into mass flux in mm day-1
    lenergy_in_mm = lenergy_to_evapotrans(LE_F_MDS, TA_F_MDS, PA_F)
  ) |>
  rowwise() |>  # Ensures the function is applied per row/site
  mutate(
    PET_penman = ET.PenmanMonteith(
      data = data.frame(
        net_radiation = NETRAD,   # W/m²
        temp = TA_F_MDS,          # Air temperature (°C)
        pressure = PA_F / 1000    # Convert PA_F from Pa to kPa
      ),
      constants = list()  # You may need to provide Penman-Monteith constants
    )$ET.Daily  # Extract daily ET values
  ) |>
  ungroup() |>
  mutate(year = year(TIMESTAMP)) |>
  group_by(sitename, year) |>
  summarise(
    P = sum(P_F),
    AET = sum(lenergy_in_mm),
    PET = sum(PET_penman, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(sitename) |>
  summarise(
    P = mean(P, na.rm = TRUE),
    AET = mean(AET, na.rm = TRUE),
    PET = mean(PET, na.rm = TRUE)
  )
#plot(site_annual_mean)

