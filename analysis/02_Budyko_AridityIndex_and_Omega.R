
library(dplyr)

# Funktion zur Berechnung von ω für
solve_omega <- function(aet, p, pet) { # ω
  fun_omega <- function(omega) {
    (1 + (pet / p)^omega)^(-1 / omega) - (aet / p) # innere Funktion zur U msetzung der Gleichung
  }

  # Finde die Nullstelle im Bereich [0.01, 10]
  omega_value <- tryCatch(
    uniroot(fun_omega, interval = c(0.01, 10))$root,
    error = function(e) NA  # Falls uniroot fehlschlägt, gib NA zurück
  )

  return(omega_value)
}

#estimate how much much P will be lost as actual ET
#compute predicted ε0 (modelled / theoretical Index)
compute_evap_index <- function(pet, p, omega) {
  (1 + (pet / p)^omega)^(-1 / omega) # eg. ε0 (predicted EvapoINdex)

}

test = 3
# Berechnung von ω für jede Zeile der Tabelle
table_budyko <- site_annual_mean |>
  mutate(
    omega = mapply(solve_omega, AET_mean, P_mean, PET_mean), #
    Evap_Index_predicted = mapply(compute_evap_index, PET_mean, P_mean, omega), #predict of ε0
    Evap_Index_observed = AET_mean / P_mean,
    epsilon_deviation = Evap_Index_observed - Evap_Index_predicted
    ) |>
  left_join(
    table_merged |> select(sitename, cti),  # Only add CTI and Elevation
    by = "sitename"
  )

head(table_budyko)

# what site characteristics are statistically associated with omega.


 # I want to figure out, how CTI and Omega are correlating

