


# Funktion zur Berechnung von ω
solve_omega <- function(aet, p, pet) {
  #if (is.na(aet) | is.na(p) | is.na(pet) | p == 0) {
  #  return(NA)  # für fehlende Werte
 # }

  f <- function(omega) {
    (1 + (pet / p)^omega)^(-1 / omega) - (aet / p)
  }

  # Finde die Nullstelle im Bereich [0.01, 10]
  omega_value <- tryCatch(
    uniroot(f, interval = c(0.01, 10))$root,
    error = function(e) NA  # Falls uniroot fehlschlägt, gib NA zurück
  )

  return(omega_value)
}

budyko_fun <- function(pet, p, omega) {
  (1 + (pet / p)^omega)^(-1 / omega)
}

# Berechnung von ω für jede Zeile der Tabelle
table_budyko <- site_annual_mean |>
  mutate(
    omega = mapply(solve_omega, AET_mean, P_mean, PET_mean)
  )|>
  mutate(
    AET_over_P = mapply(budyko_fun, PET_mean, P_mean, omega),
  ) |>
  left_join(
    table_merged |> select(sitename, cti),  # Only add CTI and Elevation
    by = "sitename"
  )
test = 1

# what site characteristics are statistically associated with omega.


 # I want to figure out, how CTI and Omega are correlating

