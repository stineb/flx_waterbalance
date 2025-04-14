
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
#calculate predicted ε0 (modelled / theoretical Index)
compute_evap_index <- function(pet, p, omega) {
  (1 + (pet / p)^omega)^(-1 / omega) # eg. ε0 (predicted EvapoINdex)

}


# Berechnung von ω für jede Zeile der Tabelle and hinzufügen von zusätzlichen Faktoren
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
  ) |>
  left_join(
    table_merged |> select(sitename, mat),
    by="sitename"
  ) |>
  left_join(
    flx_all_data |> select(sitename, igbp_land_use),
    by="sitename"
  ) |>
  left_join(
    flx_all_data |> select(sitename, canopy_height),
    by="sitename"
  )

head(table_budyko)

library(dplyr)
clean_budyko <- table_budyko |>
  filter(!is.na(cti) & !is.na(epsilon_deviation))


#Color MApping  with positive ε′ Deviation per Site -----------------------

ggplot(clean_budyko, aes(x = PET_mean / P_mean, y = AET_mean / P_mean, color = epsilon_deviation)) +
  geom_point(size = 2) +
  scale_color_gradient2(
    low = "blue4",
    mid = "white",
    high = "firebrick",
    midpoint = 0) +
  labs(
    title = "Budyko Diagram with ε′ Deviation",
      x = "PET / P (Dryness Index)",
      y = "AET / P (Evaporation Index)",
      color = "ε′ (Deviation)",
  theme_minial()
      #ylim = c(0,4),
      #xlim = c(0,10)
)


#Color Mapping and Scatterplot----------------------------
install.packages('ggrepel')
library(ggrepel)


positive_epsilon <- clean_budyko |>
  arrange(desc(epsilon_deviation)) |>
  slice_max(epsilon_deviation, n=10)#chose top ten locations


ggplot(clean_budyko, aes(x=PET_mean / P_mean, y=AET_mean / P_mean)) +
  geom_point(aes(color = epsilon_deviation), size=3) +
  #geom_density_2d(color ="black") +
  scale_color_gradient2(low="blue4", mid="white", high="firebrick", midpoint=0) +
  labs(title="Budyko with positive ε′ sites (top ten)",
       x="PET / P",
       y="AET / P",
       color="ε′ (Deviation)") +
  geom_text_repel(data=positive_epsilon,
            aes(label=sitename),
            size=3, color="black") +
  theme_minimal()
ggsave(here::here("~/flx_waterbalance/data/positive_sites_deviating_budyko.png"),
       width = 9,
       height = 6,
       dpi = 300)
