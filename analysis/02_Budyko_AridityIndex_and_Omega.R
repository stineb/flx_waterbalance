
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


#CORRELATION----------------------
# what site characteristics are statistically associated with omega.
# I want to figure out, how CTI and deviation are correlating
library(dplyr)
clean_budyko <- table_budyko |>
  filter(!is.na(cti) & !is.na(epsilon_deviation))

cor(clean_budyko$cti,clean_budyko$epsilon_deviation, method = 'pearson')
plot(clean_budyko$cti, clean_budyko$epsilon_deviation, main = 'Korrelation')
# results with -0.02786773 is very low and negatively significantly under the threshold
# correlation is therefore given
# threshold was set to 0.05



#REGRESSION----------------------
regression_cti <- clean_budyko |>
  lm(epsilon_deviation ~ cti)

head(lm_model)



#plot the relations between cti and epsilon deviations
library(ggplot2)
install.packages("labeling")
install.packages('farver')

gg_deviation_per_site <- table_budyko |>
  ggplot(
    na.omit(table_budyko), aes(x = sitename, y = epsilon_deviation)) +
  geom_boxplot() +
  labs(title = "Deviation from Budyko (ε') at different sites",
       x = "Sites",
       y = "Deviations ε' ") +
  #theme_minimal()

ggsave(here::here("~/flx_waterbalance/data/epsilon_deviation_budyko.png"), width = 8, height = 5)



ggplot(na.omit(table_budyko), aes(x = cti, y = epsilon_deviation)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between CTI and ε'",
       x = "Compound Topographic Index (CTI)",
       y = "Deviation from Budyko Curve (ε')") +
  theme_minimal()
  ggsave(here::here("~/flx_waterbalance/data/cti_budyko_relation_regression.png"), width = 6, height = 3.5)





