
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

#CORRELATION----------------------
# what site characteristics are statistically associated with omega.
# I want to figure out, how CTI and deviation are correlating


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


#### Boxplots -------------------------------------------
# CTI in 3 groups (low, midddle, high)
table_budyko$cti_class <- cut(table_budyko$cti,
                              breaks = quantile(table_budyko$cti, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                              labels = c("low", "middle", "high"),
                              include.lowest = TRUE)

# Boxplot with cti
ggplot(table_budyko, aes(x = cti_class, y = epsilon_deviation, fill = cti_class)) +
  geom_boxplot(outlier.color = "darkred") +
  labs(title = "Boxplot: ε′ in CTI-classes",
       x = "CTI-Class",
       y = "ε′ (deviation from Budyko)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave(here::here("~/flx_waterbalance/data/epsilon_prime_cti_boxplot.png"))

#Boxplot with landuse



##### Heatmap -----------------------------------------

cti_breaks <- c(2.285880, 4.197392, 4.942680, 5.849463, 6.893455, 14.612223) # quantiles seq(0,1,0.2)
cti_labels <- c("very low", "low", "middle", "high", "verx high")

table_budyko <- table_budyko |>
  mutate(cti_class = cut(
    cti,
    breaks = cti_breaks,
    labels = cti_labels,
    include.lowest = TRUE,
    include.highest = TRUE
  ))

# Mittelwert von ε′ je Kombination (Landnutzung x CTI-Klasse)
heatmap_data <- table_budyko |>
  group_by(cti_class, igbp_land_use) |>
  summarise(mean_epsilon = mean(epsilon_deviation, na.rm = TRUE), .groups = "drop")

ggplot(heatmap_data,
       aes(x = cti_class, y = igbp_land_use, fill = mean_epsilon)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "royalblue",
    mid = "darkseagreen",
    high = "purple",
    midpoint = 0,
    name = expression(epsilon*"′ (Mean Deviation)")) +
  labs(
    title = "Heatmap: CTI classes, Land Use and ε′",
       x = "CTI Class",
       y = "Land Use",
       fill = "ε′ (Mean Deviation)") +
  theme_minimal()
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
)
ggsave(here::here("~/flx_waterbalance/data/cti_landuse_heatmap.png"),
       width = 9,
       height = 6,
       dpi = 300)



#Color MApping-----------------------

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
  slice_max(epsilon_deviation, n=10)


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
