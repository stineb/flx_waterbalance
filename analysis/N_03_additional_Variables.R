



N_merged_characteristics_df <- N_budyko_omega_df |>
  dplyr::select(sitename, epsilon_deviation) |>
  left_join(
    N_site_df |> select(sitename, canopy_height, igbp_land_use, whc, elv),
    by="sitename"
  ) |>
  left_join(
    table_merged |> select(sitename, cti),  # Only add CTI and Elevation
    by = "sitename"
  ) |>
  left_join(
    N_annual_means_df |> select(sitename, prec, pet)
)




#--------------############PLOTS###################-------------

library(dplyr)
library(ggplot2)
#-------------------ELEVATION-----------###

### Elevation and ε′----------###
#Scatterplot
ggplot(N_merged_characteristics_df, aes(x = elv, y = epsilon_deviation)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Elevation vs. ε′",
       x = "Elevation (m)",
       y = "ε′ (Deviation from Budyko Curve)") +
  theme_minimal()


#-------------------LAND COVER------####

### IGBP and ε′
ggplot(N_merged_characteristics_df, aes(x = igbp_land_use, y = epsilon_deviation)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Landcover vs. ε′",
       x = "Landcover Types",
       y = "ε′ (Deviation from Budyko Curve)") +
  theme_minimal()


### Heatmap with CTI, Landcover and ε′
# ggplot(N_merged_characteristics_df, aes(x = cti, y = as.factor(igbp_land_use), fill = epsilon_deviation)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#   labs(title = "Heatmap: CTI, Land Use & ε′",
#        x = "CTI",
#        y = "Land Cover",
#        fill = "ε′ (Deviation)") +
#   theme_minimal()




### Heatmap with CTI, Landcover and ε′


library(dplyr)
library(ggplot2)

# quantile-basierte Schwellenwerte
N_cti_breaks <- c(2.285880, 4.197392, 4.942680, 5.849463, 6.893455, 14.612223)
N_cti_labels <- c("very low", "low", "middle", "high", "very high")

# CTI klassifizieren
N_merged_characteristics_df <- N_merged_characteristics_df |>
  mutate(cti_class = cut(cti,
                         breaks = cti_breaks,
                         labels = cti_labels,
                         include.lowest = TRUE,
                         include.highest = TRUE))

# Durchschnittliche ε′-Abweichung pro CTI-Klasse & Landnutzung berechnen
N_heatmap_data_cti <- N_merged_characteristics_df |>
  group_by(cti_class, igbp_land_use) |>
  summarise(mean_epsilon = mean(epsilon_deviation, na.rm = TRUE), .groups = "drop")

# Heatmap zeichnen
ggplot(N_heatmap_data_cti, aes(x = cti_class, y = igbp_land_use, fill = mean_epsilon)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "darkslategrey",
    mid = "beige",
    high = "hotpink4",
    midpoint = 0,
    limits = c(-0.9, 0.9),
    name = expression(epsilon*"′ (Mean Deviation)")) +
  labs(title = "Heatmap: CTI classes, Land Cover Types and ε′",
       x = "CTI Classes",
       y = "Land Cover") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Heatmap Land Cover Types



N_merged_characteristics_df |>
  filter(igbp_land_use == "GRA") |>
  ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
  geom_point(shape = 21, size = 4, color = "black") +  # schöne Punkte mit Outline
  scale_fill_gradient2(
    low = "darkslategrey", mid = "beige", high = "hotpink4",
    midpoint = 0,
    limits = c(-0.9, 0.9),  # Skala aus deinen Daten
    name = expression(epsilon*"′ (Deviation)")
  ) +
  labs(
    title = "ε′ across CTI and Aridity Indexfor GRASSLAND",
    x = "CTI",
    y = "Aridity Index (PET / P)"
  ) +
  theme_minimal()





