



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



N_epsilon_min <- min(N_merged_characteristics_df$epsilon_deviation, na.rm = TRUE)
N_epsilon_max <- max(N_merged_characteristics_df$epsilon_deviation, na.rm = TRUE)

epsilon_abs_max <- max(abs(epsilon_min), abs(epsilon_max))


#--------------############PLOTS###################-------------



library(dplyr)
library(ggplot2)

#---------------------------REGRESSIONS---------------------------------------------------------------------------------------------


### Elevation and ε′----------###
#Scatterplot
ggplot(N_merged_characteristics_df, aes(x = elv, y = epsilon_deviation)) +
  geom_point(alpha = 0.9, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Elevation vs. ε′",
       x = "Elevation (m)",
       y = "ε′ (Deviation from Budyko Curve)") +
  theme_minimal()


### WHC and ε′----------###
#Scatterplot
ggplot(N_merged_characteristics_df, aes(x = whc, y = epsilon_deviation)) +
  geom_point(alpha = 0.9, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "WHC vs. ε′",
       x = "Water Holding Capacity",
       y = "ε′ (Deviation from Budyko Curve)") +
  theme_minimal()


### IGBP and ε′--------------###
# ggplot(N_merged_characteristics_df, aes(x = igbp_land_use, y = epsilon_deviation)) +
#   geom_point(alpha = 0.7, color = "darkgreen") +
#   geom_smooth(method = "lm", color = "black") +
#   labs(title = "Landcover vs. ε′",
#        x = "Landcover Types",
#        y = "ε′ (Deviation from Budyko Curve)") +
#   theme_minimal()



#---------------------------HEATMAPS---------------------------------------------------------------------------------------------



library(dplyr)
library(ggplot2)
library(ggrepel)

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
    low = "blue",
    mid = "white",
    high = "firebrick4",
    midpoint = 0,
    limits = c(-epsilon_abs_max, epsilon_abs_max),  # jetzt korrekt skaliert
    name = expression(epsilon*"′ (Mean Deviation)"),
    labels = scales::label_scientific(digits = 2),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Heatmap: CTI classes, Land Cover Types and ε′",
    x = "CTI Classes",
    y = "Land Cover"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_heatmap_landcovers.png"))


# ggplot(N_heatmap_data_cti, aes(x = cti_class, y = igbp_land_use, fill = mean_epsilon)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(
#     low = "darkslategrey",
#     mid = "beige",
#     high = "hotpink4",
#     midpoint = 0,
#     limits = c(-0.9, 0.9),
#     name = expression(epsilon*"′ (Mean Deviation)")) +
#   labs(title = "Heatmap: CTI classes, Land Cover Types and ε′",
#        x = "CTI Classes",
#        y = "Land Cover") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))



#---------------------------DIAGRAMs---------------------------------------------------------------------------------------------


install.packages("patchwork")
library(patchwork)
library(dplyr)
library(ggplot2)
library(ggrepel)


N_area_cti <- quantile(
  N_merged_characteristics_df$cti,
  probs = 2/3,
  na.rm = TRUE
)

N_area_pet_p <- quantile(
  N_merged_characteristics_df$pet / N_merged_characteristics_df$prec,
  probs = 2/3,
  na.rm = TRUE
)

N_area_epsilon <- quantile(
  N_merged_characteristics_df$epsilon_deviation[N_merged_characteristics_df$epsilon_deviation > 0],
  probs = 0.7,
  na.rm = TRUE
)

# global scaling for different landcover types in graph
N_xlim_cti <- range(c(0, N_merged_characteristics_df$cti))
N_ylim_aridity <- range(c(0, N_merged_characteristics_df$pet / N_merged_characteristics_df$prec))




###----------GRASSLAND PET/P to CTI-----------


#focus on sites with deviation higher 0.7 quintile
N_highlight_sites_grassland <- N_merged_characteristics_df |>
  filter(
    igbp_land_use == "GRA",
    epsilon_deviation > N_area_epsilon
  )

N_gg_landcover_grassland_img <- N_merged_characteristics_df |>
  filter(igbp_land_use == "GRA") |>
  ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
  geom_point(shape = 21, size = 4, color = "black") +
  scale_fill_gradient2(
    low = "darkslategrey", mid = "beige", high = "hotpink4",
    midpoint = 0,
    limits = c(-epsilon_abs_max, epsilon_abs_max),
    name = expression(epsilon*"′ (Deviation)"),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_scientific(digits = 2)
  ) +
  coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
  geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
  geom_text_repel(
    data = N_highlight_sites_grassland,
    aes(label = sitename),
    size = 4,
    color = "black",
    box.padding = 0.4,
    point.padding = 0.5,
    segment.color = "black",
    max.overlaps = Inf,
    force = 2,
    max.time = 5,
    max.iter = 2000
  ) +
  labs(
    title = "ε′ across CTI and Aridity Index for GRASSLAND",
    x = "CTI",
    y = "Aridity Index (PET / P)"
  ) +
  theme_classic()

print(N_gg_landcover_grassland_img)

ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_grassland.png"))



  ###----------SAVANNA PET/P to CTI-----------

library(patchwork)
library(dplyr)
library(ggplot2)
library(ggrepel)

N_highlight_sites_savanna <- N_merged_characteristics_df |>
  filter(igbp_land_use == "SAV",
         epsilon_deviation > N_area_epsilon
  )

N_gg_landcover_savanna_img <- N_merged_characteristics_df |>
  filter(igbp_land_use == "SAV") |>
  ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
  geom_point(shape = 21, size = 4, color = "black") +
  scale_fill_gradient2(
    low = "darkslategrey", mid = "beige", high = "hotpink4",
    midpoint = 0,
    limits = c(-epsilon_abs_max, epsilon_abs_max),
    name = expression(epsilon*"′ (Deviation)"),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_scientific(digits = 2)
  ) +
  coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
  geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
  geom_text_repel(
    data = N_highlight_sites_savanna,
    aes(label = sitename),
    size = 4,
    color = "black",
    box.padding = 0.4,
    point.padding = 0.5,
    segment.color = "black",
    max.overlaps = Inf,
    force = 2,
    max.time = 5,
    max.iter = 2000
  ) +
  labs(
    title = "ε′ across CTI and Aridity Index for SAVANNA",
    x = "CTI",
    y = "Aridity Index (PET / P)"
  ) +
  theme_classic()

print(N_gg_landcover_savanna_img)

ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_savanna.png"))


###----------WET- and SHRUBLAND PET/P to CTI-----------

library(patchwork)
library(dplyr)
library(ggplot2)
library(ggrepel)

N_highlight_sites_wetandshrub <- N_merged_characteristics_df |>
  filter(igbp_land_use == "WSA",
         epsilon_deviation > N_area_epsilon
  )

N_gg_landcover_wetandshrub_img <- N_merged_characteristics_df |>
  filter(igbp_land_use == "WSA") |>
  ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
  geom_point(shape = 21, size = 4, color = "black") +
  scale_fill_gradient2(
    low = "darkslategrey", mid = "beige", high = "hotpink4",
    midpoint = 0,
    limits = c(-epsilon_abs_max, epsilon_abs_max),
    name = expression(epsilon*"′ (Deviation)"),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_scientific(digits = 2)
  ) +
  coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
  geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
  geom_text_repel(
    data = N_highlight_sites_wetandshrub,
    aes(label = sitename),
    size = 4,
    color = "black",
    box.padding = 0.4,
    point.padding = 0.5,
    segment.color = "black",
    max.overlaps = Inf,
    force = 2,
    max.time = 5,
    max.iter = 2000
  ) +
  labs(
    title = "ε′ across CTI and Aridity Index for WET- and SHRUBLAND",
    x = "CTI",
    y = "Aridity Index (PET / P)"
  ) +
  theme_classic()

print(N_gg_landcover_wetandshrub_img)

ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_wetandsrhub.png"))


###----------CROPLANDS PET/P to CTI-----------

library(patchwork)
library(dplyr)
library(ggplot2)
library(ggrepel)

N_highlight_sites_cropland <- N_merged_characteristics_df |>
  filter(igbp_land_use == "CRO",
         epsilon_deviation > N_area_epsilon
  )

N_gg_landcover_cropland_img <- N_merged_characteristics_df |>
  filter(igbp_land_use == "CRO") |>
  ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
  geom_point(shape = 21, size = 4, color = "black") +
  scale_fill_gradient2(
    low = "darkslategrey", mid = "beige", high = "hotpink4",
    midpoint = 0,
    limits = c(-epsilon_abs_max, epsilon_abs_max),
    name = expression(epsilon*"′ (Deviation)"),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_scientific(digits = 2)
  ) +
  coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
  geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
  geom_text_repel(
    data = N_highlight_sites_cropland,
    aes(label = sitename),
    size = 4,
    color = "black",
    box.padding = 0.4,
    point.padding = 0.5,
    segment.color = "black",
    max.overlaps = Inf,
    force = 2,
    max.time = 5,
    max.iter = 2000
  ) +
  labs(
    title = "ε′ across CTI and Aridity Index for CROPLANDS",
    x = "CTI",
    y = "Aridity Index (PET / P)"
  ) +
  theme_classic()

print(N_gg_landcover_cropland_img)

ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_cropland.png"))



###----------CROPLANDS PET/P to CTI-----------Canopy Height -----------


