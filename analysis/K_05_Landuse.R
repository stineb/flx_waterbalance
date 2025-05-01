

## LANDCOVER IN ARIDITY-INDEX AND CTI DIAGRAM

N_CORR_area_cti <- quantile(
    N_CORR_merged_df$cti,
    probs = 2/3,
    na.rm = TRUE
  )

  N_CORR_area_pet_p <- quantile(
    N_CORR_merged_df$pet / N_CORR_merged_df$water_input,
    probs = 2/3,
    na.rm = TRUE
  )

  N_CORR_area_epsilon <- quantile(
    N_CORR_merged_df$epsilon_deviation[N_CORR_merged_df$epsilon_deviation > 0],
    probs = 0.7,
    na.rm = TRUE
  )

  # global scaling for different landcover types in graph
  N_CORR_xlim_cti <- range(c(0, N_CORR_merged_df$cti))
  N_CORR_ylim_aridity <- range(c(0, N_CORR_merged_df$pet / N_CORR_merged_df$water_input))

  N_highlight_sites_all <- N_CORR_merged_df |>
    filter(
      !is.na(epsilon_deviation),
      !is.na(cti),
      !is.na(water_input),
      !is.na(pet),
      epsilon_deviation > N_CORR_area_epsilon,
      cti > N_CORR_area_cti,
      pet / water_input > N_CORR_area_pet_p
    )

  # #---------------------------DIAGRAMs---------------------------------------------------------------------------------------------

#Highlight all Sites

library(ggplot2)
library(ggrepel)


  N_gg_landcover_all_img <- N_CORR_merged_df |>
    ggplot(aes(x = cti, y = pet / water_input, fill = epsilon_deviation)) +
    geom_point(shape = 21, size = 4, color = "black") +
    scale_fill_gradient2(
      low = "darkslategrey", mid = "beige", high = "hotpink4",
      midpoint = 0,
      name = expression(epsilon*"′ (Deviation)"),
      limits = c(-max(abs(N_CORR_merged_df$epsilon_deviation), na.rm = TRUE),
                 max(abs(N_CORR_merged_df$epsilon_deviation), na.rm = TRUE)),
      breaks = scales::pretty_breaks(n = 5),
      labels = scales::label_scientific(digits = 2)
    ) +
    coord_cartesian(
      xlim = N_CORR_xlim_cti,
      ylim = N_CORR_ylim_aridity
    ) +
    geom_vline(xintercept = N_CORR_area_cti, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = N_CORR_area_pet_p, linetype = "dashed", color = "grey50") +
    geom_text_repel(
      data = N_highlight_sites_all,
      aes(label = igbp_land_use),
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
      title = expression("ε′ across CTI and Aridity Index (All Land Covers)"),
      x = "CTI",
      y = "Aridity Index (PET / P)"
    ) +
    theme_classic()

  print(N_gg_landcover_all_img)



# #---------------------------HEATMAP---------------------------------------------------------------------------------------------


N_CORR_cti_breaks <- c(2.285880, 4.197392, 4.942680, 5.849463, 6.893455, 14.612223)
N_CORR_cti_labels <- c("very low", "low", "middle", "high", "very high")

  # Neue CTI-Klassen im DataFrame anlegen
N_CORR_merged_df <- N_CORR_merged_df |>
  mutate(cti_class = cut(
    cti,
    breaks = N_cti_breaks,
    labels = N_cti_labels,
    include.lowest = TRUE,
    right = TRUE
  ))

N_CORR_heatmap_data_cti <- N_CORR_merged_df |>
  group_by(cti_class, igbp_land_use) |>
  summarise(
    mean_epsilon = mean(epsilon_deviation, na.rm = TRUE),
    .groups = "drop"
  )



epsilon_abs_max <- max(abs(N_CORR_heatmap_data_cti$mean_epsilon), na.rm = TRUE)

ggplot(N_CORR_heatmap_data_cti, aes(x = cti_class, y = igbp_land_use, fill = mean_epsilon)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "darkslategrey",
    mid = "beige",
    high = "hotpink4",
    midpoint = 0,
    limits = c(-epsilon_abs_max, epsilon_abs_max),
    name = expression(epsilon*"′ (Mean Deviation)"),
    labels = scales::label_scientific(digits = 2),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Heatmap: CTI Classes, Land Cover Types and ε′",
    x = "CTI Class",
    y = "Land Cover"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# #---------------------------DIAGRAMs---------------------------------------------------------------------------------------------
#
#
# install.packages("patchwork")
# library(patchwork)
# library(dplyr)
# library(ggplot2)
# library(ggrepel)
#
#
# N_area_cti <- quantile(
#   N_merged_characteristics_df$cti,
#   probs = 2/3,
#   na.rm = TRUE
# )
#
# N_area_pet_p <- quantile(
#   N_merged_characteristics_df$pet / N_merged_characteristics_df$prec,
#   probs = 2/3,
#   na.rm = TRUE
# )
#
# N_area_epsilon <- quantile(
#   N_merged_characteristics_df$epsilon_deviation[N_merged_characteristics_df$epsilon_deviation > 0],
#   probs = 0.7,
#   na.rm = TRUE
# )
#
# # global scaling for different landcover types in graph
# N_xlim_cti <- range(c(0, N_merged_characteristics_df$cti))
# N_ylim_aridity <- range(c(0, N_merged_characteristics_df$pet / N_merged_characteristics_df$prec))
#
#
#
#
# ###----------GRASSLAND PET/P to CTI-----------
#
#
# #focus on sites with deviation higher 0.7 quintile
# N_highlight_sites_grassland <- N_merged_characteristics_df |>
#   filter(
#     igbp_land_use == "GRA",
#     epsilon_deviation > N_area_epsilon
#   )
#
# N_gg_landcover_grassland_img <- N_merged_characteristics_df |>
#   filter(igbp_land_use == "GRA") |>
#   ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
#   geom_point(shape = 21, size = 4, color = "black") +
#   scale_fill_gradient2(
#     low = "darkslategrey", mid = "beige", high = "hotpink4",
#     midpoint = 0,
#     limits = c(-epsilon_abs_max, epsilon_abs_max),
#     name = expression(epsilon*"′ (Deviation)"),
#     breaks = scales::pretty_breaks(n = 5),
#     labels = scales::label_scientific(digits = 2)
#   ) +
#   coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
#   geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
#   geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
#   geom_text_repel(
#     data = N_highlight_sites_grassland,
#     aes(label = sitename),
#     size = 4,
#     color = "black",
#     box.padding = 0.4,
#     point.padding = 0.5,
#     segment.color = "black",
#     max.overlaps = Inf,
#     force = 2,
#     max.time = 5,
#     max.iter = 2000
#   ) +
#   labs(
#     title = "ε′ across CTI and Aridity Index for GRASSLAND",
#     x = "CTI",
#     y = "Aridity Index (PET / P)"
#   ) +
#   theme_classic()
#
# print(N_gg_landcover_grassland_img)
#
# ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_grassland.png"))
#
#
#
#   ###----------SAVANNA PET/P to CTI-----------
#
# library(patchwork)
# library(dplyr)
# library(ggplot2)
# library(ggrepel)
#
# N_highlight_sites_savanna <- N_merged_characteristics_df |>
#   filter(igbp_land_use == "SAV",
#          epsilon_deviation > N_area_epsilon
#   )
#
# N_gg_landcover_savanna_img <- N_merged_characteristics_df |>
#   filter(igbp_land_use == "SAV") |>
#   ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
#   geom_point(shape = 21, size = 4, color = "black") +
#   scale_fill_gradient2(
#     low = "darkslategrey", mid = "beige", high = "hotpink4",
#     midpoint = 0,
#     limits = c(-epsilon_abs_max, epsilon_abs_max),
#     name = expression(epsilon*"′ (Deviation)"),
#     breaks = scales::pretty_breaks(n = 5),
#     labels = scales::label_scientific(digits = 2)
#   ) +
#   coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
#   geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
#   geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
#   geom_text_repel(
#     data = N_highlight_sites_savanna,
#     aes(label = sitename),
#     size = 4,
#     color = "black",
#     box.padding = 0.4,
#     point.padding = 0.5,
#     segment.color = "black",
#     max.overlaps = Inf,
#     force = 2,
#     max.time = 5,
#     max.iter = 2000
#   ) +
#   labs(
#     title = "ε′ across CTI and Aridity Index for SAVANNA",
#     x = "CTI",
#     y = "Aridity Index (PET / P)"
#   ) +
#   theme_classic()
#
# print(N_gg_landcover_savanna_img)
#
# ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_savanna.png"))
#
#
# ###----------WET- and SHRUBLAND PET/P to CTI-----------
#
# library(patchwork)
# library(dplyr)
# library(ggplot2)
# library(ggrepel)
#
# N_highlight_sites_wetandshrub <- N_merged_characteristics_df |>
#   filter(igbp_land_use == "WSA",
#          epsilon_deviation > N_area_epsilon
#   )
#
# N_gg_landcover_wetandshrub_img <- N_merged_characteristics_df |>
#   filter(igbp_land_use == "WSA") |>
#   ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
#   geom_point(shape = 21, size = 4, color = "black") +
#   scale_fill_gradient2(
#     low = "darkslategrey", mid = "beige", high = "hotpink4",
#     midpoint = 0,
#     limits = c(-epsilon_abs_max, epsilon_abs_max),
#     name = expression(epsilon*"′ (Deviation)"),
#     breaks = scales::pretty_breaks(n = 5),
#     labels = scales::label_scientific(digits = 2)
#   ) +
#   coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
#   geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
#   geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
#   geom_text_repel(
#     data = N_highlight_sites_wetandshrub,
#     aes(label = sitename),
#     size = 4,
#     color = "black",
#     box.padding = 0.4,
#     point.padding = 0.5,
#     segment.color = "black",
#     max.overlaps = Inf,
#     force = 2,
#     max.time = 5,
#     max.iter = 2000
#   ) +
#   labs(
#     title = "ε′ across CTI and Aridity Index for WET- and SHRUBLAND",
#     x = "CTI",
#     y = "Aridity Index (PET / P)"
#   ) +
#   theme_classic()
#
# print(N_gg_landcover_wetandshrub_img)
#
# ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_wetandsrhub.png"))
#
#
# ###----------CROPLANDS PET/P to CTI-----------
#
# library(patchwork)
# library(dplyr)
# library(ggplot2)
# library(ggrepel)
#
# N_highlight_sites_cropland <- N_merged_characteristics_df |>
#   filter(igbp_land_use == "CRO",
#          epsilon_deviation > N_area_epsilon
#   )
#
# N_gg_landcover_cropland_img <- N_merged_characteristics_df |>
#   filter(igbp_land_use == "CRO") |>
#   ggplot(aes(x = cti, y = pet / prec, fill = epsilon_deviation)) +
#   geom_point(shape = 21, size = 4, color = "black") +
#   scale_fill_gradient2(
#     low = "darkslategrey", mid = "beige", high = "hotpink4",
#     midpoint = 0,
#     limits = c(-epsilon_abs_max, epsilon_abs_max),
#     name = expression(epsilon*"′ (Deviation)"),
#     breaks = scales::pretty_breaks(n = 5),
#     labels = scales::label_scientific(digits = 2)
#   ) +
#   coord_cartesian(xlim = N_xlim_cti, ylim = N_ylim_aridity) +
#   geom_vline(xintercept = N_area_cti, linetype = "dashed", color = "grey50") +
#   geom_hline(yintercept = N_area_pet_p, linetype = "dashed", color = "grey50") +
#   geom_text_repel(
#     data = N_highlight_sites_cropland,
#     aes(label = sitename),
#     size = 4,
#     color = "black",
#     box.padding = 0.4,
#     point.padding = 0.5,
#     segment.color = "black",
#     max.overlaps = Inf,
#     force = 2,
#     max.time = 5,
#     max.iter = 2000
#   ) +
#   labs(
#     title = "ε′ across CTI and Aridity Index for CROPLANDS",
#     x = "CTI",
#     y = "Aridity Index (PET / P)"
#   ) +
#   theme_classic()
#
# print(N_gg_landcover_cropland_img)
#
# ggsave(here::here("~/flx_waterbalance/data/N_deviation_cti_covertype_cropland.png"))
#
#
#
# ###----------CROPLANDS PET/P to CTI-----------Canopy Height -----------
