

#create table with corrected and necessary columns for statistical analysis
N_CORR_merged_df <- N_CORR_budyko_omega_df |>
  select(
      sitename,
      epsilon_deviation,
      water_input,
      aet_corr_with_cond,
      evap_index_with_cond,
  ) |>
  mutate(
    aridity_index_with_cond = pet/water_input
  ) |>
  left_join(
    N_annual_means_df |>
      select(
        sitename,
        pet),
    by = join_by(sitename)
  ) |>
  left_join(
    N_merged_characteristics_df |>
      select(
        sitename,
        whc,
        canopy_height,
        cti,
        cti_class),
    by = join_by(sitename)
  )|>
  left_join(
    flx_all_data |>
      select(
        sitename,
        igbp_land_use),
    by = join_by(sitename)
  )


#####------------Regressions-------------####




#- CTI and ε′ (Deviation

ggplot(N_CORR_merged_df,
       aes(x = cti, y = epsilon_deviation)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", color = "aquamarine4") +
  labs(
    title = "CTI vs. ε′ (Deviation from Budyko Curve)",
    x = "Compound Topographic Index (CTI)",
    y = expression(epsilon*"′ (Deviation from Budyko Curve)")
  ) +
  theme_minimal()



# - CTI and Evaporative Index AET/P
ggplot(N_CORR_merged_df,
       aes(x = cti, y = evap_index_with_cond)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", color = "aquamarine4") +
  labs(
    title = "CTI vs. Evaporative Index AET / P",
    x = "Compound Topographic Index (CTI)",
    y = "Evaporative Index AET/P"
  ) +
  theme_minimal()



# - CTI and Aridity Index PET/P

ggplot(N_CORR_merged_df,
       aes(x = cti, y = aridity_index_with_cond)) +
  geom_point(alpha = 0.9) +
  geom_smooth(method = "lm", color = "aquamarine4") +
  labs(
    title = "CTI vs. Aridity Index PET / P",
    x = "Compound Topographic Index (CTI)",
    y = "Aridity Index AET/P"
  ) +
  theme_minimal()



#####------------Multiple - Regressions-------------####


N_CORR_multreg_deviation_cti <- lm(epsilon_deviation ~ aridity_index_with_cond + evap_index_with_cond + cti, data = N_CORR_merged_df)

summary(N_CORR_multreg_deviation_cti)






















#----EPSILON DEVIATIONS-------



#------Boxplot:
#-----------------------

library(ggplot2)
library(ggrepel)

# Extremwerte extrahieren

# N_extremes <- N_CORR_budyko_omega_df |>
#   arrange(desc(epsilon_deviation)) |>
#   slice_head(n = 5)
#
# # Boxplot mit Jitter und Ausreißer-Beschriftung
# ggplot(N_CORR_budyko_omega_df, aes(x = "", y = epsilon_deviation)) +
#   geom_boxplot(outlier.shape = NA, fill = "lightblue", color = "black", width = 0.3) +
#   geom_jitter(width = 0.1, alpha = 0.4, size = 2, color = "grey30") +
#   geom_point(data = N_extremes, aes(y = epsilon_deviation), color = "firebrick", size = 2) +
#   geom_text_repel(
#     data = N_extremes,
#     aes(y = epsilon_deviation, label = round(epsilon_deviation, 2)),
#     x = 1.2,
#     size = 4,
#     color = "firebrick"
#   ) +
#   labs(
#     title = "Top 10 positive ε′-Abweichungen",
#     y = expression(epsilon*"′ (Deviation)"),
#     x = NULL
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_blank())
#











###-----------CTI and ε′----------###

### Regressionline with Scatterplot

ggplot(N_merged_characteristics_df, aes(x = cti, y = epsilon_deviation)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "CTI vs. ε′ (Deviation from Budyko Curve)",
       x = "Compound Topographic Index (CTI)",
       y = "ε′ (Deviation)") +
  theme_minimal()
