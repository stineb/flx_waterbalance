
library(dplyr)



###---- ADJUSTMENT WITH CONDENSATION AND LE_CORR:::
#-----------------------------------------------------

N_CORR_solve_omega <- function(aet_corr_with_cond, water_input, pet) {
  N_CORR_evaporation_ratio <- aet_corr_with_cond / water_input

  # Check for invalid values
  if (is.na(N_CORR_evaporation_ratio) || N_CORR_evaporation_ratio < 0 || is.infinite(N_CORR_evaporation_ratio)) {
    return(NA)
  }

  N_CORR_fun_abs <- function(omega) {
    abs((1 + (pet / water_input)^omega)^(-1 / omega) - N_CORR_evaporation_ratio)
  }

  # Perform the optimization
  opt <- tryCatch({
    optimize(N_CORR_fun_abs, interval = c(0.0001, 100))
  }, error = function(e) {
    return(list(minimum = NA))
  })

  # Return the result
  return(opt$minimum)
}

N_CORR_budyko_omega_df <- N_annual_means_df %>%
  filter(!is.na(aet_corr_with_cond) & !is.na(water_input) & !is.na(pet)) %>%
  mutate(
    water_input = prec + cond_mean_ann,
    aet_corr_with_cond = aet_corr + cond_mean_ann,
    omega = mapply(N_CORR_solve_omega, aet_corr_with_cond, water_input, pet),
    Evap_Index_predicted = mapply(N_CORR_compute_evap_index, pet, water_input, omega),
    Evap_Index_observed = aet_corr_with_cond / water_input,
    epsilon_deviation = Evap_Index_observed - Evap_Index_predicted
  )

#-------------------------------------------------------------------------------------------------------------------------





### ----- SOLVING OMEGA WITHOUT COND AND LE_CORR ADJUSTMENT

# N_solve_omega <- function(aet, p, pet) {
#   N_evaporation_ratio <- aet / p                                          #observed EvaporationIndex ε
#   if (is.na(N_evaporation_ratio) || N_evaporation_ratio < 0) return(NA)   #no negative values permitted (physically not possible)
#
#   #calculate absolute lapse in function to find omega (eg. according formula near Evaporative Ratio)
#   N_fun_abs <- function(omega) {
#     abs((1 + (pet / p)^omega)^(-1 / omega) - N_evaporation_ratio)
#   }
#
#   # find omega value, where lapse is minimal in searching in given intervall
#   opt <- optimize(N_fun_abs, interval = c(0.0001, 100))
#   return(opt$minimum)
# }
#
# #estimate how much much P will be lost as actual ET
# #calculate predicted ε0 (modelled / theoretical Index)
# N_compute_evap_index <- function(pet, p, omega) {
#   (1 + (pet / p)^omega)^(-1 / omega)                        # predicted ε
# }
#
# N_budyko_omega_df <- N_annual_means_df |>
#   mutate(
#     omega = mapply(N_solve_omega, aet, prec, pet),
#     Evap_Index_predicted = mapply(N_compute_evap_index, pet, prec, omega),
#     Evap_Index_observed = aet / prec,
#     epsilon_deviation = Evap_Index_observed - Evap_Index_predicted  # ε' = ε - ε0
#   )
#
#
#
# head(N_budyko_omega_df)

#-------------------------------------------------------------------------------------------------------------------------


####PLOTS --------------------------------------------------

N_CORR_budyko_data <- data.frame(
  DI = seq(0, 9, length.out = 2000)
) |>
  mutate(evap_index = N_budyko_curve(DI, omega = mean(N_CORR_budyko_omega_df$omega, na.rm = TRUE)))

N_CORR_budyko_omega_df <- N_CORR_budyko_omega_df |>
  mutate(
    x = pet / water_input,
    y = Evap_Index_observed,
    budyko_theoretical = N_budyko_curve(x, omega = mean(omega, na.rm = TRUE)),
    epsilon_actual = y - budyko_theoretical
  )

top_pos_devs <- N_CORR_budyko_omega_df |>
  arrange(desc(epsilon_actual)) |>
  slice(1:5)

top_neg_devs <- N_CORR_budyko_omega_df |>
  arrange(epsilon_actual) |>
  slice(1:5)


ggplot(N_CORR_budyko_omega_df, aes(x = x, y = y)) +
  geom_line(data = N_CORR_budyko_data, aes(x = DI, y = evap_index),
            color = "grey", linewidth = 0.5) +
  geom_point(aes(color = epsilon_actual), size = 1.5) +
  geom_point(data = top_pos_devs, aes(x = x, y = y), color = "firebrick2", size = 1, shape = 17) +
  geom_point(data = top_neg_devs, aes(x = x, y = y), color = "dodgerblue4", size = 1, shape = 15) +
  geom_text_repel(data = top_pos_devs, aes(label = sitename), color = "firebrick2", size = 3) +
  geom_text_repel(data = top_neg_devs, aes(label = sitename), color = "dodgerblue4", size = 3) +
  scale_color_gradient2(
    low = "dodgerblue4", mid = "white", high = "firebrick2",
    midpoint = 0,
    name = expression(epsilon*"′ (Deviation)")
  ) +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Budyko Curve with Top ε′ Deviations",
    #subtitle = paste0("Using mean omega = ", round(mean(N_CORR_budyko_omega_df$omega, na.rm = TRUE), 2)),
    x = "Aridity Index PET / P",
    y = "Evaporative Index (AET / P )"
  ) +
  theme_minimal()












#-------------------------------------------------------------------------------------------------------------------------


## top ten deviating sites
install.packages('ggrepel')
library(ggrepel)


### LECORR AND COND
epsilon_min <- min(N_CORR_budyko_omega_df$epsilon_deviation, na.rm = TRUE)
epsilon_max <- max(N_CORR_budyko_omega_df$epsilon_deviation, na.rm = TRUE)

# Auswahl der Top-10 Standorte mit positiver Abweichung
N_CORR_positive_epsilon <- N_CORR_budyko_omega_df |>
  filter(epsilon_deviation > 0) |>
  arrange(desc(epsilon_deviation)) |>
  slice(1:10) |>
  mutate(x = pet / prec, y = Evap_Index_observed)

# Plot
ggplot(N_CORR_budyko_omega_df, aes(x = pet / water_input, y = Evap_Index_observed)) +
  geom_point(aes(color = epsilon_deviation), size = 1.5) +
  scale_color_gradient2(
    low = "white", mid = "dodgerblue4", high = "firebrick2", midpoint = 0,
    limits = c(epsilon_min, epsilon_max),
    name = expression(epsilon*"′ (Deviation)"),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_scientific(digits = 2)
  ) +
  scale_x_continuous(limits = c(0, 8)) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title = "Budyko with positive ε′ sites (top ten) - including le_corr and cond",
    x = "Aridity Index PET / P",
    y = "Evaporative Index AET / P",
    color = "ε′ (Deviation)"
  ) +
  geom_text_repel(data = N_CORR_positive_epsilon,
                  aes(label = sitename),
                  size = 3, color = "black") +
  theme_minimal()

ggsave(here::here("~/flx_waterbalance/data/N_CORR_positive_sites_deviating_budyko.png")
)










#-------------------------------------------------------------------------------------------------------------------------


# N_positive_epsilon <- N_budyko_omega_df |>
#   filter(epsilon_deviation > 0) |>
#   arrange(desc(epsilon_deviation)) |>
#   slice(1:10)
#   #slice_max(epsilon_deviation, n=15) |>#chose top fiftheen locations
#   mutate(x = pet/prec, y = aet/prec)
#
# ggplot(N_budyko_omega_df, aes(x = pet / prec, y = aet / prec)) +
#   geom_point(aes(color = epsilon_deviation), size =1.5) +
#   scale_color_gradient2(
#     low = "white", mid = "dodgerblue4", high = "firebrick2", midpoint = 0,
#     limits = c(epsilon_min, epsilon_max),
#     name = expression(epsilon*"′ (Deviation)"),
#     breaks = scales::pretty_breaks(n = 5),  # optional: automatische sinnvolle Ticks
#     labels = scales::label_scientific(digits = 2)
#   ) +
#     labs(
#      title = "Budyko with positive ε′ sites (top ten)",
#      x = " Evaporative Index PET/P",
#      y = "Aridity Index AET/P",
#       color = "ε′ (Deviation)"
#   ) +
#   geom_text_repel(data = N_positive_epsilon,
#                   aes(label = sitename),
#                   size = 3, color = "black") +
#   theme_minimal()
#
#
# ggsave(here::here("~/flx_waterbalance/data/N_positive_sites_deviating_budyko.png"),
#        width = 9,
#        height = 6,
#        dpi = 300)





### Countourlines of ε′ per Site:

# ggplot(N_budyko_omega_df, aes(x = pet / prec, y = aet / prec)) +
#   geom_point(aes(color = epsilon_deviation), size = 3) +
#   geom_density_2d(aes(color = epsilon_deviation)) +
#   scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#   labs(title = "Budyko Diagram with ε′ Contours",
#        x = "PET / P",
#        y = "AET / P",
#        color = "ε′ (Deviation)") +
#   theme_minimal()
#
#
#



### Dots of ε′ per Site

# ggplot(N_budyko_omega_df, aes(x = pet / prec, y = aet / prec, size = abs(epsilon_deviation))) +
  # geom_point(alpha = 0.7) +
  # scale_size(range = c(0, 9)) +
  # labs(title = "Budyko Diagram: Point Size Represents |ε′|",
  #      x = "PET / P",
  #      y = "AET / P",
  #      size = "|ε′| (Absolute Deviation)") +
  # theme_minimal()




### Regressionline in Scatterploot




#Color MApping  with positive ε′ Deviation per Site -----------------------

# ggplot(clean_budyko, aes(x = PET_mean / P_mean, y = AET_mean / P_mean, color = epsilon_deviation)) +
#   geom_point(size = 2) +
#   scale_color_gradient2(
#     low = "blue4",
#     mid = "white",
#     high = "firebrick",
#     midpoint = 0) +
#   labs(
#     title = "Budyko Diagram with ε′ Deviation",
#       x = "PET / P (Dryness Index)",
#       y = "AET / P (Evaporation Index)",
#       color = "ε′ (Deviation)",
#   theme_minial()
#       #ylim = c(0,4),
#       #xlim = c(0,10)
# )


#-------------------------------------------------------------------------------------------------------------------------




