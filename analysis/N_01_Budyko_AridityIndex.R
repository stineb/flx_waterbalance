


library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(cwd)
install.packages('gghighlight')
library(gghighlight)
install.packages('cowplot')
library(cowplot)

# Get site meta info -----------------
N_site_df <- readr::read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_info.csv") |> # former sites
  filter(!(sitename %in% c("MX-Tes", "US-KS3"))) |>  # failed sites
  left_join(
    readr::read_csv("/data_2/FluxDataKit/v3.4/zenodo_upload/fdk_site_fullyearsequence.csv"),
    by = "sitename"
  ) |>
  filter(!drop_lecorr) |>  # where no full year sequence was found
  filter(nyears_lecorr >= 3)

# Load data -------------
N_path <- "/data_2/FluxDataKit/v3.4/zenodo_upload/fluxnet/"  # former path
N_read_onesite <- function(site, path){ #former read_onesite
  filename <- list.files(path = path,
                         pattern = paste0("FLX_", site, "_FLUXDATAKIT_FULLSET_DD"),
                         full.names = TRUE
  )
  out <- read_csv(filename) |>
    mutate(sitename = site)
  return(out)
}

# read all daily data for the selected sites
N_daily_data <- purrr::map_dfr( #former ddf
  N_site_df$sitename,
  ~N_read_onesite(., path)
)

# Select data sequences ----------------
N_daily_data <- N_daily_data |> # former ddf
  left_join(
    N_site_df |>
      select(
        sitename,
        year_start = year_start_lecorr,
        year_end = year_end_lecorr),
    by = join_by(sitename)
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  filter(year >= year_start & year <= year_end) |>
  select(-year_start, -year_end, -year)

#function for converting latent energy to et
N_l_energy_to_et <- function(le, tc, patm){ #former le_to_et
  1000 * 60 * 60 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
}

# Get mean annual X
N_annual_means_df <- N_daily_data |>   # former adf and ddf
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    # LE_F_MDS = latent energy flux
    #TA_F_MDS = Temperature to determine the latent heat of vaporization
    #PA_F = Air pressure for conversion from energy flux to water flux
    le_mm = N_l_energy_to_et(LE_F_MDS, TA_F_MDS, PA_F),    #Umwandlung von Latentwärmefluss in Verdunstung in mm pro Tag
    le_mm_corr = N_l_energy_to_et(LE_CORR, TA_F_MDS, PA_F), # Umwandlung mit korrigierten latenten Wärmeflüssen in mm pro Tag
    pet = 60 * 60 * 24 * cwd::pet(NETRAD, TA_F_MDS, PA_F)  # Umrechnung von W/m² zu mm
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  group_by(sitename, year) |>
  summarise(
    prec = sum(P_F),
    aet = sum(le_mm),
    aet_corr = sum(le_mm_corr),
    pet = sum(pet)
  ) |>
  ungroup() |>                #Mittelwerte pro site
  group_by(sitename) |>
  summarise(
    prec = mean(prec, na.rm = TRUE),
    aet = mean(aet, na.rm = TRUE),
    aet_corr = mean(aet_corr, na.rm = TRUE),
    pet = mean(pet, na.rm = TRUE)
  )




# load condensation data:
N_cond_df <- read.csv("/data/scratch/bstocker/foranna/df_cond_mean_ann.csv")

N_annual_means_df <- N_annual_means_df |>
  left_join(
    N_cond_df |>
      select(sitename, cond_mean_ann),
    by = "sitename"
  )

##----- Budyko curve  ------------------------------------

######theoretical Budyko-curve:
#-----------------------------


#budyko formula by Fu:
N_budyko_curve <- function(DI, omega = 2) { #
  1 + DI - (1 + DI^omega)^(1 / omega)
}

# generate according to budyko formula 300 points from 0-9 with theoretical curve
N_budyko_data <- data.frame(
  aridity = seq(0, 9, length.out = 300)
)

N_budyko_data$evaporation <- N_budyko_curve(N_budyko_data$aridity)
N_annual_means_df$aridity <- N_annual_means_df$pet / N_annual_means_df$prec #add additional row 'aridity'

# Budyko theoretical per site
N_annual_means_df <- N_annual_means_df |>
  mutate(evap_theory = N_budyko_curve(aridity))

# filter sites   higher Aridityindex 3 and above theoretical curve
# N_annual_means_df <- N_annual_means_df |>
#   mutate(over_theory = (aet_corr / prec) > evap_theory & aridity >= 3)

N_annual_means_df <- N_annual_means_df |>
  mutate(over_budyko_label = ifelse((aet_corr / prec) > evap_theory & aridity >= 3,
                                    "Over Budyko & Aridity ≥ 3", NA))


##CONDENSATION:::
#-----------------------------------------------
# add condensation and update AET_CORR

N_annual_means_df <- N_annual_means_df |>
  mutate(
    aet_corr_with_cond = aet_corr + cond_mean_ann,
    water_input = prec + cond_mean_ann,
    aridity_with_cond = pet / water_input,
    evap_theory_with_cond = N_budyko_curve(aridity_with_cond),
    evap_index_with_cond = aet_corr_with_cond / water_input,
    over_budyko_label = ifelse(evap_index_with_cond > evap_theory_with_cond & aridity_with_cond >= 3,
                               "Over Budyko & Aridity ≥ 3", NA)
  )





#---------------------------------### PLOTS ####---------------------------------------------------



#plot with lecorr and condensation:
N_gg_budyko_img_aridity_lecorr_cond <- N_annual_means_df |>
  ggplot(aes(x = aridity_with_cond, y = evap_index_with_cond)) +
  geom_point(aes(color = over_budyko_label), na.rm = TRUE) +  # Points colored by over Budyko label
  scale_color_manual(values = c("Over Budyko & Aridity ≥ 3" = "firebrick")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_line(data = N_budyko_data, aes(x = aridity, y = evaporation),
            inherit.aes = FALSE, color = "black", linewidth = 0.7, linetype = "solid") +
  theme_classic() +
  scale_x_continuous(limits = c(0, 8)) +
  ylim(0, 5) +
  labs(
    x = "Aridity Index (PET / P)",
    y = "Evaporative Index (AET / P)",
    title = "Budyko Curve  (With Condensation)",
    color = NULL
  )+
  theme(legend.position = "none")

plot(N_gg_budyko_img_aridity_lecorr_cond)
ggsave(here::here("~/flx_waterbalance/data/N_gg_budyko_lecorr_and_cond-high_aridity.png"))



# N_gg_budyko_img_aridity_lecorr_cond <- N_annual_means_df |>
#   ggplot(aes(x = aridity_with_cond, y = evap_index_with_cond)) +
#   geom_point(aes(color = over_budyko_label), na.rm = TRUE) +
#   scale_color_manual(values = c("Over Budyko & Aridity ≥ 3" = "firebrick")) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   geom_line(data = N_budyko_data, aes(x = aridity, y = evaporation),
#             inherit.aes = FALSE, color = "black", linewidth = 0.7, linetype = "solid") +
#   theme_classic() +
#   scale_x_continuous(limits = c(0, 8)) +
#   ylim(0, 2) +  # ggf. begrenzen, wenn 5 zu viel ist
#   labs(
#     x = "Aridity Index (PET / (P + Condensation))",
#     y = "Evaporative Index ((AET_CORR + Cond) / (P + Cond))",
#     title = "Budyko Curve and Fluxnet Sites (With Condensation)",
#     color = NULL
#   )






#plot with lecorr and without condensation:

N_annual_means_df <- N_annual_means_df |>
  mutate(over_budyko_label = ifelse((aet_corr / prec) > evap_theory & aridity >= 3,
                                    "Over Budyko & Aridity ≥ 3", NA))

N_gg_budyko_img_aridity <- N_annual_means_df |>
  ggplot(aes(x = aridity, y = aet_corr / prec)) +
  geom_point(aes(color = over_budyko_label), na.rm = TRUE) +
  scale_color_manual(values = c("Over Budyko & Aridity ≥ 3" = "firebrick")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_line(data = N_budyko_data, aes(x = aridity, y = evaporation),
            inherit.aes = FALSE, color = "black", linewidth = 0.7, linetype = "solid") +
  theme_classic() +
  scale_x_continuous(limits = c(0, 8)) +
  ylim(0, 5) +
  labs(
    x = "Aridity Index (PET / P)",
    y = "Evaporative Index (AET / P)",
    title = "Budyko Curve (Without Condensateion)",
    color = NULL
  )+
  theme(legend.position = "none")

plot(N_gg_budyko_img_aridity)
ggsave(here::here("~/flx_waterbalance/data/N_gg_budyko_lecorr-high_aridity.png"))



cowplot::plot_grid(N_gg_budyko_img_aridity_lecorr_cond, N_gg_budyko_img_aridity, ncol =2)

#--------------------------------------------------------------------------------------------------

# N_gg_budyko_img_aridity <- N_annual_means_df |>
#   ggplot(aes(x = aridity, y = aet_corr / prec)) +
#   geom_point(aes(color = over_theory)) +
#   #scale_color_manual(values = c("FALSE" = "grey43", "TRUE" = "firebrick")) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   geom_line(data = N_budyko_data, aes(x = aridity, y = evaporation),
#             inherit.aes = FALSE, color = "black", linewidth = 0.7, linetype = "solid") +
#   theme_classic() +
#   scale_x_continuous(limits = c(0, 10)) +
#   ylim(0, 7) +
#   labs(
#     x = "Aridity Index (PET / P)",
#     y = "Evaporative Index (AET_CORR / P)",
#     title = "Budyko Curve and Fluxnet Sites",
#   ) +
#
#
# plot(N_gg_budyko_img_aridity)
#
# 4# Plot with all sites with high
# # aritity index (10% quintiles):
# #--------------------------------------
#
# ## expand plot with upper and lower quintiles og site's means
#
#
#
#
# ##aridity quintiles:
# #N_lower_quintile <- quantile(N_annual_means_df$aridity, 0.10, na.rm = TRUE)
# N_upper_quintile <- quantile(N_annual_means_df$aridity, 0.9, na.rm = TRUE)
# #N_lower_quintile_budyko <- N_annual_means_df[N_annual_means_df$aridity <= N_lower_quintile, ]
# N_upper_quintile_budyko <- N_annual_means_df[N_annual_means_df$aridity >= N_upper_quintile, ]
#
# N_gg_budyko_img_aridity <- N_annual_means_df |>
#   #ggplot(aes(x = pet / prec, y = aet / prec)) +
#   ggplot(aes(x = pet / prec, y = aet_corr / prec)) +
#   geom_point(color = "grey43") +
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   geom_line(data = N_budyko_data, aes(x = aridity, y = evaporation),
#             inherit.aes = FALSE, color = "black", linewidth = 0.7, linetype = "solid") +
#   #geom_point(data= N_lower_quintile_budyko, color = "firebrick") +
#   geom_point(data = N_upper_quintile_budyko, color = "firebrick") +
#   theme_classic() +
#   scale_x_continuous(limits = c(0, NA))+
#   xlim(0, 6) +
#   ylim(0, 2.5) +
#   labs(
#     x = "Aridity Index (PET / P)",
#     #y = " Evaporative Index (AET / P)",
#     y = " Evaporative Index (AET_CORR / P)",
#     title = "Budyko Curve and Fluxnet Sites"
#   )
#
# plot(N_gg_budyko_img_aridity)
#ggsave(here::here("~/flx_waterbalance/data/budyko-high_aridity.png"))






## AET-PET  by Geco----------

# gg1 <- N_annual_means_df |>   #former gg1 and adf
#   ggplot(
#     aes(
#       x = pet,
#       y = aet
#     )
#   ) +
#   geom_point(color = "tomato") +
#   gghighlight(
#     sitename %in% c("DE-Hai", "US-Ton", "FI-Hyy", "US-ICh", "AU-How"),
#     label_key = sitename,
#     use_direct_label = FALSE,
#     unhighlighted_params = list(color = "grey40")
#   ) +
#   geom_label(aes(label = sitename),
#              hjust = 1, vjust = 1, fill = "white", colour = "black", alpha = 0.7) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#   geom_hline(yintercept = 1, linetype = "dotted") +
#   theme_classic() +
#   xlim(-30, 2500) +
#   labs(
#     x = expression(paste("PET (mm yr"^-1, ")")),
#     y = expression(paste("AET (mm yr"^-1, ")"))
#   )


