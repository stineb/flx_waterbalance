

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

N_l_energy_to_et <- function(le, tc, patm){ #former le_to_et
  1000 * 60 * 60 * 24 * le / (cwd::calc_enthalpy_vap(tc) * cwd::calc_density_h2o(tc, patm))
}

# Get mean annual X
N_annual_means_df <- N_daily_data |>   # former adf and ddf
  mutate(
    # convert latent heat flux into mass flux in mm day-1
    le_mm = N_l_energy_to_et(LE_F_MDS, TA_F_MDS, PA_F),
    pet = 60 * 60 * 24 * cwd::pet(NETRAD, TA_F_MDS, PA_F)
  ) |>
  mutate(year = year(TIMESTAMP)) |>
  group_by(sitename, year) |>
  summarise(
    prec = sum(P_F),
    aet = sum(le_mm),
    pet = sum(pet)
  ) |>
  ungroup() |>
  group_by(sitename) |>
  summarise(
    prec = mean(prec, na.rm = TRUE),
    aet = mean(aet, na.rm = TRUE),
    pet = mean(pet, na.rm = TRUE)
  )



#---------------------------------### PLOTS ####---------------------------------------------------

## AET-PET  by Geco----------

gg1 <- N_annual_means_df |>   #former gg1 and adf
  ggplot(
    aes(
      x = pet,
      y = aet
    )
  ) +
  geom_point(color = "tomato") +
  gghighlight(
    sitename %in% c("DE-Hai", "US-Ton", "FI-Hyy", "US-ICh", "AU-How"),
    label_key = sitename,
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "grey40")
  ) +
  geom_label(aes(label = sitename),
             hjust = 1, vjust = 1, fill = "white", colour = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(-30, 2500) +
  labs(
    x = expression(paste("PET (mm yr"^-1, ")")),
    y = expression(paste("AET (mm yr"^-1, ")"))
  )




##----- Budyko curve  ------------------------------------

######theoreticel Budyko-curve:
#-----------------------------

budyko_curve <- function(DI, omega = 2) { #
  1 + DI - (1 + DI^omega)^(1 / omega)
}

# generate according to budyko formula 300 points from 0-3
budyko_data <- data.frame(
  aridity = seq(0, 8, length.out = 300)
)
budyko_data$evaporation <- budyko_curve(budyko_data$aridity)




# Plot with all sites with hig
#and low aritity index (10% quintiles):
#--------------------------------------

## expand plot with upper and lower quintiles og site's means

N_annual_means_df$aridity <- N_annual_means_df$pet / N_annual_means_df$prec #add additional row 'aridity'


##aridity quintiles:
lower_quintile <- quantile(N_annual_means_df$aridity, 0.10, na.rm = TRUE)
upper_quintile <- quantile(N_annual_means_df$aridity, 0.90, na.rm = TRUE)
N_lower_quintile_budyko <- N_annual_means_df[N_annual_means_df$aridity <= lower_quintile, ]
N_upper_quintile_budyko <- N_annual_means_df[N_annual_means_df$aridity >= upper_quintile, ]
gg_budyko_img_aridity <- N_annual_means_df |>
  ggplot(aes(x = pet / prec, y = aet / prec)) +
  geom_point(color = "grey43") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_line(data = budyko_data, aes(x = aridity, y = evaporation),
            inherit.aes = FALSE, color = "black", linewidth = 0.7, linetype = "solid") +
  geom_point(data= N_lower_quintile_budyko, color = "firebrick") +
  geom_point(data = N_upper_quintile_budyko, color = "springgreen3") +
  theme_classic() +
  scale_x_continuous(limits = c(0, NA))+
  xlim(0, 7) +
  ylim(0, 5) +
  labs(
    x = "Aridity Index (PET / P)",
    y = " Evaporative Index (AET / P)",
    main = "Budyko Curve and Fluxnet Sites"
  )

plot(gg_budyko_img_aridity)
#ggsave(here::here())




# Plot with all sites with high
#and low evaporation index (10% quintiles):
#--------------------------------------


## expand plot with upper and lower quintiles og site's means

N_annual_means_df$evap_theory <- budyko_curve(N_annual_means_df$evap_theory) #calculating theoretical budyko by formula

##evaporative quintiles:
lower_quintile <- quantile(N_annual_means_df$evap_theory, 0.10, na.rm = TRUE)
upper_quintile <- quantile(N_annual_means_df$evap_theory, 0.90, na.rm = TRUE)
N_lower_quintile_budyko_evap <- N_annual_means_df[N_annual_means_df$evap_theory <= lower_quintile, ]
N_upper_quintile_budyko_evap <- N_annual_means_df[N_annual_means_df$evap_theory >= upper_quintile, ]


gg_budyko_img_evap <- N_annual_means_df |>
  ggplot(aes(x = pet / prec, y = aet / prec)) +
  geom_point(color = "grey43") +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_line(data = budyko_data, aes(x = aridity, y = evaporation),
            inherit.aes = FALSE, color = "black", linewidth = 0.7, linetype = "solid") +
  geom_point(data= N_lower_quintile_budyko_evap, color = "firebrick") +
  geom_point(data = N_upper_quintile_budyko_evap, color = "springgreen3") +
  theme_classic() +
  scale_x_continuous(limits = c(0, NA))+
  xlim(0, 7) +
  ylim(0, 5) +
  labs(
    x = "Aridity Index (PET / P)",
    y = " Evaporative Index (AET / P)",
    main = "Budyko Curve and Fluxnet Sites"
  )

plot(gg_budyko_img_aridity)








#---------------------------------------------------------------------------------------------------
## gg2 Budyko by Geco ---------
gg2 <- N_annual_means_df |>  #former gg2 and adf
  ggplot(
    aes(
      x = pet/prec,
      y = aet/prec
    )
  ) +
  geom_point(color = "tomato") +
  gghighlight(
    sitename %in% c("DE-Hai", "US-Ton", "FI-Hyy", "US-ICh", "AU-How"),
    label_key = sitename,
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "grey40")
  ) +
  geom_label(aes(label = sitename),
             hjust = 1, vjust = 1, fill = "white", colour = "black", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() +
  xlim(0, 3) +
  ylim(0, 2.5) +
  labs(
    x = expression(paste("Aridity Index")),
    y = expression(paste("Evaporative Index"))
  )

plot_grid(
  gg1,
  gg2,
  labels = c("a", "b")
)
plot()

ggsave(here::here("~/flx_waterbalance/data/budyko.png"), width = 8, height = 3.5)
