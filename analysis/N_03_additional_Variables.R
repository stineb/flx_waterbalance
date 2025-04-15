







N_merged_characteristics_df <- N_budyko_omega_df |>
  dplyr::select(sitename, epsilon_deviation) |>
  left_join(
    N_site_df |> select(sitename, canopy_height, igbp_land_use, whc),
    by="sitename"
  ) |>
  left_join(
    table_merged |> select(sitename, cti),  # Only add CTI and Elevation
    by = "sitename"
)




###LAND COVER------------------------------------------------------
# Filter by type

