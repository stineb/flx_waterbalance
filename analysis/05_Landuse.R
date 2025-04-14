ggplot(clean_budyko, aes(x = igbp_land_use, y = epsilon_deviation, fill = igbp_land_use)) +
  geom_boxplot() +
  labs(title = "ε′ Deviation by Land Use Type", x = "IGBP Land Use", y = "ε′") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
