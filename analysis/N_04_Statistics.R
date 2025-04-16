


### CTI and ε′----------###

### Regressionline with Scatterplot

ggplot(N_merged_characteristics_df, aes(x = cti, y = epsilon_deviation)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "CTI vs. ε′ (Deviation from Budyko Curve)",
       x = "Compound Topographic Index (CTI)",
       y = "ε′ (Deviation)") +
  theme_minimal()
