source(here("Scripts", "process_results.R"))

# Create plot for survival across models
ggplot(df.survival, aes(x = Model, y = Mean, color = Model)) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower_CrI, ymax = Upper_CrI), width = 0.2, size = 1, show.legend = FALSE) + #credible interavals
  labs(x = bquote(bold("Model")), y = expression(bold("Mean"~"Phi"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))  # Rotate x-axis labels

# Create plot for gam across models
ggplot(df.gam, aes(x = Model, y = Mean, color = Model)) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower_CrI, ymax = Upper_CrI), width = 0.2, size = 1, show.legend = FALSE) + #credible interavals
  labs(x = bquote(bold("Model")), y = expression(bold("Mean"~"Gamma"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))  # Rotate x-axis labels
