source(here("Scripts", "results", "process_results.R"))

# Define custom order of levels
custom_order <- c("Top", "Second/Null", "Model Avg")

# Convert "Model" variable to factor with custom levels
df.survival$Model <- factor(df.survival$Model, levels = custom_order)
df.gam$Model <- factor(df.survival$Model, levels = custom_order)


# Create plot for survival across models
ggplot(df.survival, aes(x = Model, y = Mean, color = "red3")) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower_CrI, ymax = Upper_CrI), width = 0.1, size = 1, show.legend = FALSE) + #credible interavals
  labs(x = bquote(bold("Model")), y = expression(bold("Mean Breeding Success (95% Credible Intervals)"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) +
  scale_y_continuous(limits = c(0,1))


# Create plot for survival across models
ggplot(df.gam, aes(x = Model, y = Mean, color = "red3")) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower_CrI, ymax = Upper_CrI), width = 0.1, size = 1, show.legend = FALSE) + #credible interavals
  labs(x = bquote(bold("Model")), y = expression(bold("Mean Fledgling Production (95% Credible Intervals)"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) +
  scale_y_continuous(limits = c(0,1))


# Plot predicted phi by NPGO 
NPGO.pred <- seq(-3, 3, by = 0.25)

S.pred <- matrix(NA, nrow = nrow(out.bestmodel), ncol = length(NPGO.pred))
for (s in 1:nrow(S.pred)) {
  for (v in 1:ncol(S.pred)) {
    S.pred[s, v] <- 1 / (1 + exp(-(out.bestmodel[s, 47] + out.bestmodel[s, 51] * NPGO.pred[v])))
  }
} 
means.pred <- apply(S.pred, 2, mean)
Cr.pred <- apply(S.pred, 2, function(x) { quantile(x, probs = c(0.025, 0.975)) })

# Create data frame for predicted values and intervals
df_pred <- data.frame(NPGO = NPGO.pred, Mean = means.pred, Lower_CrI = Cr.pred[1,], Upper_CrI = Cr.pred[2,])

# Create the plot using ggplot2
ggplot(df_pred, aes(x = NPGO, y = Mean)) +
  geom_line(size = 1, color = "red") + # Line connecting mean values
  geom_ribbon(aes(ymin = Lower_CrI, ymax = Upper_CrI), fill = "#FF6F61", alpha = 0.3) + # Uncertainty ribbon
  labs(x = expression(bold("NPGO")), y = expression(bold("Mean Breeding Success (95% Credible Intervals"))) +
  theme(axis.text.x = element_text(hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) +
  scale_y_continuous(limits = c(0,1))
