source(here("Scripts", "results", "process_results.R"))

# Define custom order of levels
custom_order <- c("Best Fit", "Second Best Fit", "Model Average")

# Convert "Model" variable to factor with custom levels
df.survival$Model <- factor(df.survival$Model, levels = custom_order)
df.gam$Model <- factor(df.survival$Model, levels = custom_order)


# Create plot for survival across models
ggplot(df.survival, aes(x = Model, y = Mean)) +
  geom_point(size = 3, show.legend = FALSE, color = "red2") +
  geom_errorbar(aes(ymin = Lower_CrI, ymax = Upper_CrI), width = 0.1, linewidth = 1, color = "red2", show.legend = FALSE) + #credible interavals
  labs(x = bquote(plain("Models")), y = expression(plain(paste("Mean ", phi, " (95% CrI)")))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) +
  scale_y_continuous(limits = c(0,1))


# Create plot for production across models
ggplot(df.gam, aes(x = Model, y = Mean, color = "red2")) +
  geom_point(size = 3, show.legend = FALSE, color = "red2") +
  geom_errorbar(aes(ymin = Lower_CrI, ymax = Upper_CrI), width = 0.1, linewidth = 1, show.legend = FALSE, color = "red2") + #credible interavals
  labs(x = bquote(plain("Models")), y = expression(plain(paste("Mean ", gamma, " (95% CrI)")))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
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
  labs(x = expression(plain("NPGO")), y = expression(plain(paste("Mean ", phi, " (95% CrI)")))) +
  theme(axis.text.x = element_text(hjust = 1, size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) +
  scale_y_continuous(limits = c(0,1))

#############################################################
# Plot random deviates by year
years.pred.hist <- seq(1996, 2013, by = 1)
years.pred.pres <- c(2022, 2023)
years.pred <- c(years.pred.hist, years.pred.pres)
deviates_year <- list(years = years.pred, Mean = out.bestmodel, Lower_CrI = Cr.pred[1,], Upper_CrI = Cr.pred[2,])


# Create the plot using ggplot2
ggplot(df_pred, aes(x = NPGO, y = Mean)) +
  geom_line(size = 1, color = "red") + # Line connecting mean values
  geom_ribbon(aes(ymin = Lower_CrI, ymax = Upper_CrI), fill = "#FF6F61", alpha = 0.3) + # Uncertainty ribbon
  labs(x = expression(plain("Year")), y = expression(plain(paste("Mean ", phi, " (95% CrI)")))) +
  theme(axis.text.x = element_text(hjust = 1, size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) +
  scale_y_continuous(limits = c(0,1))
