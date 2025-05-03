library(ggplot2)

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

S.pred <- matrix(NA, nrow = nrow(second), ncol = length(NPGO.pred))
for (s in 1:nrow(S.pred)) {
  for (v in 1:ncol(S.pred)) {
    S.pred[s, v] <- 1 / (1 + exp(-(second[s, 47] + second[s, 51] * NPGO.pred[v])))
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
Year <- c("1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2022","2023")

out.years.phi <- second[,1:20]
out.years.gamma <- second[,21:40]
out.phi.mean <- second[,47]
out.gamma.mean <- second[,48]

S.phi.pred <- S.gamma.pred <- matrix(NA, nrow = nrow(out.years.phi), ncol = ncol(out.years.phi))
for (s in 1:nrow(S.phi.pred)) {
  for (t in 1:ncol(S.phi.pred)) {
    S.phi.pred[s, t] <- 1 / (1 + exp(-(out.phi.mean[s] + out.years.phi[s, t])))
    S.gamma.pred[s, t] <- 1 / (1 + exp(-(out.gamma.mean[s] + out.years.gamma[s, t])))
  }
} 

means.phi <- apply(S.phi.pred, 2, mean)
means.gamma <- apply(S.gamma.pred, 2, mean)
Cr.phi <- apply(S.phi.pred, 2, function(x) { quantile(x, probs = c(0.025, 0.975)) })
Cr.gamma <- apply(S.gamma.pred, 2, function(x) { quantile(x, probs = c(0.025, 0.975)) })

df.yr.phi <- data.frame(Year,means.phi,lower.phi = Cr.phi[1,],upper.phi = Cr.phi[2,])
colnames(df.yr.phi) <- c("Year","Mean","Lower","Upper")

df.yr.gamma <- data.frame(Year,means.gamma,lower.gamma = Cr.gamma[1,],upper.gamma = Cr.gamma[2,])
colnames(df.yr.gamma) <- c("Year","Mean","Lower","Upper")

# Create plot for production across models
phi.years <- ggplot(df.yr.phi, aes(x = Year, y = Mean, color = "red2")) +
  geom_point(size = 3, show.legend = FALSE, color = "red2") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, linewidth = 1, show.legend = FALSE, color = "red2") + #credible interavals
  labs(x = bquote(plain("Year")), y = expression(plain(paste("Mean ", phi, " (95% CrI)")))) +
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) 

gamma.years <- ggplot(df.yr.gamma, aes(x = Year, y = Mean, color = "red2")) +
  geom_point(size = 3, show.legend = FALSE, color = "red2") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1, linewidth = 1, show.legend = FALSE, color = "red2") + #credible interavals
  labs(x = bquote(plain("Year")), y = expression(plain(paste("Mean ", gamma, " (95% CrI)")))) +
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray90")) 
  

phi.years
gamma.years

