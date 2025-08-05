library(mgcv)
library(ggplot2)
library(dplyr)
data <- data.frame(
  elevation_range = c("200-400m", "400-600m", "600-800m", "800-1000m", 
                      "1000-1200m", "1200-1400m", "1400-1600m"),
  species_count = c(170, 185, 203, 246, 321, 155, 184),
  elev_min = c("200", "400", "600", "800", "1000", "1200", "1400"),
  elev_max = c("400", "600", "800", "1000", "1200", "1400", "1600"),
  elevation_mid = c("300", "500", "700", "900", "1100", "1300", "1500"))
data$elevation_mid <- as.numeric(data$elevation_mid)
unique_elev <- length(unique(data$elevation_mid))
cat("number of altitude intervals:", unique_elev, "\n")
safe_k <- min(10, max(3, unique_elev - 1))
cat("k value:", safe_k, "\n")
gam_fit <- gam(species_count ~ s(elevation_mid, k=safe_k),
               data = data,
               family = poisson(link = "log"),
               method = "REML")
new_data <- data.frame(
  elevation_mid = seq(200, 1600, length.out = 200))
pred <- predict(gam_fit, newdata = new_data, type = "link", se.fit = TRUE)
new_data$fit <- exp(pred$fit) 
new_data$lower <- exp(pred$fit - 1.96 * pred$se.fit)
new_data$upper <- exp(pred$fit + 1.96 * pred$se.fit)
ggplot() +
  geom_point(data = data, 
             aes(x = elevation_mid, y = species_count),
             color = "#2b8cbe", alpha = 0.8, shape = 19) +
  geom_ribbon(data = new_data,
              aes(x = elevation_mid, ymin = lower, ymax = upper),
              fill = "#fdbb84", alpha = 0.3) +
  geom_line(data = new_data,
            aes(x = elevation_mid, y = fit),
            color = "#e34a33", linewidth = 1.2)+
  labs(x = "Elevation (m)", y = "Species richness",) +
  scale_x_continuous(breaks = seq(200, 1600, by = 200)) +
  scale_y_continuous(breaks = seq(100, 400, by = 50)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", size = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks.x = element_line(), 
    axis.ticks.y = element_line(), 
    axis.title = element_text(
      family = "Arial",
      size = 14,
      color = "black"))
summary(gam_fit)$r.sq
