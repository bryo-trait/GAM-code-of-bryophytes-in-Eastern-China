library(mgcv)
library(ggplot2)
data <- read.csv("Mt. Daiyun.csv")
elevation <- data$elevation
species_count<- data$species_richness
unique_elev <- length(unique(data$elevation))
print(paste("number of altitude intervals:", unique_elev))
safe_k <- min(10, max(3, unique_elev - 1))
gam_fit <- gam(species_richness ~ s(elevation, k = safe_k), 
               data = data, 
               family = poisson(), 
               method = "REML")
new_data <- data.frame(
  elevation = seq(900, 1600, length.out = 200))
pred_link <- predict(gam_fit, newdata = new_data, type = "link", se.fit = TRUE)
new_data$fit <- exp(pred_link$fit)
link_lower <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
link_upper <- exp(pred_link$fit + 1.96 * pred_link$se.fit)
inv_link <- family(gam_fit)$linkinv
new_data$predicted_count <- inv_link(pred_link$fit)
new_data$lower_ci <- inv_link(link_lower)
new_data$upper_ci <- inv_link(link_upper)
new_data$is_extrapolation <- new_data$elevation > max(data$elevation)
ggplot() +
  geom_point(data = data, 
             aes(x = elevation, y = species_count),
             color = "#2b8cbe", alpha = 0.8, shape = 19) +
  geom_ribbon(data = new_data,
              aes(x = elevation, ymin = link_lower, ymax = link_upper),
              fill = "#fdbb84", alpha = 0.3) +
  geom_line(data = new_data,
            aes(x = elevation, y = fit),
            color = "#e34a33", linewidth = 1.2)+
  labs(x = "Elevation (m)", y = "Species richness",) +
  scale_x_continuous(breaks = seq(900, 1600, by = 100)) +
  scale_y_continuous(breaks = seq(10, 70, by = 10))+
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
      color = "black"
    )
  )
summary(gam_fit)$r.sq 
