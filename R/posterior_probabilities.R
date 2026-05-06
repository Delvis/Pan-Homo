library(tidybayes)
library(ggplot2)
library(dplyr)
library(glue)

# 1. Extract Draws from the existing fit_main object
draws <- fit_main %>%
  spread_draws(b_Intercept)

# 2. Calculate probabilities for labels
p_70 <- mean(draws$b_Intercept < 7.0) * 100
p_75 <- mean(draws$b_Intercept < 7.5) * 100

# 3. Visualization
p <- ggplot(draws, aes(x = b_Intercept)) +
  geom_density(fill = "#ecf0f1", color = "#ecf0f1") +

  # Shaded Probabilities: Calculated on-the-fly
  stat_density(geom = "area", aes(fill = after_stat(x < 7.5)), alpha = 1) +
  stat_density(geom = "area", aes(fill = after_stat(x < 7.0)), alpha = 0.7) +

  # Threshold Lines
  geom_vline(xintercept = c(7), linetype = "dashed", color = "#f39c12") +
  geom_vline(xintercept = c(7.5), linetype = "dashed", color = "#f1c40f") +

  # Aligned Labels: vjust=2 keeps them both at the same height at the top
  annotate("text", x = 7.0, y = Inf, label = glue("<7.0 Ma\n{round(p_70, 1)}%"),
           vjust = 8, hjust = 1.2, color = "#f39c12", fontface = "bold", size = 5) +

  annotate("text", x = 7.5, y = Inf, label = glue("<7.5 Ma\n{round(p_75, 1)}%"),
           vjust = 8, hjust = -0.2, color = "#f1c40f", fontface = "bold", size = 5) +

  # Styling
  scale_fill_manual(values = c("TRUE" = "#f39c12", "FALSE" = "#ecf0f1"), guide = "none") +
  scale_x_continuous(limits = c(4, 12), breaks = seq(4, 12, 1)) +
  labs(x = "Divergence Time (Ma)", y = "Posterior Density") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
  )

# 4. Export to the updated directory
if(!dir.exists("figures/output")) dir.create("figures/output", recursive = TRUE)

ggsave("figures/output/Figure_6.png", p, width = 16, height = 9,
       dpi = 360, bg = "white", scale = 0.5)
