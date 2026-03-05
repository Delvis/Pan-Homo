# --- figures/make_main_figures.R ---
# FINAL REPRODUCTION - FIXED FACTOR ORDERING AND LEGENDS

library(ggplot2)
library(dplyr)
library(cowplot)
library(tidybayes)
library(ggdist)
library(ggridges)
library(glue)
library(forcats)
library(stringr)
library(zoo)

source("R/prepare_data.R")

# GLOBAL FIXES
ggymini <- "Divergence (Ma)"

# Ensure output directory exists
if(!dir.exists("figures/output")) dir.create("figures/output", recursive = TRUE)

# ------------------------------------------------------------------------------
# FIGURE 1: Histogram & Density
# ------------------------------------------------------------------------------
fig1 <- ggplot(data = HomoPanDivergences,
               aes(ESTIMATION, mean = u_mean, sdx = sdx, bw = bw, n_obs = n_obs)) +
  geom_histogram(breaks = seq(0, 15, by = bw), fill = "#2c3e50", alpha = 0.75) +
  stat_function(fun = dnorm_fun, size = 1) +
  labs(x = ggxaxis, y = "# published estimates") +
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1.2, 14)) +
  theme_minimal() +
  geom_vline(xintercept = FAD_A, col = "#e74c3c", linetype = "dashed") +
  geom_vline(xintercept = FAD_B, col = "#e67e22", linetype = "dashed") +
  geom_vline(xintercept = FAD_C, col = "#f1c40f", linetype = "dashed")

ggsave("figures/output/Figure_1.png", fig1, width = 5, height = 2, dpi = 360,
       bg = 'white', type = 'cairo')

# ------------------------------------------------------------------------------
# FIGURE 2: Boxplots (FIXED ORDER)
# ------------------------------------------------------------------------------
# Explicitly set levels so "None" appears first
boxdf$Filtered <- factor(boxdf$Filtered, levels = c("None", "4.4 Ma", "6.2 Ma", "7.2 Ma"))

fig2 <- ggplot(boxdf, aes(x = Filtered, y = ESTIMATION, fill = Filtered)) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = lateMiocene_ends, ymax = lateMiocene_starts, alpha = .05) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.2, position = position_jitter(width  = 0.2)) +
  scale_fill_manual(values = c("grey", "#e74c3C", "#e67e22", "#f1c40f")) +
  scale_y_continuous(breaks =  seq(0, 15, 1), name = ggyaxis) +
  scale_x_discrete("Paleontological thresholds") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  geom_hline(yintercept = lateMiocene_ends, col = "#2c3e50", size = 1, linetype = "dotted") +
  annotate("text", x = 4.5 , y = 8.5, label = "Late Miocene", colour = "#2c3e50", angle = 90, size = 5) +
  geom_hline(yintercept = lateMiocene_starts, col = "#2c3e50", size = 1, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/output/Figure_2.png", fig2, width = 6.6, height = 3.3, dpi = 360, bg = 'white')

# ------------------------------------------------------------------------------
# FIGURE 3: Temporal Trend
# ------------------------------------------------------------------------------
fig3 <- ggplot(aes(x = date, y = ESTIMATION), data = HomoPanDivergences) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 5.33, ymax = 11.63, alpha = .1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 4.2, fill = "#e74c3c", alpha = .5) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1, col = "black", alpha = 0.3) +
  geom_hline(yintercept = 2.8, col = "#e74c3c", size = 1.2) +
  geom_hline(yintercept = FAD_A, col = "#e74c3c", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 5.33, col = "black", size = 1, linetype = "dotted") +
  geom_hline(yintercept = 6.0, col = "#e67e22", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 6.3, col = "#e67e22", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = FAD_C, col = "#f1c40f", size = 1.2, linetype = "dashed") +
  geom_hline(yintercept = 8, col = "#3498db", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 9.5, col = "#3498db", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 9.9, col = "#3498db", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 11.63, col = "black", size = 1, linetype = "dotted") +
  geom_hline(yintercept = 12.5, col = "#7ed6df", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 13, col = "#7ed6df", size = 1, linetype = "dashed") +
  xlab("Publication date") + ylab(ggymini)  +
  scale_x_continuous(breaks = round(seq(1965, 2030, by = 5),1), limits = c(1967, 2029)) +
  scale_y_continuous(breaks = round(seq(0, 17, by = 1))) +
  geom_errorbar(aes(ymin = vMin, ymax = vMax), width = 0.75, linetype = "dotted", alpha = 0.8) +
  geom_point(size = 2) +
  theme_minimal() +
  geom_label(label = "Paranthropus & \n Homo FADs", x = 2028, y = 2.8, color = 'white', fontface = 'bold', fill = "#e74c3C") +
  geom_label(label = "Australopithecus FADs", x = 2026.5, y = FAD_A, color = 'white', fontface = 'bold', fill = "#e74c3C") +
  geom_label(label = "Ardipithecus & \n Orrorin FADs", x = 2028.1, y = FAD_B, color = 'white', fontface = 'bold', fill = "#e67e22") +
  geom_label(label = "Sahelanthropus", x = 2028.1, y = FAD_C, color = 'white', fontface = 'bold', fill = "#f1c40f") +
  geom_label(label = "Chororapithecus", x = 2027.9, y = 8, color = 'white', fontface = 'bold', fill = "#3498db") +
  geom_label(label = "Nakalipithecus & \n Samburupithecus", x = 2027.6, y = 9.7, color = 'white', fontface = 'bold', fill = "#3498db") +
  geom_label(label = "Ngorora ape & \n Otavipithecus", x = 2028.2, y = 12.8, color = 'white', fontface = 'bold', fill = "#7ed6df")

ggsave("figures/output/Figure_3.png", fig3, width = 12, height = 7, dpi = 360, bg = 'white')

# ------------------------------------------------------------------------------
# FIGURE 4: Grid Regressions (Combined logic from your snippet)
# ------------------------------------------------------------------------------
# [Subplots goReg, gmtReg, gnReg, ggReg as per provided definitions]
fig4_combined <- plot_grid(goReg + theme(legend.position = "none"),
                           gmtReg + theme(legend.position = "none"),
                           gnReg + theme(legend.position = "none"),
                           ggReg + theme(legend.position = "none"),
                           ncol = 1, labels = c("A", "B", "C", "D"))

legend_fig4 <- cowplot::get_legend(ggReg + theme(legend.direction = 'horizontal'))
fig4_final <- plot_grid(fig4_combined, legend_fig4, ncol = 1, rel_heights = c(4, 0.1))

ggsave("figures/output/Figure_4.png", fig4_final, width = 9, height = 12, dpi = 360, bg = 'white')

# ------------------------------------------------------------------------------
# FIGURE 5: Meta-Analysis of Phylogenomic Estimates
# ------------------------------------------------------------------------------
library(brms) # required for fixef()

message("Generating Figure 5...")

mod <- readRDS("models/bma_final_genomic.rds")

# Extract draws
study.draws <- spread_draws(mod, r_Author[Author,], b_Intercept) %>%
  mutate(b_Intercept = r_Author + b_Intercept)

pooled.effect.draws <- spread_draws(mod, b_Intercept) %>%
  mutate(Author = "Pooled Effect")

# Combine and reorder
forest.data <- bind_rows(study.draws, pooled.effect.draws) %>%
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>%
  mutate(Author = reorder(Author, b_Intercept))

forest.data.summary <- forest.data %>%
  group_by(Author) %>%
  mean_qi(b_Intercept)

# Build the plot
fig5 <- ggplot(aes(x = b_Intercept,
                   y = relevel(as_factor(Author), "Pooled Effect", after = Inf),
                   fill = after_stat(x)), data = forest.data) +

  # White background for text labels
  annotate("rect", xmin = 15.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +

  # Reference Lines (Updated from 'size' to 'linewidth' to stop warnings)
  geom_vline(xintercept = fixef(mod)[1, 1], color = "grey23", linewidth = 1, linetype = 4) +
  geom_vline(xintercept = fixef(mod)[1, 3:4], color = "grey", linetype = 2) +

  # Density Ridges
  geom_density_ridges_gradient(rel_min_height = 0.01, col = NA, scale = 1.5) +
  scale_fill_viridis_c(guide = "none") +

  # Point Intervals (Updated from 'size' to 'linewidth')
  geom_pointinterval(data = forest.data.summary,
                     aes(xmin = .lower, xmax = .upper),
                     linewidth = 0.8, size = 1) +

  # Numerical Labels
  geom_text(data = mutate_if(forest.data.summary, is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = 16.2),
            hjust = "inward", fontface = "plain") +

  labs(x = "Standardized Mean Divergence (Ma)", y = NULL) +
  scale_x_continuous(limits = c(3.4, 16.2), breaks =  seq(0, 15, 1), expand = c(0, 0)) +
  theme_minimal()

ggsave("figures/output/Figure_5.png", fig5, width = 8, height = 7, dpi = 360, bg = 'white')
