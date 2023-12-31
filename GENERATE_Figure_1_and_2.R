source("prepare_data.R")

# Code for Figure 1:

fig1 <- ggplot(data = HomoPanDivergences,
       aes(ESTIMATION, mean = u_mean, sdx = sdx, bw = bw, n_obs = n_obs)) + 
  geom_histogram(breaks = seq(0, 15, by = bw), 
                 fill = "#2c3e50",
                 alpha = 0.75) + 
  stat_function(fun = dnorm_fun, size = 1) +
  labs(x = ggxaxis, y = "# published estimates") +
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1.2, 14)) +
  theme_minimal() +
  geom_vline(xintercept = FAD_A, col = "#e74c3c", linetype = "dashed") +
  geom_vline(xintercept = FAD_B, col = "#e67e22", linetype = "dashed") +
  geom_vline(xintercept = FAD_C, col = "#f1c40f", linetype = "dashed")

# SAVE Figure 1:

ggsave("Figure_1.tiff", fig1, width = 5, height = 2, dpi = "retina",
       bg = 'white', device = grDevices::tiff, type = 'cairo')
# I had to force grDevices and Cairo because the new ragg creates very weird white lines between columns artifact.



# Code for Figure 2:

fig2 <- ggplot(boxdf, aes(x = Filtered, y = ESTIMATION, fill = Filtered)) +
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = lateMiocene_ends, ymax = lateMiocene_starts, alpha = .05) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.2, position = position_jitter(width  = 0.2)) +
  scale_fill_manual(values = c("grey", "#e74c3C", "#e67e22", "#f1c40f")) +
  scale_y_continuous(breaks =  seq(0, 15, 1), name = ggyaxis) +
  scale_x_discrete("Paleontological thresholds") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  geom_hline(yintercept = lateMiocene_ends,
             col = "#2c3e50", size = 1, linetype = "dotted") +
  annotate("text", x = 4.5 , y = 8.5, label = "Late Miocene",
           colour = "#2c3e50", angle = 90, size = 5) +
  geom_hline(yintercept = lateMiocene_starts,
             col = "#2c3e50", size = 1, linetype = "dotted") +
  theme_minimal()

# SAVE Figure 2:

ggsave("Figure_2.tiff", fig2, width = 6.6, height = 3.3, dpi = "retina",
       device = "tiff", bg = 'white')

