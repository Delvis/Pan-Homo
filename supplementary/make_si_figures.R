# --- supplementary/make_si_figures.R ---
# Unified SI Figure Generation Script - MAXIMIZED FOR LARGE DATASETS
# Updated: Explicit NA filtering and aesthetic consistency.

library(ggplot2)
library(dplyr)
library(ggridges)
library(tidybayes)
library(ggdist)
library(scales)
library(glue)
library(tidyr)
library(stringr)
library(brms)

# Load the project foundation
source("R/prepare_data.R")

# Ensure SI output directory exists
if(!dir.exists("supplementary/output")) dir.create("supplementary/output", recursive = TRUE)

# ------------------------------------------------------------------------------
# SI FIGURE 1: Density Plots of Min/Estimation/Max
# ------------------------------------------------------------------------------
message("Generating SI Figure 1...")

dens3 <- HomoPanDivergences %>%
  select(Min, ESTIMATION, Max) %>%
  pivot_longer(cols = everything(), names_to = "key", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(key = factor(key, levels = c('Min', 'ESTIMATION', 'Max')))

vcols <- hue_pal()(3)

si_fig1 <- ggplot(dens3, aes(x = value, color = key)) +
  geom_line(stat = "density") +
  geom_vline(xintercept = median(HomoPanDivergences$Min, na.rm = TRUE), col = vcols[1], linetype = "dashed") +
  geom_vline(xintercept = median(HomoPanDivergences$ESTIMATION, na.rm = TRUE), col = vcols[2], linetype = "dashed") +
  geom_vline(xintercept = median(HomoPanDivergences$Max, na.rm = TRUE), col = vcols[3], linetype = "dashed") +
  scale_color_discrete(name = "Density curves", labels = c("Min range", "Divergence estimate", "Max range")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(name = "Mega-annum (Ma)", limits = c(0, 16), breaks = seq(0, 16, by = 1)) +
  theme_classic() +
  theme(legend.position = c(0.85, 0.7))

ggsave("supplementary/output/SI_Figure_1.png", si_fig1, width = 5, height = 2, dpi = 360, bg = 'white')

# ------------------------------------------------------------------------------
# SI BMA FOREST PLOTS (NO TITLES, MAX SPACE)
# ------------------------------------------------------------------------------

plot_si_bma <- function(model_path, x_lims = c(1, 16.2)) {

  if(!file.exists(model_path)) {
    warning(paste("Model file not found:", model_path))
    return(NULL)
  }

  mod <- readRDS(model_path)

  # Extract draws
  study.draws <- spread_draws(mod, r_Author[Author,], b_Intercept) %>%
    mutate(b_Intercept = r_Author + b_Intercept)

  pooled.effect.draws <- spread_draws(mod, b_Intercept) %>%
    mutate(Author = "Pooled Effect")

  # SAFETY: Combine and explicitly filter out any NAs
  forest.data <- bind_rows(study.draws, pooled.effect.draws) %>%
    ungroup() %>%
    filter(!is.na(b_Intercept)) %>%
    mutate(Author = stringr::str_replace_all(Author, "[.]", " ")) %>%
    mutate(Author = reorder(Author, b_Intercept))

  forest.data.summary <- forest.data %>%
    group_by(Author) %>%
    mean_qi(b_Intercept)

  p <- ggplot(aes(x = b_Intercept,
                  y = relevel(as_factor(Author), "Pooled Effect", after = Inf),
                  fill = after_stat(x)), data = forest.data) +
    # White background for labels
    annotate("rect", xmin = 15.1, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white") +
    # Reference lines
    geom_vline(xintercept = fixef(mod)[1, 1], color = "grey23", linewidth = 1, linetype = 4) +
    geom_vline(xintercept = fixef(mod)[1, 3:4], color = "grey", linewidth = 0.5, linetype = 2) +
    # Density ridges
    geom_density_ridges_gradient(rel_min_height = 0.01, col = NA, scale = 1.5) +
    scale_fill_viridis_c(guide = "none") +
    # Error bars
    geom_pointinterval(data = forest.data.summary, aes(xmin = .lower, xmax = .upper), size = 1) +
    # Numerical Labels
    geom_text(data = mutate_if(forest.data.summary, is.numeric, round, 2),
              aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = 16.2),
              hjust = "inward", fontface = "plain", size = 2.5) +
    labs(x = "Standardized Mean Divergence (Ma)", y = NULL) +
    scale_x_continuous(limits = x_lims, breaks = seq(0, 16, 2), expand = c(0.01, 0)) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6)) # Smaller text for SI crowded plots

  return(p)
}

si_models <- list(
  list(path = "models/bma_subset_FullData.rds", file = "SI_Figure_2.png", h = 14),
  list(path = "models/bma_subset_A.rds",        file = "SI_Figure_3.png", h = 12),
  list(path = "models/bma_subset_B.rds",        file = "SI_Figure_4.png", h = 10),
  list(path = "models/bma_subset_C.rds",        file = "SI_Figure_5.png", h = 8)
)

for(m in si_models) {
  message(glue("Processing {m$file}..."))
  p <- plot_si_bma(m$path)
  if(!is.null(p)) {
    ggsave(paste0("supplementary/output/", m$file), p, width = 8, height = m$h, dpi = 360, bg = 'white')
  }
}

message("All SI Figures have been exported successfully to supplementary/output/")
