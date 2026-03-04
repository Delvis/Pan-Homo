# --- R/functions_utility.R ---

# 1. Plotting Aesthetics (Standardize the look of your GitHub)
# Use these instead of re-typing long expressions
label_pan_homo_ma <- expression(paste(italic("Pan/Homo"), " divergence estimates (Ma)"))
label_binwidth_025 <- expression(paste(italic("Pan/Homo"), " divergence (Ma); binwidth = 0.25 Ma"))

# Standard theme for the project to ensure all SI and Main figures look like they belong together
theme_publication <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", color = "#2c3e50"),
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "italic"),
      legend.position = "bottom"
    )
}

# 2. Mathematical Functions
# This was previously floating in Figure_1.R and GENERATE_Figure_1_and_2.R
calc_density_curve <- function(x, data_mean, data_sd, n, binwidth) {
  dnorm(x, mean = data_mean, sd = data_sd) * n * binwidth
}

# 3. Data Helper Functions
# A quick helper to format "Author Year" for Meta-Analysis and Forest Plots
format_author_year <- function(ref, year) {
  paste0(ref, " (", year, ")")
}


# 4. Standard labels
ggyaxis <- expression(paste(italic("Pan/Homo"), " divergence estimates (Ma)"))
ggxaxis <- expression(paste(italic("Pan/Homo"), " divergence (Ma); binwidth = 0.25 Ma"))
bw <- 0.25

# 5. A flexible function for the density curve
# This replaces the need for global u_mean, sdx variables
get_dnorm_curve <- function(x, data_vector, binwidth = 0.25) {
  u  <- mean(data_vector, na.rm = TRUE)
  s  <- sd(data_vector, na.rm = TRUE)
  n  <- sum(!is.na(data_vector))
  dnorm(x, mean = u, sd = s) * n * binwidth
}
