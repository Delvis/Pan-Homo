# --- R/analysis_meta.R ---
# Core Bayesian Meta-Analysis (BMA) for Pan/Homo Divergence

library(brms)
library(tidybayes)
library(dplyr)
library(glue)
library(stringr)

source("R/prepare_data.R")

# --- 1. SETTINGS & PRIORS ---
# Formula: Accounting for study-level variation (Random Effects)
formula_bma <- bf(ESTIMATION | se(v) ~ 1 + (1|Author))

# Priors used in final manuscript (Figure 5)
# Based on historical averages: mu=8, sd=0.7 for the intercept
main_priors <- c(
  prior(normal(8, 0.7), class = "Intercept"),
  prior(student_t(3, 0, 2.5), class = "sd")
)

# Ensure models directory exists
if(!dir.exists("models")) dir.create("models")

# --- 2. HELPER FUNCTION ---
# Process data and run BMA if the file doesn't already exist
run_bma_model <- function(dat, name, file_path) {
  if(file.exists(file_path)) {
    message(glue("Model '{name}' already exists. Skipping..."))
    return(NULL)
  }

  # Standardize Author label and remove missing errors
  dat_clean <- dat %>%
    mutate(Author = paste(Reference, Year)) %>%
    filter(!is.na(v))

  message(glue("Running BMA for {name} with {nrow(dat_clean)} studies..."))

  fit <- brm(
    formula = formula_bma,
    family  = gaussian(),
    prior   = main_priors,
    data    = dat_clean,
    cores   = 4,
    seed    = 1,
    iter    = 4000,           # Higher iterations for better ESS
    warmup  = 2000,
    control = list(adapt_delta = 0.99) # Extra precision to prevent warnings
  )

  saveRDS(fit, file_path)
  return(fit)
}

# --- 3. EXECUTE MODELS ---

# 3a. Main Genomic Model (Figure 5)
ma_genomic <- genomeReg %>% filter(ESTIMATION > 4.5)
fit_main <- run_bma_model(ma_genomic, "Main Genomic", "models/bma_final_genomic.rds")

# 3b. SI Models (Figures 2-5)
run_bma_model(HomoPanDivergences,   "Full Dataset", "models/bma_subset_FullData.rds")
run_bma_model(HomoPanDivergences_A, "Filter A",      "models/bma_subset_A.rds")
run_bma_model(HomoPanDivergences_B, "Filter B",      "models/bma_subset_B.rds")
run_bma_model(HomoPanDivergences_C, "Filter C",      "models/bma_subset_C.rds")

# --- 4. QUICK SUMMARY ---
if(!is.null(fit_main)) summary(fit_main)
