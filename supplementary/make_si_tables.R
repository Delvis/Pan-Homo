# --- supplementary/make_si_table.R ---
# Table S1: Summary of Descriptive and Bayesian Estimates

library(psych)
library(dplyr)
library(brms)

# Load data subsets (A, B, C, genomeReg, etc.)
source("R/prepare_data.R")

# Ensure output directory exists
if(!dir.exists("supplementary/output")) dir.create("supplementary/output", recursive = TRUE)

# ------------------------------------------------------------------------------
# PART 1: DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------
message("Calculating descriptive statistics...")

obj.tab <- rbind(
  describe(HomoPanDivergences$ESTIMATION),
  describe(HomoPanDivergences_A$ESTIMATION),
  describe(HomoPanDivergences_B$ESTIMATION),
  describe(HomoPanDivergences_C$ESTIMATION),
  describe(oReg$ESTIMATION),
  describe(mtReg$ESTIMATION),
  describe(nReg$ESTIMATION),
  describe(genomeReg$ESTIMATION)
)

# Clean and round
obj.tab <- apply(obj.tab, 2, function(x) round(x, 2))
obj.tab <- obj.tab[, -c(1, 6)] # Removing "vars" and "trimmed" columns

# Create descriptive labels
row_labels <- c(
  'Full Dataset (No Filter)',
  'Filter A (>4.4 Ma)',
  'Filter B (>6.2 Ma)',
  'Filter C (>7.2 Ma)',
  'Miscellaneous Source',
  'mtDNA Source',
  'Nuclear DNA Source',
  'Genomic/Integrative'
)

descriptive_table <- cbind(Subset = row_labels, as.data.frame(obj.tab))

# ------------------------------------------------------------------------------
# PART 2: BAYESIAN POOLED EFFECTS (Optional but recommended)
# Extracts the "Grand Mean" from the models we saved in R/analysis_meta.R
# ------------------------------------------------------------------------------
message("Extracting Bayesian results from saved models...")

get_bma_summary <- function(path, label) {
  if(file.exists(path)) {
    mod <- readRDS(path)
    # Get fixed effects (Intercept)
    fe <- fixef(mod)
    data.frame(
      Subset = label,
      Bayes_Mean = round(fe[1, 1], 2),
      CrI_Lower  = round(fe[1, 3], 2),
      CrI_Upper  = round(fe[1, 4], 2)
    )
  } else {
    return(NULL)
  }
}

bma_results <- bind_rows(
  get_bma_summary("models/bma_subset_FullData.rds", "Full Dataset (No Filter)"),
  get_bma_summary("models/bma_subset_A.rds",        "Filter A (>4.4 Ma)"),
  get_bma_summary("models/bma_subset_B.rds",        "Filter B (>6.2 Ma)"),
  get_bma_summary("models/bma_subset_C.rds",        "Filter C (>7.2 Ma)"),
  get_bma_summary("models/bma_final_genomic.rds",   "Main Genomic (>4.5 Ma)")
)

# ------------------------------------------------------------------------------
# PART 3: EXPORT
# ------------------------------------------------------------------------------

# Save Descriptive Table
write.csv(descriptive_table, "supplementary/output/Table_S1_Descriptive.csv", row.names = FALSE)

# Save Bayesian Summary
if(nrow(bma_results) > 0) {
  write.csv(bma_results, "supplementary/output/Table_S2_Bayesian_Estimates.csv", row.names = FALSE)
  message("Table S1 and Table S2 exported to supplementary/output/")
} else {
  message("Table S1 exported. No Bayesian models found to create Table S2.")
}
