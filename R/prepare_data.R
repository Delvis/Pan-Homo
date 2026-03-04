# --- Libraries ---
library(sjPlot)
library(ggplot2)
library(zoo)
library(dplyr) # Adding dplyr for safer data manipulation

source("R/functions_utility.R")

# --- 1. Load & Basic Clean ---
# Using relative path from project root
HomoPanDivergences <- read.csv("data/HomoPanDivergences.csv")
colnames(HomoPanDivergences)[1] <- "Reference" # Consistency fix

# Remove NAs and convert Date
HomoPanDivergences <- HomoPanDivergences %>%
  filter(!is.na(ESTIMATION)) %>%
  mutate(date = as.yearmon(paste(Year, Month), "%Y %b"))

# --- 2. Define Scientific Constants ---
lateMiocene_starts <- 11.63
lateMiocene_ends <- 5.33

# Paleontological Thresholds (Fossil Arrival Dates)
FAD_A <- 4.4 # Ar. ramidus / Au. anamensis
FAD_B <- 6.2 # Orrorin / Ar. kadabba
FAD_C <- 7.2 # Sahelanthropus

# Publication Milestones (Dates of major fossil announcements)
PUB_A <- 1994.0 # Ar. ramidus announcement
PUB_B <- 2001.0 # Orrorin & Ar. kadabba announcement
PUB_C <- 2002.5 # Sahelanthropus announcement

# --- 3. Robust Material Mapping ---
# We define a lookup table (dictionary) to group the fine-grained materials
# into your 5 main publication categories.

HomoPanDivergences <- HomoPanDivergences %>%
  mutate(Source = case_match(
    Material,
    # Category: Nuclear DNA
    "DNA"                    ~ "Nuclear DNA",

    # Category: Genome
    "genome"                 ~ "Genome",
    "Recalibrated"           ~ "Genome", # They are based on genomic outputs

    # Category: mtDNA
    "mtDNA"                  ~ "mtDNA",
    "mtDNA + DNA"            ~ "mtDNA",
    "mtDNA genome"           ~ "mtDNA",

    # Category: Integrative
    "total evidence dating"  ~ "Integrative",

    # Category: Miscellaneous (The rest)
    "DNA-DNA hybridization"  ~ "Miscellaneous",
    "immunology"             ~ "Miscellaneous",
    "RECALIBRATION"          ~ "Miscellaneous",
    "RNA"                    ~ "Miscellaneous",
    "virogenes"              ~ "Miscellaneous",
    ""                       ~ "Miscellaneous",

    # Safety net: If a new value is added to the CSV later,
    # it defaults to Miscellaneous instead of breaking the script.
    .default                 = "Miscellaneous"
  ))

# Now we set the specific order you want for your plots
HomoPanDivergences$Source <- factor(
  HomoPanDivergences$Source,
  levels = c("Miscellaneous", "mtDNA", "Nuclear DNA", "Genome", "Integrative")
)

# --- 4. Error Metrics Calculation ---
# Standardizing the conversion of Credibility Intervals to Standard Error
HomoPanDivergences <- HomoPanDivergences %>%
  mutate(
    v = ifelse(ErrorMetric == "Bayes",
               (Max - Min) / 3.92,  # Normal distribution assumption
               (Max - Min) / 2),    # SE assumption
    vMax = ESTIMATION + v,
    vMin = ESTIMATION - v,
    logv = log(v)
  )

# --- 5. Creating Subsets (The "Truths") ---

# A: Basic Threshold Subsets
HomoPanDivergences_A <- HomoPanDivergences[HomoPanDivergences$ESTIMATION > FAD_A,]
HomoPanDivergences_B <- HomoPanDivergences[HomoPanDivergences$ESTIMATION > FAD_B,]
HomoPanDivergences_C <- HomoPanDivergences[HomoPanDivergences$ESTIMATION > FAD_C,]

# B: The "Super Filter" (Studies that ignore known fossil constraints at time of pub)
superFilter <- (HomoPanDivergences$date < 1980) |
  (HomoPanDivergences$date > PUB_A & HomoPanDivergences$ESTIMATION < FAD_A) |
  (HomoPanDivergences$date > PUB_B & HomoPanDivergences$ESTIMATION < FAD_B) |
  (HomoPanDivergences$date > PUB_C & HomoPanDivergences$ESTIMATION < FAD_C)

filteredReg <- HomoPanDivergences[!superFilter, ]

# C: Source-based Subsets
oReg      <- HomoPanDivergences[HomoPanDivergences$Source == 'Miscellaneous',]
mtReg     <- HomoPanDivergences[HomoPanDivergences$Source == 'mtDNA',]
nReg      <- HomoPanDivergences[HomoPanDivergences$Source == 'Nuclear DNA',]
genomeReg <- HomoPanDivergences[HomoPanDivergences$Source %in% c('Genome', 'Integrative'),]

# --- 6. Plotting Helpers ---
ggyaxis <- expression(paste(italic("Pan/Homo"), " divergence estimates (Ma)"))
ggxaxis <- expression(paste(italic("Pan/Homo"), " divergence (Ma); binwidth = 0.25 Ma"))
bw <- 0.25

# Global function for Figure 1 density curve
n_obs <- nrow(HomoPanDivergences)
u_mean <- mean(HomoPanDivergences$ESTIMATION)
sdx <- sd(HomoPanDivergences$ESTIMATION)
dnorm_fun <- function(x) dnorm(x, mean = u_mean, sd = sdx) * n_obs * bw
