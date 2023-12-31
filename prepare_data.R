library(sjPlot) # HTML Summary model tables
library(ggplot2) # for graphics
library(zoo) # has as.yearmon() function

HomoPanDivergences <- read.csv("./HomoPanDivergences.csv") # import dataset

# Combine year and month variables into a single continuous "date of publication" variable

HomoPanDivergences$date <- as.yearmon(
  paste(HomoPanDivergences$Year, HomoPanDivergences$Month), "%Y %b" # see ?as.yearmon
  )

# Remove NAs

HomoPanDivergences <- HomoPanDivergences[-which(is.na(HomoPanDivergences$ESTIMATION)),]

lateMiocene_starts <- 11.63
lateMiocene_ends <- 5.33

FAD_A <- 4.4 # Ar. ramidus or Au. anamensis have LAD = 4.4 and FAD = 4.2, respectively
FAD_B <- 6.2 # Orrorin and Kadabba FADs, are respectively 6.1 and 6.3 (if we accept the Gona's material as Ar. kadabba)
FAD_C <- 7.3 # Sahelanthropus tchadensis, 7.34-7.1 Ma (Lebatard et al. 2010)

# 3 SubSets

HomoPanDivergences_A <- HomoPanDivergences[HomoPanDivergences$ESTIMATION > FAD_A,]
HomoPanDivergences_B <- HomoPanDivergences[HomoPanDivergences$ESTIMATION > FAD_B,]
HomoPanDivergences_C <- HomoPanDivergences[HomoPanDivergences$ESTIMATION > FAD_C,]

# Regression filtered by Sahelanthropus

sahelReg <- HomoPanDivergences[HomoPanDivergences$date > 2002.5,] # Date of publication
sahelReg <- sahelReg[which(sahelReg$ESTIMATION > FAD_C),] # Threshold based on cosmogenic dating (Lebatard et al. 2010)

# Italic for axis
ggyaxis <- expression(paste(italic("Pan/Homo"), " divergence estimates (Ma)"))
bw <- 0.25 # binwidth
ggxaxis <- expression(paste(italic("Pan/Homo"), " divergence (Ma); binwidth = 0.25 Ma"))

HomoPanDivergences$Type <- as.factor(HomoPanDivergences$Material) # Quick change for legend name

# CAREFUL, order is data-dependent, if you added entries to the database, you might need to edit the following lines:

levels(HomoPanDivergences$Type) <- c("others", "others", "genome", "others", "mtDNA", "mtDNA",
                                     "mtDNA", "others", "others", "integrative", "others")

# Regression filtered by genomic divergence studies
genomeReg <- HomoPanDivergences[HomoPanDivergences$Type == 'genome',] # only genome studies

# Prepare dataset to be filtered by both date of publication and associated thresholds:
superFilter <- HomoPanDivergences$date < 1980 | 
  (HomoPanDivergences$date > 1994 & HomoPanDivergences$ESTIMATION < FAD_A) | 
  (HomoPanDivergences$date > 2001 & HomoPanDivergences$ESTIMATION < FAD_B) | 
  (HomoPanDivergences$date > 2002.583 & HomoPanDivergences$ESTIMATION < FAD_C)

# Regression filtered by thresholds-by-date-of-publication *and 1980s onwards*
filteredReg <- HomoPanDivergences[which(!superFilter),]


######################

# FIGURE 1 PRE-PROCESSING

n_obs <- sum(!is.na(HomoPanDivergences$ESTIMATION)) # number of estimates
sdx <- sd(HomoPanDivergences$ESTIMATION)
u_mean <- mean(HomoPanDivergences$ESTIMATION)
ybreaks = seq(0, 30, by = 5) 
dnorm_fun <- function(x) dnorm(x, mean = u_mean, sd = sdx) * n_obs * bw


# FIGURE 2 PRE-PROCESSING

# New dataframe for ggplot boxplot figure

newdf <- HomoPanDivergences
newdf$Filtered <- " (No)"
newdfA <- HomoPanDivergences[which(HomoPanDivergences$Min >= FAD_A | is.na(HomoPanDivergences$Min)),]
newdfA <- newdfA[which(newdfA$ESTIMATION >= FAD_A),]
newdfA$Filtered <- "4.4 Ma"
newdfB <- HomoPanDivergences[which(HomoPanDivergences$Min >= FAD_B | is.na(HomoPanDivergences$Min)),]
newdfB <- newdfB[which(newdfB$ESTIMATION >= FAD_B),]
newdfB$Filtered <- "6.2 Ma"
newdfC <- HomoPanDivergences[which(HomoPanDivergences$Min >= FAD_C | is.na(HomoPanDivergences$Min)),]
newdfC <- newdfC[which(newdfC$ESTIMATION >= FAD_C),]
newdfC$Filtered <- "7.3 Ma"

boxdf <- rbind(newdf, newdfA, newdfB, newdfC) # COMBINE INTO A SINGLE DATA SET


# For meta analysis, and also for figures with error-intervals

# The most precise studies will have more weight in our calculation of the
# average divergence estimate (= effect, in a meta-analysis sense).
# if the metric is SE, we extract v = SE^2
# else the metric is bayesian 95% credibility intervals, we need to apply some standardization technique first


HomoPanDivergences$v <- ifelse(
  HomoPanDivergences$ErrorMetric == "Bayes", # rule selects
  (HomoPanDivergences$Max - HomoPanDivergences$Min)/3.92, # assuming Gaussian shape of the posterior distribution, convert credible interval into SE
  # else: SE
  (HomoPanDivergences$Max - HomoPanDivergences$Min)/2 # Get Standard Error from mean of HomoPanDivergencesx and min (since they are symmetrical)
)

HomoPanDivergences$vMax <- HomoPanDivergences$ESTIMATION + HomoPanDivergences$v
HomoPanDivergences$vMin <- HomoPanDivergences$ESTIMATION - HomoPanDivergences$v