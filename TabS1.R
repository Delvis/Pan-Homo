# Table S1, summarizes all values

source("prepare_data.R")

library(psych)

obj.tab <- rbind(
  describe(HomoPanDivergences$ESTIMATION),
  describe(HomoPanDivergences_A$ESTIMATION),
  describe(HomoPanDivergences_B$ESTIMATION),
  describe(HomoPanDivergences_C$ESTIMATION),
  describe(sahelReg$ESTIMATION),
  describe(filteredReg$ESTIMATION),
  describe(genomeReg$ESTIMATION)
)

obj.tab <- apply(obj.tab, 2, function(x) round(x,2))
obj.tab <- obj.tab[,-c(1,6)] # removing "vars" and "trimmed"
descriptive_table <- cbind(Filtered = c('No', '4.4 Ma', '6.2 Ma', '7.3 Ma',
                                        'Reg-B', 'Reg-C', 'Reg-D'), obj.tab)

write.table(descriptive_table, file = "descriptive_stats.csv", sep = ",", quote = FALSE, row.names = FALSE)