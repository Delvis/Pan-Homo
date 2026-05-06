goReg <- ggplot(aes(x = date, y = ESTIMATION), data = HomoPanDivergences) +
  xlab("Publication date") + ylab(ggymini)  +
  scale_x_continuous(breaks = round(seq(1965, 2025, by = 5),1), limits = c(1967, 2025)) +
  scale_y_continuous(breaks = round(seq(0, 17, by = 3))) +
  geom_errorbar(aes(ymin = vMin, ymax = vMax), width = 0.75, alpha = 0.1) +
  geom_point(aes(shape = Source, fill = Source, color = Source, size = Source)) +
  scale_fill_manual(values = c('#2c3e50', rep('grey', 4))) +
  scale_color_manual(values = c('#2c3e50', rep('grey', 4))) +
  scale_size_manual(values = c(3, rep(2,4))) +
  geom_smooth(data = oReg, aes(x = date, y = ESTIMATION), fill = "#34495e", col = "#34495e", method = "lm", size = 1.2) +
  scale_shape_manual(values = c(16,rep(16,4))) +
  theme_minimal()
# B
gmtReg <- ggplot(aes(x = date, y = ESTIMATION), data = HomoPanDivergences) +
  xlab("Publication date") + ylab(ggymini)  +
  scale_x_continuous(breaks = round(seq(1965, 2025, by = 5),1), limits = c(1967, 2025)) +
  scale_y_continuous(breaks = round(seq(0, 17, by = 3))) +
  geom_errorbar(aes(ymin = vMin, ymax = vMax), width = 0.75, alpha = 0.1) +
  geom_point(aes(shape = Source, fill = Source, color = Source, size = Source)) +
  scale_fill_manual(values = c('grey','#2980b9', rep('grey', 3))) +
  scale_color_manual(values = c('grey','#2980b9', rep('grey', 3))) +
  scale_size_manual(values = c(2, 3, rep(2,3))) +
  geom_smooth(data = mtReg, aes(x = date, y = ESTIMATION), fill = "#3498db", col = "#3498db", method = "lm", size = 1.2) +
  scale_shape_manual(values = c(16,15,rep(16,3))) +
  theme_minimal()
# C
gnReg <- ggplot(aes(x = date, y = ESTIMATION), data = HomoPanDivergences) +
  xlab("Publication date") + ylab(ggymini)  +
  scale_x_continuous(breaks = round(seq(1965, 2025, by = 5),1), limits = c(1967, 2025)) +
  scale_y_continuous(breaks = round(seq(0, 17, by = 3))) +
  geom_errorbar(aes(ymin = vMin, ymax = vMax), width = 0.75, alpha = 0.1) +
  geom_point(aes(shape = Source, fill = Source, color = Source, size = Source)) +
  scale_fill_manual(values = c(rep('grey', 2), '#8e44ad', rep('grey', 2))) +
  scale_color_manual(values = c(rep('grey', 2), '#8e44ad', rep('grey', 2))) +
  geom_smooth(data = nReg, aes(x = date, y = ESTIMATION), fill = "#9b59b6", col = "#9b59b6", method = "lm", size = 1.2) +
  scale_size_manual(values = c(2,2,3,2,2)) +
  scale_shape_manual(values = c(16,16,25,16,16)) +
  theme_minimal()
# D
ggReg <- ggplot(aes(x = date, y = ESTIMATION), data = HomoPanDivergences) +
  xlab("Publication date") + ylab(ggymini)  +
  scale_x_continuous(breaks = round(seq(1965, 2025, by = 5),1), limits = c(1967, 2025)) +
  scale_y_continuous(breaks = round(seq(0, 17, by = 3))) +
  geom_errorbar(aes(ymin = vMin, ymax = vMax), width = 0.75, alpha = 0.1) +
  geom_point(aes(shape = Source, fill = Source, color = Source, size = Source)) +
  scale_fill_manual(values = c(rep('grey', 3),'#27ae60','white')) +
  scale_color_manual(values = c(rep('grey', 3),'#27ae60','#27ae60')) +
  geom_smooth(data = genomeReg, aes(x = date, y = ESTIMATION), fill = "#2ecc71", col = "#2ecc71", method = "lm", size = 1.2) +
  scale_shape_manual(values = c(rep(16,3),17,24)) +
  scale_size_manual(values = c(rep(2,3), rep(3,2))) +
  theme_minimal()
