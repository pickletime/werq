#figures from 3/22/24
quads <- c("A1", "A2", "A3", "A4")
plate.table.magnet <- plate.table.magnet[plate.table.magnet$Mag %in% quads,]


ggplot(plate.table.compare, aes(x = factor(Mag), y = pass.rate, fill = factor(Mag))) + 
  geom_boxplot() +
  ggtitle("IdentiBEAD genotyping rate by magnet (n = 8)", ) + 
  ylab("Plate genotyping rate") +
  xlab("Magnet") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  

ggplot(plate.table.magnet, aes(x = factor(Mag), y = pass.rate, fill = factor(Mag))) + 
  geom_boxplot() +
  ggtitle("Magmini genotyping rate by magnet (n = 49)", ) + 
  ylab("Plate genotyping rate") +
  xlab("Magnet") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))





