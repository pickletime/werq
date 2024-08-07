sample.list <- unique(df.final$SubjectID)

sample.table <- data.frame(matrix(NA, nrow = length(sample.list), ncol = 5))
colnames(sample.table) <- c("sample", "Plate", "% failed", "average distance", "normalized distance")
sample.table$sample <- sample.list

a.plate.table <- data.frame(matrix(NA, nrow = length(plate.list), ncol = 6))
colnames(a.plate.table) <- c("plate BC", "QC.pr", "% failed", "average distance", "normalized distance", "project(s)")
a.plate.table$`plate BC` <- plate.list

df.master.list <- df.final[!df.final$Call %in% uncalled,]


start <- Sys.time()
#sample list, this is actually for the qc pass rate calculation downstream
for(i in 1:length(sample.list)){
  loop.df.subset <- df.final[df.final$SubjectID == sample.list[i],]
  loop.df.subset <- loop.df.subset[!is.na(loop.df.subset$SubjectID),]
  loop.df.subset.qqc <- df.master.list[df.master.list$SubjectID == sample.list[i],]
  #sample.table[i,2] <- paste(unique(loop.df.subset$MasterPlate))
  sample.table[i,2] <- as.numeric(unique(loop.df.subset$MasterPlate)[1])
  sample.table[i,3] <- 100*(1-length(loop.df.subset.qqc$SubjectID)/length(loop.df.subset$SubjectID))
  sample.table[i,4] <- as.numeric(mean(loop.df.subset.qqc$distance, na.rm = T))
  sample.table[i,5] <- as.numeric(mean(loop.df.subset.qqc$distance.norm, na.rm = T))
  print(length(sample.list)-i)
}
  sample.table <- sample.table[!is.na(sample.table$Plate),]
end <- Sys.time()
total.time <- end-start; total.time
paste(ceiling(length(sample.list)/(as.numeric(total.time)*60)), "samples per second")

#adding new columns into the plate table (well, a new plate table)
for(i in 1:length(plate.list)){
  loop.df.subset <- df.final[df.final$MasterPlate == plate.list[i],]
  loop.df.subset.qqc <- df.master.list[df.master.list$MasterPlate == plate.list[i],]
  QCPR <- sample.table[sample.table$Plate == plate.list[i],]
  a.plate.table[i,2] <- 100*(1-(length(QCPR[QCPR$`% failed` > 29.99,1])/length(QCPR$`% failed`)))
  a.plate.table[i,3] <- 100*(1-length(loop.df.subset.qqc$MasterPlate)/length(loop.df.subset$MasterPlate))
  a.plate.table[i,4] <- as.numeric(mean(loop.df.subset.qqc$distance, na.rm = T))
  a.plate.table[i,5] <- as.numeric(mean(loop.df.subset.qqc$distance.norm, na.rm = T))
  if(length(unique(loop.df.subset.qqc$Project)) == 1) {
    a.plate.table[i,6] <- unique(loop.df.subset.qqc$Project)
  } else {
    a.plate.table[i,6] <- unique(loop.df.subset.qqc$Project)[2]  
  }
  print(length(plate.list)-i)
}

a.plate.table$GTrate <- 100-a.plate.table$`% failed`

pca.plate.table <- a.plate.table[,-c(1,3,6)]

pca.plate.table <- scale(pca.plate.table)
corr_matrix <- cor(pca.plate.table)
data.pca <- princomp(corr_matrix)
data.pca.2 <- prcomp(pca.plate.table)
summary(data.pca)
data.pca$loadings[,1:2]

new.pca.table <- cbind(a.plate.table, data.pca.2$x)

library(factoextra)
fviz_pca_var(data.pca, col.var = "black")

fviz_pca_var(data.pca.2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

data.pca$scores

#ggplot(new.pca.table, aes(x = factor(chem), y = PC1, fill = factor(chem))) +
ggplot(new.pca.table, aes(x = factor(`project(s)`), y = -PC1, fill = factor(`project(s)`))) + 
  #scale_fill_brewer(palette="Set3") + 
  geom_boxplot() + 
  xlab("Chemistry") + 
  ylab("PC1") +
  theme(legend.position = "none") +
  #  facet_grid(rows = factor(plate.table$cust)) +
  #geom_violin(trim=FALSE) + 
  stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) 


###comparing time treatments
###comparing time treatments
{
ggplot(new.pca.table[new.pca.table$`project(s)` == project.list[3],], aes(x = factor(notes), y = GTrate, fill = factor(notes))) +
  #scale_fill_brewer(palette="Set3") + 
  geom_boxplot() + 
  xlab("treatment") + 
  ylab("GT rate") +
  ggtitle("IDBD treatment differences") +
  theme(legend.position = "none") +
  #  facet_grid(rows = factor(plate.table$cust)) +
  #geom_violin(trim=FALSE) + 
  stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75))

wilcox.test(new.pca.table$GTrate[new.pca.table$notes == note.list[1]], new.pca.table$GTrate[new.pca.table$notes == note.list[2]]); describeBy(new.pca.table$GTrate, group = new.pca.table$notes)
}
###comparing time treatments
###comparing time treatments

###comparing mags for 49.274
###comparing mags for 49.274
{
  ggplot(new.pca.table[new.pca.table$`project(s)` == project.list[4],], aes(x = factor(mag), y = GTrate, fill = factor(mag))) +
    #scale_fill_brewer(palette="Set3") + 
    geom_boxplot() + 
    xlab("magnet") + 
    ylab("GT rate") +
    ggtitle("IDBD performance between magnets") +
    theme(legend.position = "none") +
    #  facet_grid(rows = factor(plate.table$cust)) +
    #geom_violin(trim=FALSE) + 
    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75))
  dunnTest(new.pca.table$GTrate[new.pca.table$`project(s)` == project.list[4]] ~ new.pca.table$mag[new.pca.table$`project(s)` == project.list[4]], method = "holm")[["dtres"]]; describeBy(new.pca.table$GTrate[new.pca.table$`project(s)` == project.list[4]], group = new.pca.table$mag[new.pca.table$`project(s)` == project.list[4]])
  wilcox.test(new.pca.table$GTrate[new.pca.table$notes == note.list[1]], new.pca.table$GTrate[new.pca.table$notes == note.list[2]]); describeBy(new.pca.table$GTrate, group = new.pca.table$notes)
}
###comparing mags for 49.274
###comparing mags for 49.274

###comparing magsep for 49.274 & 49.207
###comparing magsep for 49.274 & 49.207
{
  new.proj.list <- project.list[c(1,4)]
  
  mag.pca.plate.table <- new.pca.table[new.pca.table$`project(s)` %in% new.proj.list,]
  mag.pca.plate.table <- mag.pca.plate.table[-c(1:4),]
  mag.pca.plate.table$agowa[mag.pca.plate.table$mag %in% c("1D","3D")] <- "1 & 3" 
  mag.pca.plate.table$agowa[mag.pca.plate.table$mag %in% c("2D","4D")] <- "2 & 4"
  ggplot(mag.pca.plate.table, aes(x = factor(agowa), y = GTrate, fill = factor(agowa))) +
    #scale_fill_brewer(palette="Set3") + 
    geom_boxplot() + 
    xlab("magnet") + 
    ylab("GT rate") +
    ggtitle("IDBD performance between magnet sets") +
    theme(legend.position = "none") +
  #  facet_grid(rows = factor(mag.pca.plate.table$`project(s)`)) +
    #geom_violin(trim=FALSE) + 
    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75))
  dunnTest(new.pca.table$GTrate[new.pca.table$`project(s)` == project.list[4]] ~ new.pca.table$mag[new.pca.table$`project(s)` == project.list[4]], method = "holm")[["dtres"]]; describeBy(new.pca.table$GTrate[new.pca.table$`project(s)` == project.list[4]], group = new.pca.table$mag[new.pca.table$`project(s)` == project.list[4]])
  wilcox.test(new.pca.table$GTrate[new.pca.table$notes == note.list[1]], new.pca.table$GTrate[new.pca.table$notes == note.list[2]]); describeBy(new.pca.table$GTrate, group = new.pca.table$notes)
}
###comparing magsep for 49.274 & 49.207
###comparing magsep for 49.274 & 49.207

###comparing D set mags
###comparing D set mags
{
  d.pca.plate.table <- new.pca.table[-c(1:4),]
  d.pca.plate.table$agowa[d.pca.plate.table$mag %in% c("1D","3D")] <- "1 & 3" 
  d.pca.plate.table$agowa[d.pca.plate.table$mag %in% c("2D","4D")] <- "2 & 4"
  ggplot(d.pca.plate.table, aes(x = factor(mag), y = GTrate, fill = factor(mag))) +
    #scale_fill_brewer(palette="Set3") + 
    geom_boxplot() + 
    xlab("magnet") + 
    ylab("GT rate") +
    ggtitle("IDBD performance between magnets") +
    theme(legend.position = "none") +
    #  facet_grid(rows = factor(plate.table$cust)) +
    #geom_violin(trim=FALSE) + 
    stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75))
  dunnTest(new.pca.table$GTrate[new.pca.table$`project(s)` == project.list[4]] ~ new.pca.table$mag[new.pca.table$`project(s)` == project.list[4]], method = "holm")[["dtres"]]; describeBy(new.pca.table$GTrate[new.pca.table$`project(s)` == project.list[4]], group = new.pca.table$mag[new.pca.table$`project(s)` == project.list[4]])
  wilcox.test(new.pca.table$GTrate[new.pca.table$notes == note.list[1]], new.pca.table$GTrate[new.pca.table$notes == note.list[2]]); describeBy(new.pca.table$GTrate, group = new.pca.table$notes)
}
###comparing D set mags
###comparing D set mags


dunnTest(new.pca.table$GTrate ~ new.pca.table$notes, method = "holm")[["dtres"]]; describeBy(new.pca.table$GTrate, group = new.pca.table$notes)
wilcox.test(new.pca.table$PC1[new.pca.table$chem == "IDBD"], new.pca.table$PC1[new.pca.table$chem == "Mag mini"]); describeBy(new.pca.table$PC1, group = new.pca.table$chem)
