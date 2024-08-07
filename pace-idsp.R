library(psych)
library(FSA)
library(ggstatsplot)
library(ggplot2)
library(psych)

###


ID003.SW$pass.rate <- plate.table$`% failed`[match(ID003.SW$plate.BC, plate.table$`plate BC`, incomparables = NULL)]
ID003.SW$dist <- plate.table$`average distance`[match(ID003.SW$plate.BC, plate.table$`plate BC`)]
ID003.SW$dist.norm <- plate.table$`normalized distance`[match(ID003.SW$plate.BC, plate.table$`plate BC`)]
ID003.SW$chem <- plate.table$chem[match(ID003.SW$plate.BC, plate.table$`plate BC`)]
ID003.SW$lab <- plate.table$lab[match(ID003.SW$plate.BC, plate.table$`plate BC`)]

ID003 <- ID003.SW[!is.na(ID003.SW$pass.rate),]
ID003$pass.rate

dunnTest(ID003$pass.rate ~ ID003$chem, method = "holm")[["dtres"]]; describeBy(ID003$pass.rate, group = ID003$chem)
dunnTest(ID003$pass.rate ~ ID003$spec.cust, method = "holm")[["dtres"]]; describeBy(ID003$pass.rate, group = ID003$spec.cust)
dunnTest(ID003$pass.rate ~ ID003$`Initials.(lysis)`, method = "holm")[["dtres"]]; describeBy(ID003$pass.rate, group = ID003$`Initials.(lysis)`)
dunnTest(ID003$pass.rate ~ ID003$PK.batch, method = "holm")[["dtres"]]; describeBy(ID003$PK.batch, group = ID003$`Initials.(lysis)`)

wilcox.test(ID003$pass.rate[ID003$chem == "SDS"], ID003$pass.rate[ID003$chem == "Tween"]); describeBy(ID003$pass.rate, group = ID003$chem); 
dunnTest(plate.table$`% failed` ~ plate.table$lab, method = "holm")[["dtres"]]; describeBy(plate.table$`% failed`, group = plate.table$lab)

dunnTest(plate.table$`% failed` ~ plate.table$chem.cust, method = "holm")[["dtres"]]; describeBy(plate.table$`% failed`, group = plate.table$chem.cust)
wilcox.test(plate.table$`% failed`[plate.table$chem == "IDBD"], plate.table$`% failed`[plate.table$chem == "Mag mini"]); describeBy(plate.table$`% failed`, group = plate.table$chem);
wilcox.test(plate.table$pass.rate[plate.table$chem == "IDBD"], plate.table$pass.rate[plate.table$chem == "Mag mini"]); describeBy(plate.table$pass.rate, group = plate.table$chem);
wilcox.test(plate.table$`average distance`[plate.table$chem == "IDBD"], plate.table$`average distance`[plate.table$chem == "Mag mini"]); describeBy(plate.table$`average distance`, group = plate.table$chem);
wilcox.test(plate.table$PTC[plate.table$chem == "IDBD"], plate.table$PTC[plate.table$chem == "Mag mini"]); describeBy(plate.table$PTC, group = plate.table$chem); 



colnames(us.irl.plate.table)
plate.table$`% failed` <- 100-plate.table$`% failed`
plate.table$chem <- "SDS"
plate.table$lab <- "IRL-US"


ggplot(plate.table, aes(x = factor(chem), y = pass.rate, fill = factor(chem))) + 
  #scale_fill_brewer(palette="Set3") + 
  geom_boxplot() + 
  xlab("Chemistry") + 
  ylab("Genotyping rate") +
  theme(legend.position = "none") +
#  facet_grid(rows = factor(plate.table$cust)) +
  #geom_violin(trim=FALSE) + 
  stat_summary(fun.data = n_fun, geom = "text", position = position_dodge(width = 0.75)) 

ggplot(ID003, aes(x = factor(Date.lysing), y = pass.rate, fill = factor(chem))) + 
  ylab("Genotyping rate")+
  xlab("Date") + 
  guides(fill=guide_legend(title = "Chemistry"),
         legend.position = "center") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        legend.title.align=0.5,
        plot.title = element_text(hjust = 0.5)) +
  geom_boxplot()

ggplot(ID003, aes(x = factor(Date.lysing), y = pass.rate, fill = factor(chem))) + 
  ylab("Genotyping rate")+
  xlab("Date") + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none",
        legend.title.align=0.5,
        plot.title = element_text(hjust = 0.5)) +
  geom_boxplot() + 
  facet_grid(rows = factor(ID003$chem))

ggplot(ID003, aes(x = factor(`1:2.Dilution.Elution.buffer.batch`), y = pass.rate, fill = factor(chem))) + 
  geom_boxplot() + 
  facet_grid(rows = factor(ID003$chem))


describeBy(ID003$pass.rate, group = ID003$chem)


{
  plate.table$`% failed` <- 100-plate.table$`% failed`

wilcox.test(plate.table$`% failed`[plate.table$lab == "US"], plate.table$`% failed`[plate.table$lab == "IRL"]); describeBy(plate.table$`% failed`, group = plate.table$lab)

wilcox.test(porcid.plate.table$`% failed`[porcid.plate.table$chemistry == "idsp"], porcid.plate.table$`% failed`[porcid.plate.table$chemistry == "pace"]); describeBy(porcid.plate.table$`% failed`, group = porcid.plate.table$chemistry)

wilcox.test(fpl.plate.table$`% failed`[fpl.plate.table$chemistry == "idsp"], fpl.plate.table$`% failed`[fpl.plate.table$chemistry == "pace"]); describeBy(fpl.plate.table$`% failed`, group = fpl.plate.table$chemistry); describeBy(fpl.plate.table$`% failed`, group = fpl.plate.table$chemistry)

wilcox.test(mb44.plate.table$`% failed`[mb44.plate.table$chemistry == "idsp"], mb44.plate.table$`% failed`[mb44.plate.table$chemistry == "pace"]); describeBy(mb44.plate.table$`% failed`, group = mb44.plate.table$chemistry); describeBy(mb44.plate.table$`% failed`, group = mb44.plate.table$chemistry)

wilcox.test(trimmed.plate.table$`average distance`[trimmed.plate.table$chem == "idsp"], trimmed.plate.table$`average distance`[trimmed.plate.table$chem == "pace"]); describeBy(trimmed.plate.table$`average distance`, group = trimmed.plate.table$chem); describeBy(trimmed.plate.table$`average distance`, group = trimmed.plate.table$chem)
wilcox.test(plate.table$`normalized distance`[plate.table$chem == "SDS"], plate.table$`normalized distance`[plate.table$chem == "Tween"]); describeBy(plate.table$`normalized distance`, group = plate.table$chem);
wilcox.test(trimmed.plate.table$`% failed`[trimmed.plate.table$chem == "SDS"], trimmed.plate.table$`% failed`[trimmed.plate.table$chem == "Tween"]); describeBy(trimmed.plate.table$`% failed`, group = trimmed.plate.table$chem); 

wilcox.test(trimmed.plate.table$`% failed`[trimmed.plate.table$chem == "SDS"], trimmed.plate.table$`% failed`[trimmed.plate.table$chem == "Tween"]); describeBy(trimmed.plate.table$`% failed`, group = trimmed.plate.table$chem); 
#tween.subsetted <- tween.plate.table[sample(nrow(tween.plate.table),length(unique(sds.plate.table$BC))),]

#snp.table <- snp.table[snp.table$`average distance` < 10,]
trimmed.plate.table <- trimmed.plate.table[trimmed.plate.table$`% failed` > 80,]
describeBy(plate.table$`% failed`, group = plate.table$chem); 
}
###


###
tapply(df.final$distance, df.final$SNPID, summary)

setDT(df.final)
df < - data.frame(grpBy=df.final$Call, num=df.final$distance)  
df[, as.list(summary(distance)), by = Call]
###

dunnTest(plate.table$`% failed` ~ plate.table$var, method = "holm")[["dtres"]]; describeBy(plate.table$`% failed`, group = plate.table$var)
dunnTest(plate.table$`normalized distance` ~ plate.table$var, method = "holm")[["dtres"]]; describeBy(plate.table$`normalized distance`, group = plate.table$var)

#cleaning up the data, trimming out plates with failure rates higher than 15%
trimmed.plate.table <- plate.table[plate.table$`% failed` < 15,]

#non-par stats, dunn is a subset of kruskal-wallace which tests >2 groups
dunnTest(trimmed.plate.table$`% failed` ~ trimmed.plate.table$var, method = "holm")[["dtres"]]; describeBy(trimmed.plate.table$`% failed`, group = trimmed.plate.table$var)
dunnTest(trimmed.plate.table$`average distance` ~ trimmed.plate.table$var, method = "holm")[["dtres"]]

#tests for normality
shapiro.test(trimmed.plate.table$`average distance`)
ks.test(trimmed.plate.table$`% failed`, 'pnorm')

#plots testing for normality
qqnorm(trimmed.plate.table$`average distance`, main = 'Normal')
qqline(trimmed.plate.table$`average distance`)

us.plate.table$lab <- "US"; uk.plate.table$lab <- "UK"
plate.table <- rbind(us.plate.table, uk.plate.table)

ggplot(data = mb44.plate.table, aes(x = mb44.plate.table$`% failed`, fill = mb44.plate.table$chemistry), color = c("red", "blue")) +                       # Draw overlaying histogram
  geom_histogram(aes(y = stat(density)), alpha = 0.75, bins = 50)
#  facet_grid(rows = factor(plate.table$spec.cust)) 
  #geom_vline(aes(xintercept = mean))

#add in rows that manually mean the things, add vlines for it?
ggplot(trimmed.plate.table, aes(x = trimmed.plate.table$`% failed`))

ggplot(data = plate.table, aes(x = `% failed`, fill = var), color = c("red", "blue")) +
  geom_histogram(aes(y = stat(density)), alpha = 0.75, bins = 45)

ggplot(data = trimmed.plate.table, aes(x = `% failed`, fill = chem), color = c("red", "blue")) +
  geom_histogram(aes(y = stat(density)), alpha = 0.75, bins = 30) +
  facet_grid(rows = factor(trimmed.plate.table$lab))

ggplot(data = trimmed.plate.table, aes(x = `% failed`, fill = chem), color = c("red", "blue")) +
  geom_histogram(aes(y = stat(density)), alpha = 0.75, bins = 30)
  #facet_grid(rows = factor(trimmed.plate.table$chem))

ggplot(trimmed.plate.table, aes(x = factor(chem), y = `% failed`, fill = factor(chem))) +
  scale_fill_brewer(palette="Set3") +
  geom_boxplot() +
  facet_grid(rows = factor(trimmed.plate.table$lab))


###swiss cluster tighness 2023-12-20
plate.table$treatment <- key$treatment[match(plate.table$`plate BC`, key$plate)]
plate.table$note <- key$Notes[match(plate.table$`plate BC`, key$plate)]
complete.plate.table <- plate.table

plate.table <- plate.table[!is.na(plate.table$treatment),]
plate.table$pass.rate <- 100-plate.table$`% failed`

treatments <- unique(plate.table$treatment)
notes <- unique(plate.table$note)

wilcox.test(plate.table$`average distance`[plate.table$treatment == "automated"], plate.table$`average distance`[plate.table$treatment == "manual"]); describeBy(plate.table$`average distance`, group = plate.table$treatment); 
ggplot(plate.table, aes(x = factor(treatment), y = `average distance`, fill = factor(treatment))) + geom_boxplot()

wilcox.test(plate.table$`average distance`[plate.table$note == notes[1]], plate.table$`average distance`[plate.table$note == notes[2]]); describeBy(plate.table$pass.rate, group = plate.table$note); 
ggplot(plate.table, aes(x = factor(note), y = `average distance`, fill = factor(note))) + geom_boxplot()
