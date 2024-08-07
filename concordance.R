orig.file <- read.csv(file.choose(), skip = 6)
qq.file <- read.csv(file.choose(), skip = 6)


new.file <- orig.file[orig.file$DNA...Assay %in% qq.file$DNA...Assay,]
new.file <- orig.file[,colnames(orig.file) %in% colnames(qq.file)]

output <- new.file == qq.file
output[,1] <- qq.file$DNA...Assay

sample.list <- unique(qq.file$DNA...Assay)
snp.list <- unique(colnames(qq.file[2:length(qq.file)]))