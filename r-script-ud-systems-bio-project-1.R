setwd("/home/visnu/MEGA/UDPhDMBG/05_Fall_2022/BINF694_Systems_Biology/Projects/p1_data")
getwd()

library(sandwich)
library(lmtest)
library(multcomp)
library(pheatmap)
library(RColorBrewer)
library(DESeq2)
library(genefilter)
library(edgeR)

files<-c("brain_a.s_trimmed.sorted.count.txt","brain_3b.s_trimmed.sorted.count.txt","brain_3c.s_trimmed.sorted.count.txt","kidney_a.s_trimmed.sorted.count.txt","kidney_b.s_trimmed.sorted.count.txt","kidney_c.s_trimmed.sorted.count.txt")
organ<-c("brain","brain","brain","kidney","kidney","kidney")
samples<-c("brain_a","brain_3b","brain_3c","kidney_a","kidney_b","kidney_c")

sampleTable<-data.frame(sampleName=samples, fileName=files, condition=organ)
project_data1 <- DESeqDataSetFromHTSeqCount(sampleTable=sampleTable, design= ~ condition)

project_data2 <-readDGE(files, group=organ, labels=samples)

data_deseq <- project_data1
head(counts(data_deseq))

# filtering 
nrow(data_deseq)
data_deseq <- data_deseq[ rowSums(counts(data_deseq)) > 1, ]

# log transformation  
rld <- rlog(data_deseq, blind=FALSE)

# distance heatmap 
sampleDists <- dist(t(assay(rld)))
sampleDists
sampleDistMatrix <- as.matrix(sampleDists)
rownames(sampleDistMatrix) <- paste(rld$cell_type, rld$dev_stage, rld$replicate, sep="-")
colors <- colorRampPalette(rev(brewer.pal(9, "Blues")) )(255)
pheatmap(sampleDistMatrix, clustering_distance_rows=sampleDists, clustering_distance_cols=sampleDists, col=colors)

# pca analysis 
plotPCA(rld, intgroup = c("condition"))

# gene heatmap 
geneVars <- rowVars(assay(rld))
geneVarsOrdered <- order(geneVars, decreasing = TRUE)
topVarGenes <- head(geneVarsOrdered, 100)
mat <- assay(rld)[topVarGenes, ]
mat <- mat - rowMeans(mat)
df <- as.data.frame(colData(rld)[,c("condition"), drop=FALSE])
clear_col_names <- paste(rld$cell_type, rld$dev_stage, rld$replicate, sep=".")
topGenesHeatmap <- pheatmap(mat, annotation_col=df, fontsize_row = 6)

# bcv plot 
project_data2 <-readDGE(files, group=organ, labels=samples)
y <- estimateDisp(project_data2)
plotBCV(y)

# smear plot & differential expression analysis 
et <- exactTest(y)
summary(de <- decideTestsDGE(et))

detags <- rownames(y)[as.logical(de)]
plotSmear(et, de.tags=detags)
abline(h=c(-1, 1), col="blue")

# list of differentially expressed gene 
diffExpGenes <- topTags(et, n=1000, p.value = 0.01)
head(diffExpGenes$table)
write.table(diffExpGenes$table, file="brain_vs_kidney_exactTest.txt", sep = "\t", row.names=TRUE, col.names=NA)
