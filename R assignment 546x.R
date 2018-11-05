setwd("/Users/jmshook/Desktop/Class/EEOB546/BCB546X-Fall2017/UNIX_Assignment")


genotypes <- read.table("fang_et_al_genotypes.txt", header = T, stringsAsFactors = F)
head(genotypes)
dim(genotypes)
str(genotypes)
nrow(genotypes)
ncol(genotypes)
colnames(genotypes)
row.names(genotypes)

snps <- read.delim("snp_position.txt", header = T, stringsAsFactors = F)
head(snps)
dim(snps)
str(snps)
nrow(snps)
ncol(snps)
colnames(snps)
row.names(snps)
############
#extract only maize rows
maize <- genotypes[genotypes$Group == "ZMMIL" | genotypes$Group == "ZMMLR" | genotypes$Group == "ZMMMR",]
nrow(maize)

transposed_maize <- t(maize)

merged_maize <- merge(snps, transposed_maize, by.x = "SNP_ID", by.y = "row.names")
nrow(merged_maize)

maize_cut <- merged_maize[,-c(2,5:15)]

maize_cut_increasing_snps <- maize_cut[order(as.numeric(as.character(maize_cut$Position))),]

for (i in 1:10) {
  maize_loop <- maize_cut_increasing_snps[maize_cut_increasing_snps$Chromosome == i,]
  write.csv(maize_loop, sprintf("maize_chromosome_%d_increasing_snps", i), row.names = F)
}


maize_cut_decreasing_snps <- maize_cut[order(-as.numeric(as.character(maize_cut$Position))),]

maize_dashes <- maize_cut_decreasing_snps
maize_dashes[maize_dashes == "?/?"] <- "-/-"

for (i in 1:10) {
  maize_loop <- maize_dashes[maize_dashes$Chromosome == i,]
  write.csv(maize_loop, sprintf("maize_chromosome_%d_decreasing_snps", i), row.names = F)
}
###############
#extract only teosinte rows
teosinte <- genotypes[genotypes$Group == "ZMMIL" | genotypes$Group == "ZMMLR" | genotypes$Group == "ZMMMR",]
nrow(teosinte)

transposed_teosinte <- t(teosinte)

merged_teosinte <- merge(snps, transposed_teosinte, by.x = "SNP_ID", by.y = "row.names")
nrow(merged_teosinte)

teosinte_cut <- merged_teosinte[,-c(2,5:15)]

teosinte_cut_increasing_snps <- teosinte_cut[order(as.numeric(as.character(teosinte_cut$Position))),]

for (i in 1:10) {
  teosinte_loop <- teosinte_cut_increasing_snps[teosinte_cut_increasing_snps$Chromosome == i,]
  write.csv(teosinte_loop, sprintf("teosinte_chromosome_%d_increasing_snps", i), row.names = F)
}

teosinte_cut_decreasing_snps <- teosinte_cut[order(-as.numeric(as.character(teosinte_cut$Position))),]

teosinte_dashes <- teosinte_cut_decreasing_snps
teosinte_dashes[teosinte_dashes == "?/?"] <- "-/-"

for (i in 1:10) {
  teosinte_loop <- teosinte_dashes[teosinte_dashes$Chromosome == i,]
  write.csv(teosinte_loop, sprintf("teosinte_chromosome_%d_decreasing_snps", i), row.names = F)
}

#############
transposed_genotypes <- t(genotypes)
snp_genotype_full <- merge(snps, transposed_genotypes, by.x = "SNP_ID", by.y = "row.names")

snp_genotype <- snp_genotype_full[,-c(2,5:15)]

log_snp_genotype <- snp_genotype[,-c(1:3)]

log_snp_genotype[log_snp_genotype == "A/C"] <- 1
log_snp_genotype[log_snp_genotype == "A/G"] <- 1
log_snp_genotype[log_snp_genotype == "A/T"] <- 1
log_snp_genotype[log_snp_genotype == "C/G"] <- 1
log_snp_genotype[log_snp_genotype == "C/T"] <- 1
log_snp_genotype[log_snp_genotype == "C/A"] <- 1
log_snp_genotype[log_snp_genotype == "G/A"] <- 1
log_snp_genotype[log_snp_genotype == "G/C"] <- 1
log_snp_genotype[log_snp_genotype == "T/A"] <- 1
log_snp_genotype[log_snp_genotype == "G/T"] <- 1
log_snp_genotype[log_snp_genotype == "T/C"] <- 1
log_snp_genotype[log_snp_genotype == "T/G"] <- 1
log_snp_genotype[log_snp_genotype == "A/A"] <- 1
log_snp_genotype[log_snp_genotype == "C/C"] <- 1
log_snp_genotype[log_snp_genotype == "G/G"] <- 1
log_snp_genotype[log_snp_genotype == "T/T"] <- 1

log_snp_genotype[log_snp_genotype == "?/?"] <- 0

write.csv(log_snp_genotype, file = "log_snp_genotype", row.names = F)
#couldn't get R to sum up the 1s for each row, so did it in excel, then read in the csv file
log_snp_genotype_csv <- read.csv("log_snp_genotype.csv", stringsAsFactors = F)

snp_genotype_cut <- snp_genotype[,1:3]
snp_counts <- log_snp_genotype_csv[,2783]
snp_genotype_counts <- cbind(snp_genotype_cut, snp_counts)
snp_genotype_counts_ordered <- snp_genotype_counts[order(as.numeric(as.character(snp_genotype_counts$Chromosome))),]

stringsAsFactors = F
snp_count_1 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 1,])
snp_count_2 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 2,])
snp_count_3 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 3,])
snp_count_4 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 4,])
snp_count_5 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 5,])
snp_count_6 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 6,])
snp_count_7 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 7,])
snp_count_8 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 8,])
snp_count_9 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 9,])
snp_count_10 <- t(snp_genotype_counts_ordered[snp_genotype_counts_ordered$Chromosome == 10,])

snp_counts <- data.frame(sum(as.numeric(snp_count_1[4,])), sum(as.numeric(snp_count_2[4,])), sum(as.numeric(snp_count_3[4,])), sum(as.numeric(snp_count_4[4,])), sum(as.numeric(snp_count_5[4,])), sum(as.numeric(snp_count_6[4,])), sum(as.numeric(snp_count_7[4,])), sum(as.numeric(snp_count_8[4,])), sum(as.numeric(snp_count_9[4,])), sum(as.numeric(snp_count_10[4,])))
snp_counts_vector <- c(sum(as.numeric(snp_count_1[4,])), sum(as.numeric(snp_count_2[4,])), sum(as.numeric(snp_count_3[4,])), sum(as.numeric(snp_count_4[4,])), sum(as.numeric(snp_count_5[4,])), sum(as.numeric(snp_count_6[4,])), sum(as.numeric(snp_count_7[4,])), sum(as.numeric(snp_count_8[4,])), sum(as.numeric(snp_count_9[4,])), sum(as.numeric(snp_count_10[4,])))

colnames(snp_counts) <- 1:10

#Plot of snps per chromosome
plot(snp_counts_vector, xlab = "Chromosome", ylab = "Number of Snps", main = "Number of Snps per Chromosome")

############
genotypes_log <- genotypes
genotypes_log[genotypes_log == "A/C"] <- 1
genotypes_log[genotypes_log == "A/G"] <- 1
genotypes_log[genotypes_log == "A/T"] <- 1
genotypes_log[genotypes_log == "C/G"] <- 1
genotypes_log[genotypes_log == "C/T"] <- 1
genotypes_log[genotypes_log == "C/A"] <- 1
genotypes_log[genotypes_log == "G/A"] <- 1
genotypes_log[genotypes_log == "G/C"] <- 1
genotypes_log[genotypes_log == "T/A"] <- 1
genotypes_log[genotypes_log == "G/T"] <- 1
genotypes_log[genotypes_log == "T/C"] <- 1
genotypes_log[genotypes_log == "T/G"] <- 1
genotypes_log[genotypes_log == "A/A"] <- 1
genotypes_log[genotypes_log == "C/C"] <- 1
genotypes_log[genotypes_log == "G/G"] <- 1
genotypes_log[genotypes_log == "T/T"] <- 1

genotypes_log[genotypes_log == "?/?"] <- 0


genotypes_TRIPS <- t(genotypes_log[genotypes_log$Group == "TRIPS",])
genotypes_ZDIPL <- t(genotypes_log[genotypes_log$Group == "ZDIPL",])
genotypes_ZPERR <- t(genotypes_log[genotypes_log$Group == "ZPERR",])
genotypes_ZLUXR <- t(genotypes_log[genotypes_log$Group == "ZLUXR",])
genotypes_ZMHUE <- t(genotypes_log[genotypes_log$Group == "ZMHUE",])
genotypes_ZMPBA <- t(genotypes_log[genotypes_log$Group == "ZMPBA",])
genotypes_ZMPJA <- t(genotypes_log[genotypes_log$Group == "ZMPJA",])
genotypes_ZMXCH <- t(genotypes_log[genotypes_log$Group == "ZMXCH",])
genotypes_ZMXCP <- t(genotypes_log[genotypes_log$Group == "ZMXCP",])
genotypes_ZMXNO <- t(genotypes_log[genotypes_log$Group == "ZMXNO",])
genotypes_ZMXNT <- t(genotypes_log[genotypes_log$Group == "ZMXNT",])
genotypes_ZMPIL <- t(genotypes_log[genotypes_log$Group == "ZMPIL",])
genotypes_ZMXIL <- t(genotypes_log[genotypes_log$Group == "ZMXIL",])
genotypes_ZMMLR <- t(genotypes_log[genotypes_log$Group == "ZMMLR",])
genotypes_ZMMMR <- t(genotypes_log[genotypes_log$Group == "ZMMMR",])
genotypes_ZMMIL <- t(genotypes_log[genotypes_log$Group == "ZMMIL",])

sum(as.numeric(genotypes_TRIPS[4:986,]))
group_snp_counts <- c(sum(as.numeric(genotypes_TRIPS[4:986,])),
                      sum(as.numeric(genotypes_ZDIPL[4:986,])),
                      sum(as.numeric(genotypes_ZPERR[4:986,])),
                      sum(as.numeric(genotypes_ZMHUE[4:986,])),
                      sum(as.numeric(genotypes_ZMPBA[4:986,])),
                      sum(as.numeric(genotypes_ZMPJA[4:986,])),
                      sum(as.numeric(genotypes_ZMXCH[4:986,])),
                      sum(as.numeric(genotypes_ZMXCP[4:986,])),
                      sum(as.numeric(genotypes_ZMXNO[4:986,])),
                      sum(as.numeric(genotypes_ZMXNT[4:986,])),
                      sum(as.numeric(genotypes_ZMPIL[4:986,])),
                      sum(as.numeric(genotypes_ZMXIL[4:986,])),
                      sum(as.numeric(genotypes_ZMMLR[4:986,])),
                      sum(as.numeric(genotypes_ZMMMR[4:986,])),
                      sum(as.numeric(genotypes_ZMMIL[4:986,])),
                      sum(as.numeric(genotypes_ZLUXR[4:986,]))
)
names(group_snp_counts) <- c("TRIPS", "ZDIPL", "ZPERR", "ZMHUE", "ZMPBA", "ZMPJA",
                             "ZMXCH", "ZMXCP", "ZMXNO", "ZMXNT", "ZMPIL", "ZMXIL", "ZMMLR", "ZMMMR", "ZMMIL", "ZLUXR")

#Plot of snps per chromosome
plot(group_snp_counts, xaxt = "n", xlab = "Group", ylab = "Number of Snps", main = "Number of Snps per Group")
axis(1, at=1:16, labels = names(group_snp_counts))

###############
####homo/hetero
genotypes_homo_hetero <- genotypes

#make heterozygotes = 1, homozygotes = 0, other = N/A
genotypes_homo_hetero[genotypes_homo_hetero == "A/C"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "A/G"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "A/T"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "C/G"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "C/T"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "C/A"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "G/A"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "G/C"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "T/A"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "G/T"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "T/C"] <- 1
genotypes_homo_hetero[genotypes_homo_hetero == "T/G"] <- 1

genotypes_homo_hetero[genotypes_homo_hetero == "A/A"] <- 0
genotypes_homo_hetero[genotypes_homo_hetero == "C/C"] <- 0
genotypes_homo_hetero[genotypes_homo_hetero == "G/G"] <- 0
genotypes_homo_hetero[genotypes_homo_hetero == "T/T"] <- 0
genotypes_homo_hetero[genotypes_homo_hetero == "?/?"] <- "N/A"


homo_hetero_TRIPS <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "TRIPS",])
homo_hetero_ZDIPL <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZDIPL",])
homo_hetero_ZPERR <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZPERR",])
homo_hetero_ZLUXR <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZLUXR",])
homo_hetero_ZMHUE <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMHUE",])
homo_hetero_ZMPBA <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMPBA",])
homo_hetero_ZMPJA <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMPJA",])
homo_hetero_ZMXCH <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMXCH",])
homo_hetero_ZMXCP <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMXCP",])
homo_hetero_ZMXNO <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMXNO",])
homo_hetero_ZMXNT <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMXNT",])
homo_hetero_ZMPIL <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMPIL",])
homo_hetero_ZMXIL <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMXIL",])
homo_hetero_ZMMLR <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMMLR",])
homo_hetero_ZMMMR <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMMMR",])
homo_hetero_ZMMIL <- t(genotypes_homo_hetero[genotypes_homo_hetero$Group == "ZMMIL",])

TRIPS <- table(unlist(homo_hetero_TRIPS))
ZDIPL <- table(unlist(homo_hetero_ZDIPL))
ZPERR <- table(unlist(homo_hetero_ZPERR))
ZLUXR<- table(unlist(homo_hetero_ZLUXR))
ZMHUE<- table(unlist(homo_hetero_ZMHUE))
ZMPBA<- table(unlist(homo_hetero_ZMPBA))
ZMPJA<- table(unlist(homo_hetero_ZMPJA))
ZMXCH<- table(unlist(homo_hetero_ZMXCH))
ZMXCP<- table(unlist(homo_hetero_ZMXCP))
ZMXNO<- table(unlist(homo_hetero_ZMXNO))
ZMXNT<- table(unlist(homo_hetero_ZMXNT))
ZMPIL<- table(unlist(homo_hetero_ZMPIL))
ZMXIL<- table(unlist(homo_hetero_ZMXIL))
ZMMLR<- table(unlist(homo_hetero_ZMMLR))
ZMMMR<- table(unlist(homo_hetero_ZMMMR))
ZMMIL<- table(unlist(homo_hetero_ZMMIL))

snp_proportion <- data.frame(c(14220, 678, 6728),
                             c(12717, 501, 1527),
                             c(7354,875,618),
                             c(14507, 472, 1732),
                             c(8515, 772, 543),
                             c(648658, 195961, 40081),
                             c(27393, 5394, 635),
                             c(56925, 13637, 3163),
                             c(51887, 13188, 2752),
                             c(5664, 973, 244),
                             c(3269, 146, 517),
                             c(37841, 731, 1731),
                             c(5521, 141, 236),
                             c(981471, 206037, 47140),
                             c(22854, 759, 2928),
                             c(259176, 1017, 24877)
)
colnames(snp_proportion) = c("TRIPS", "ZDIPL", "ZPERR", "ZLUXR",
                             "ZMHUE", "ZMPBA", "ZMPJA", "ZMXCH", "ZMXCP", "ZMXNO",
                             "ZMXNT", "ZMPIL", "ZMXIL", "ZMMLR", "ZMMMR", "ZMMIL")
#end