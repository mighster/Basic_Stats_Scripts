for (i in 1:length(myYs)) {
  setwd("E:/BCB546X-Fall2017_Undercaffeinated_And_Underpaid/Phenotype_Files/") # set directory where the phenotype files are located
  Y <- read.csv(file = myYs[i]) # read in next set of trait phenotypes
  y <- colnames(Y)[2] # obtain the name of the trait. All files have phenotypes in second column (starting from 1, not 0)
  line = Y[names(Y) %in% c('acid', y)] # obtain list of acession identifiers found in the phenotype file
  line1 <- line[(line$acid %in% GD$PI),] # filter the phenotype file for only accessions that have been genotyped (those in genotype file)
  myY <- line1[which(line1$acid %in% GD$PI),] # select lines that have been genotyped
  myGD <- GD[which(GD$PI %in% myY$acid),]# select lines that have observations available
  g= nrow(myY) # number of accessions in current GWAS test... to be used as an argument in the "GAPIT" function
  dir.create(paste(results_dir, y)) # create a folder withing the "Results" directory to store the output of the GWAS test
  setwd(paste(results_dir, y)) # change the directory to output the GAPIT results into the folder just created
  myGAPIT <- GAPIT(Y = myY, 
                   GD = myGD, 
                   GM = myGM, 
                   PCA.total = 3, 
                   group.from = g, 
                   group.to = g,
                   SNP.fraction = 1, 
                   SNP.MAF = 0.05) # regular MLM with no compression
}