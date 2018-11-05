###################GWAS#############
setwd("/Users/zhangjiaoping/Desktop/ISU/IDC")
GD <- read.table("PI467_nmis0.1_maf.05_GD_V2.txt", header = T) #genotypic data file
myGM <- read.table("PI467_nmis0.1_maf.05_GM_V2.txt",header = T)#genetic map file
Y <- read.table("ML_FVR.txt",header = T)#phenotypic data
head(Y)
names(Y)
str(Y)
Y$Rep <- as.factor(Y$Rep)
y <- Y[names(Y)=='ML_FVR']

#Prepare the BLUP of the phenotypic data that have multiple replications but one environment
install.packages('lme4')#install the lme4 if you have not. this only need once for the first time
library(lme4) 
varcomp <- lmer(y[[1]]~(1|PI) + (1|Rep),data=Y)
fixef(varcomp)
summary(varcomp)
blup <- ranef(varcomp)
str(blup)
line <- blup$PI + fixef(varcomp)
names(line) <- 'ML_FVR'
line$PI <- rownames(line)
line[1:5,1:2]
line <- line[c(2,1)] 
rownames(line) <- NULL
line <- line[order(line$PI),]
dim(line)
line[!(line$PI %in% GD$PI),]
myY <- line[which(line$PI %in% GD$PI),]#select lines have been genotyped
myGD <- GD[which(GD$PI %in% myY$PI),]#seelction lines have observation available

dim(myY)
dim(myGD)
dim(myGM)


library(multtest)
library(gplots)
library(LDheatmap)
library(genetics)
library(compiler)
library(scatterplot3d)
source("http://www.zzlab.net/GAPIT/gapit_functions.txt")
source("http://www.zzlab.net/GAPIT/emma.txt")

g= nrow(myY)
##in this case I first creat 4 folders to save the GAPIT output file under the '/Users/zhangjiaoping/Desktop/ISU/IDC'. They are 'M_selection','GLM','cMLM' and 'MLM'.
setwd("/Users/zhangjiaoping/Desktop/ISU/IDC/M_selection")
myGAPIT <- GAPIT(Y = myY, GD = myGD, GM = myGM, PCA.total =10, group.from =1, group.to = g, SNP.fraction = 1, SNP.MAF = 0.05,kinship.cluster = c("average","complete","ward"), kinship.group =c("Mean","Max"), Model.selection =T)#model selection

setwd("/Users/zhangjiaoping/Desktop/ISU/IDC/GLM")
myGAPIT <- GAPIT(Y = myY, GD = myGD, GM = myGM, PCA.total =2, group.from =0, group.to = 0, SNP.fraction = 1, SNP.MAF = 0.05)#General linear model by setting group parameters = 0, the PCA.total = 2 is determined by the model selection output results

setwd("/Users/zhangjiaoping/Desktop/ISU/IDC/cMLM")
myGAPIT <- GAPIT(Y = myY, GD = myGD, GM = myGM, PCA.total =2, group.from =244, group.to = 244,SNP.fraction = 1, SNP.MAF = 0.05, kinship.cluster = c("average"), kinship.group =c("Mean"))#compressed mixed linear model by specify the group number and the compression algorithms that were optimized through the 'model selection'. 

setwd("/Users/zhangjiaoping/Desktop/ISU/IDC/MLM")
myGAPIT <- GAPIT(Y = myY, GD = myGD, GM = myGM, PCA.total =2, group.from =g, group.to = g,SNP.fraction = 1, SNP.MAF = 0.05)#Regular mixed linear model with no compression by setting group number = g (the number of PI).
