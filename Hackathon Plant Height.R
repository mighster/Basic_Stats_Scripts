
#set working directory
setwd("C:/Users/falk/Google Drive/PhD/Analysis and Statistics/Hackathon - NIFA/tutorials-master/height_prediction/data")

####################### Part 1 ##################################

#read in csv file and set it as full_data
genotypes <- read.delim("genotype_ids.csv", header = T, sep = ",", stringsAsFactors = T)
height <- read.delim("estimated_heights.csv", header = T, sep = ",", stringsAsFactors = T)

head(height)
kinship<-read.csv("centered_kinship.csv")
head(kinship)

#Remove columns with complete missing data
height_sub <- height[,colSums(is.na(height))<nrow(height)]

#Impute missing data for columns that have missing data
###MissForest Imputation
library(missForest)
library(easypackages)
library(caret)
library(randomForest)
library(parallel)
library(doSNOW)
#Enable Parallel processing for quicker computational time
detectCores();NumberOfCluster <- 4  #Set as the number of cores found from detectCores function;
cl <- makeCluster(NumberOfCluster) ;registerDoSNOW(cl) #Make clusters from assigned cores
forestImputes <- missForest(height_sub,maxiter=10,variablewise = T,verbose = T,parallelize = "variables")
oob_error<-forestImputes$OOBerror
a<-sqrt(oob_error)
finalForest <- forestImputes$ximp
head(finalForest)



oob_error<-cbind(colnames(only_data),a)
b<-as.data.frame(oob_error)
b$a <- as.numeric(as.character(b$a))
