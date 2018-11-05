setwd("/Volumes/GoogleDrive/My Drive/IA Grad Projects/GWAS/Files_Analysis/Input_Files/Version_01")
#Install/library packages
library(lme4)
library(ggplot2)
library(dplyr)
library(nlme)
library(plyr)
########################################
#Begin analysis with new df   #

#Import new df
    df<-read.csv("Combined_v2.csv")
    
#Run linear mixed model to compute BLUPs and variance components
    
#Remove columns with missing data     
    df_01 <- df[,colSums(is.na(df))<nrow(df)]  

#Remove options to run mixed models
    options(lmerControl=list(maxIter = 1000,check.nobs.vs.rankZ = "warning",
                         check.nobs.vs.nlev = "warning",
                         check.nobs.vs.nRE = "warning",
                         check.nlev.gtreq.5 = "warning",
                         check.nlev.gtr.1 = "warning",calc.derivs = FALSE))
#Run mixed model for grain yield
  #Atl 1
  y <- lmer(Yield~ (1|name)+(1|name:Loc) +(1|Loc/Block/iBlock) ,data = df_01)
  y
  varComp<-as.data.frame(VarCorr(y))
  blup = ranef(y)
  blup$Yield = blup$name
  LINEBLUP = blup[,1]
  
  #Alt 2
  y <- lmer(Yield~ (1|name)+(1|name:Loc) +(1|Loc/Block) + (1|Block/iBlock) ,data = df_01)
  
  #Alt 3
  y1<-lmer(Yield~(1|name)+(1|name:Loc) + (1|Block%in%Loc) + (1|iBlock%in%Block),data = df_01)
  varComp1<-as.data.frame(VarCorr(y1))
  blup1 = ranef(y1)
  blup1 = blup1$name
  LINEBLUP1 = blup1[,1]

#Modify variance component df
  drops <- c("var1","var2","vcov")
  varComp<-varComp[ , !(names(varComp) %in% drops)]
  varComp$Trait<-"Yield"
  
## Compare BLUP to line averages on a scatterplot
  LINEBLUP = brixlineblup[,1]
## Compare BLUP to line averages on a scatterplot
  lmean = tapply(df_01$Yield, df_01$name, na.rm=T, mean)
  plot(LINEBLUP, lmean, col="blue")
  cor(LINEBLUP, lmean)







