# Description: LSD
# by Kevin Falk
# May 2018

###############################################################

library(lme4)
library(agricolae)
library(dplyr)
df=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/BLUPs/AdjustedBLUPsAllDays_thinned.csv")

str(df)
summary(df)
rownames(df)
#remove BAD genotypes PI594438 (273) PI495832 (202) PI467333A (382) PI473899 (390) PI506528 (204) PI445845 (367)
df <- df %>%
  filter(!((Entry == "273")|(Entry == "202")|(Entry  == "382")|( df$Entry  == "390")|( df$Entry  == "204")|( df$Entry  == "367" )))

model<- aov(df$TRL~Shape, data=df)
cv.model(model)
mean(df$TRL)
degreesFreedom <- df.residual(model)
MSerror <- deviance(model)/degreesFreedom
LSD.test(model, 'Entry',console=TRUE)







########################################################## stolen from OUTLIER FUN
outputdf <- data.frame() #empty dataframe

for (j in c(6,9,12)){ #j is day
  dayloop <- subset(df, df$Day == j)
  
  for (i in 1:292){ #i is entry number
    entryloop <- subset(dayloop, dayloop$Entry == i)
    for (k in 16:ncol(df)){ #k is trait column name
      var <- entryloop[[k]]
      var_name <- eval(substitute(var),eval(entryloop)) #pulls out the numbers from the dataframe
      outlier <- boxplot.stats(var_name)$out #sets outliers to outlier variable
      var_name <- ifelse(var_name %in% outlier, NA, var_name) #removes outliers from data and sets them to N/A
      entryloop[[k]] <- var_name
    }
    outputdf <- rbind(outputdf,entryloop)
  }
}
#assign(nam,entryloop) #### replace df with whatever data frame contains all your information
write.csv(outputdf,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Root Shape/9ShapeClusterLSD.csv", row.names = F)    
#check loop