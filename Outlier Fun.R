# Description: Outlier Fun
# by Kevin Falk
# May 2018

######## 

AllData=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/August_Extracted_Data.csv")

outputdf <- data.frame() #empty dataframe

for (j in c(6,9,12)){ #j is day
  dayloop <- subset(AllData, AllData$Day == j)
 
  for (i in 1:300){ #i is entry number
    entryloop <- subset(dayloop, dayloop$Entry == i)
    for (k in 20:32){ #k is trait column name
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
write.csv(outputdf,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/August_Extracted_Data_NoOutliers.csv", row.names = F)    








#check loop
i = 164 #entry
j = 6 #day
k = 24

k[1:5,20:23]

outlierKD(entryloop,TotalRootLength)

write.csv(outputdf,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/FullData_PrimaryRootLength_NoOutliers3.csv", row.names = F)
k=4
for (k in col(dayloop)){
  entryloop[[k]]
  }

entryloop[[k]]
