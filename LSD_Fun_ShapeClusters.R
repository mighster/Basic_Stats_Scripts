# Description: LSD
# by Kevin Falk
# May 2018

###############################################################

library(lme4)
library(agricolae)

AllData<-read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/BLUPs/AdjustedBLUPsAllDays_thinned.csv")

#split data into df Days
Day6_data <- subset(AllData, AllData$Day == "6")
Day9_data <- subset(AllData, AllData$Day == "9")
Day12_data <- subset(AllData, AllData$Day == "12")

#take only the columns with numerical data
colnames(AllData)
colnum=c(16:ncol(Day6_data)) 
Day6DataOutput <- as.matrix
i=1
for (i in 1:41){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day6_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day6_data1 <- Day6_data
  colnames(Day6_data1)[x] <-"y"  #renames the trait variable as "y" for the model analysis below
  model<- aov(Day6_data1$y~Shape, data=Day6_data1)
  LSD_data <- LSD.test(model, 'Shape',console=TRUE)
  colnames(LSD_data$groups) <- c(trait,"group")
  #add columns to existing dataframe   
  Day6DataOutput <- cbind(Day6DataOutput,rownames(LSD_data$groups), LSD_data$groups)
}
Day6DataOutput$Day <- c(6)


#take only the columns with numerical data
colnames(AllData)
colnum=c(16:ncol(Day9_data)) 
Day9DataOutput <- matrix()
i=1
for (i in 1:41){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day9_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day9_data1 <- Day9_data
  colnames(Day9_data1)[x] <-"y"  #renames the trait variable as "y" for the model analysis below
  model<- aov(Day9_data1$y~Shape, data=Day9_data1)
  LSD_data <- LSD.test(model, 'Shape',console=TRUE)
  colnames(LSD_data$groups) <- c(trait,"group")
  #add columns to existing dataframe   
  Day9DataOutput <- cbind(Day9DataOutput,rownames(LSD_data$groups),LSD_data$groups)
}
Day9DataOutput$Day <- c(9)

#take only the columns with numerical data
colnames(AllData)
colnum=c(16:ncol(Day12_data)) 
Day12DataOutput <- matrix()
i=1
for (i in 1:41){  #this second loop runs through each TRAIT, one at a time
  x=colnum[i]  #set the current [i] column as x
  trait=colnames(Day12_data)[x] #sets the current column header as the trait name
  #print(trait)  #prints the trait name using the column header from previous line
  Day12_data1 <- Day12_data
  colnames(Day12_data1)[x] <-"y"  #renames the trait variable as "y" for the model analysis below
  model<- aov(Day12_data1$y~Shape, data=Day12_data1)
  LSD_data <- LSD.test(model, 'Shape',console=TRUE)
  colnames(LSD_data$groups) <- c(trait,"group")
  #add columns to existing dataframe   
  Day12DataOutput <- cbind(Day12DataOutput,rownames(LSD_data$groups),LSD_data$groups)
}
Day12DataOutput$Day <- c(12)

#bind the data frames
data_output <- rbind(Day6DataOutput[,2:ncol(Day6DataOutput)],Day9DataOutput[,2:ncol(Day9DataOutput)],Day12DataOutput[,2:ncol(Day12DataOutput)])

#output that beast
write.csv(data_output,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/Paper#2/Root Shape/ShapeCluster_LSDs.csv")

