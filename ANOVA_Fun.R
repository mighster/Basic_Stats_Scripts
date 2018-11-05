# Description: Run ANOVA
# by Kevin Falk
# May 2018

###############################################################

library(lme4)

df=read.csv("C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Raw Data/BlueSteelData05_11_2018_NoGR_CHECKSTOP.csv")
str(df$Rep)
summary(df$Rep)
#set columns as categorical variables
df$Rep=as.factor(df$Rep)
df$SB=as.factor(df$SB)
df$Entry=as.factor(df$Entry)

colnames(df) #look at column names
colnum=c(18:53) #take only the columns with numerical data
names=colnames(df) #set column names to a list
traits <- colnames(df,c(18:53))


#Split data by Day (total of five, missing one)
mylist <- split(df, df$Day)
length(mylist) #3 days
head(mylist)

i=1 #check loop
k = 1
dflist=list() #empty list for loop
fdflist=list()
anvlist=list() 
anvlist2=list()
re.anvlist2=list()
dataframe1 <- data.frame() #empty dataframe (it must have something in it for the cbind to work)

outputData <- data.frame(matrix(ncol = 1,nrow = 5))
dim(outputData)













for (k in 1:length(mylist)){ #this first loop runs through each DAY, one at a time
  df=mylist[[k]]
  for (i in 18:53){  #this second loop runs through each TRAIT, one at a time
    x=colnum[i]  #set the current [i] column as x
    trait=colnames(df)[x] #sets the current column header as the trait name
    print(trait)  #prints the trait name using the column header from previous line
    colnames(df)[x]="y"  #renames the trait variable as "y" for the model analysis below
    #sim.mod1 = lm(y~Entry ,df) #simple model 
    #summary(sim.mod1)
    fe.mod1=lm(y~Rep+SB/Rep+Entry+Rep*Entry, df) #fixed model, y~Rep+SB/Rep+Entry+Rep*Entry
    #fe.mod1=lm(y~Entry*Env,) #alternate fixed model
    anv = anova(fe.mod1) #run the ANOVA on the fixed model
    print(trait)
    anv #ANOVA output
    
    

  }
  outputData <- cbind(outputData,anv$`Pr(>F)`)
  
}



newDF <- as.data.frame(anv$`Pr(>F)`)
colnames(anv$`Pr(>F)`) <- 

