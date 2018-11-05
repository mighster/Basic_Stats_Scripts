# Description: outlier function found online
# https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
# by Kevin Falk
# May 2018

#I developed a script to identify, describe, plot and remove the outliers if it is necessary. 
#To detect the outliers I use the command boxplot.stats()$out which use the Tukey's method to 
#identify the outliers ranged above and below the 1.5*IQR. To describe the data I preferred to show the number (%) of outliers and the mean of the outliers in dataset. I also show the mean of data with and without outliers. Regarding the plot, I think that boxplot and histogram are the best for presenting the outliers. In the script below, I will plot the data with and without the outliers. Finally, with help from Selva, I added a question (yes/no) to ask whether to keep or remove the outliers in data. If the answer is yes then outliers will be replaced with NA.
###############################################################################################

outlierKD(Day6_Entry3,Primaryrootlength)

###outlier function
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0)) #building the plots
  boxplot(var_name, main="With outliers") #build the boxplot
  hist(var_name, main="With outliers", xlab=NA, ylab=NA) #build the histogram
  outlier <- boxplot.stats(var_name)$out #To detect the outliers I use the command boxplot.stats()$out which use the Tukey's method to identify the outliers ranged above and below the 1.5*IQR. 
  #OUT is any points that lie beyond the extremes of the whiskers
  mo <- mean(outlier) 
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

subdf = df[,6:length(df)]
subdf1 = apply(subdf,2,outlierKD)
str(subdf)

for(i in 1:300){
  outlierKD(Day6_Entry164,lengthSLbyPL)


#ONLY DAY 6
for(i in 1:300){
  nam<- paste("Day6_Entry", i, sep = "")
  assign(nam,subset(day6_data, day6_data$Entry == i)) #### replace df with whatever data frame contains all your information
}

