
df <- read.csv("August_Extracted_Data.csv")

#split data into df Days
Day6_data <- subset(df, df$Day == "6")
Day9_data <- subset(df, df$Day == "9")
Day12_data <- subset(df, df$Day == "12")

Day6_data <- Day6_data[,17:39]
Day9_data <- Day9_data[,17:39]
Day12_data <- Day12_data[,17:39]

tmp <- do.call(data.frame, 
               list(mean = apply(df, 2, mean),
                    sd = apply(df, 2, sd),
                    median = apply(df, 2, median),
                    min = apply(df, 2, min),
                    max = apply(df, 2, max),
                    n = apply(df, 2, length)))
tmp



install.packages("fBasics")
library(fBasics)
basicstatsDay6_data <- basicStats(Day6_data)
basicstatsDay6_data <- t(basicstatsDay6_data)

basicstatsDay9_data <- basicStats(Day9_data)
basicstatsDay9_data <- t(basicstatsDay9_data)

basicstatsDay12_data <- basicStats(Day12_data)
basicstatsDay12_data <- t(basicstatsDay12_data)

combinedstats <- rbind(basicstatsDay6_data, basicstatsDay9_data, basicstatsDay12_data)

write.csv(combinedstats,"C:/Users/falk/Google Drive/PhD/PhD Projects/Blue Steel/2017 Data - Growth Chamber/Maturity Group 2 ONLY data/Basic_stats_combined.csv")

