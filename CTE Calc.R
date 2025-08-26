#Calculate CTE from data

setwd("~/Desktop/Current Projects/Florence-MS/FlorenceRCode/FinalRCode") #Brandon

library(dplyr)

#Load in data
preCTE <- read.csv("NestTempsPreCTE.csv")

#Make an empty matrix to fill with CTE data
CTE <- data.frame(colnames(preCTE), ncol(preCTE))
      colnames(CTE) <- c("nest", "temp")

for(i in 1:length(CTE$nest)){
  #Get unique values and frequencies

  nest <- preCTE[, i][!is.na(preCTE[, i])] #remove NAs (because of different numbers of values)
  
  unique <- as.data.frame(table(nest)) #Get frequencies
  
  unique$nest <- unique(nest)
  
  unique$gRate <- unique$nest * 2.352 #using loggerhead data, multiply each observed unique temperature by 2.352
  
  unique$OTg <- unique$gRate * unique$nest * unique$Freq #Now multiply the original temperature by the growth rate and the frequency
  
  unique <- unique %>% mutate(rumSum = cumsum(unique$OTg)) #Make a running sum of those values and take the median
  
  less_than_target <- unique$rumSum[unique$rumSum <= median(unique$rumSum)] # Filter numbers less than or equal to the target

  # Check if any numbers meet the condition
  if (length(less_than_target) > 0) {
    # Find the closest number less than the target
    closest_number <- less_than_target[which.min(abs(less_than_target - median(unique$rumSum)
    ))]
  } 

  rows_with_value <- which(unique == closest_number, arr.ind = TRUE)[, "row"]
  
  unique$nest[rows_with_value]
  
  CTE$temp[i] <- unique$nest[rows_with_value]
  
}

write.csv(CTE, "CTE.csv")
