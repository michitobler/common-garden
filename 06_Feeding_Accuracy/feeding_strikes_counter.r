#########################################################
#Script for counting feeding strikes from a text file
#########################################################
rm(list = ls())
library(ggplot2)
library(gridExtra)
library(stringr)
source("http://peterhaschke.com/Code/multiplot.R")
library(svDialogs)

#Should be able to just directly input directory, don't worry about slashes (if on PC not sure about MAC yet :( )
#File names need to follow this format: \XX-XX-XX_XX-XXX-X ex: 07-10-19_01-BON-L
allFileDirectories <- dlgInput("Enter the directory of all feeding strikes .txt files: ", Sys.info()["user"])$res
readyFilesDirectory <- dlgInput("Enter the directory of the .txt file that has all the file names in it: ", Sys.info()["user"])$res
firstChar <- dlgInput("Enter the key used to indicate unsuccessful feeding strikes in the .txt files : ", Sys.info()["user"])$res
secondChar <- dlgInput("Enter the key used to indicate successful feeding strikes in the .txt files : ", Sys.info()["user"])$res
#end of user input

numOfFiles <- length(list.files(allFileDirectories))
numOfFiles = numOfFiles - 1
fileNames = read.delim(readyFilesDirectory, header = T)
fileNames = as.data.frame(fileNames)


#########################################################
#BULK PROCESSING
#########################################################
headers <- c("Date and Tank", "Successful Feeding Strikes", "Unsuccessful Feeding Strikes", "Success Accuracy")
#results <- data.frame(c("Date and Tank"), c("Successful Feeding Strikes"), c("Unsuccessful Feeding Strikes"), c("Success Accuracy"), header=T)
results <- as.data.frame(matrix(nrow=0,ncol=4))
colnames(results) <- headers
for(files in fileNames$Files.names){
  buildingFilePath = allFileDirectories
  buildingFilePath = paste(buildingFilePath, "\\", sep="")
  buildingFilePath = paste(buildingFilePath, files, sep="")
  buildingFilePath = paste(buildingFilePath, ".txt", sep="")
  
  
  ###This WILL throw a warning message but is just a procedural error, does not effect outcome whatsoever
  currWorkingFile = read.delim(buildingFilePath, header = F)
  allTallies <- currWorkingFile[1,1]
  countTallies <- nchar(allTallies)
  
  successCount <- str_count(allTallies, secondChar)
  unsuccessfulCount <- str_count(allTallies, firstChar)
  if(successCount != 0 && unsuccessfulCount != 0){
    accuracyCount <- successCount/unsuccessfulCount
  }else if(successCount != 0){
    accuracyCount <- 100
  }else{
    accuracyCount <- 0
  }

  accuracyCount <- paste(accuracyCount, "%")
  compiledData <- c(files, successCount, unsuccessfulCount, accuracyCount)
  #results <- rbind(results, compiledData)
  results[nrow(results)+1,] <- compiledData
}

setwd(allFileDirectories)
write.csv(results,"Feeding_strikes_anaylsis.csv")









