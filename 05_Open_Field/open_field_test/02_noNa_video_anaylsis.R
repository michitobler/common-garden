rm(list = ls())
library(ggplot2)
library(gridExtra)
library(stringr)
source("http://peterhaschke.com/Code/multiplot.R")
library(svDialogs)

################################################################################################################################################################################################################################
#
# 1. User Inputs
#
###########################################################################################################################################################################################################################3
scaleDirect <- dlgInput("Enter directory of the scale .csv file: ", Sys.info()["user"])$res
scales <- read.csv(scaleDirect)
fps = 30 #change if not using 60 fps
fileLocation = "F:\\PhD\\Pmex_sulf_nonsulf_food_maternal_effects\\Open_field_test\\Open Field Test\\"
dateOfDirectory <- dlgInput("Enter the date of the data (mm-dd-yy): ", Sys.info()["user"])$res #dlgInput just handles user input so it is a pop-up window prompt, sys.info is referencing type of input (user), $res
#references what the output needs to be 



################################################################################################################################################################################################################
#
#Starting outer for loop that handles reading each traj file in directory
#
################################################################################################################################################################################################################


index = 1
for(tanks in scales$ï..Tank){
  fileLocation = "F:\\PhD\\Pmex_sulf_nonsulf_food_maternal_effects\\Open_field_test\\Open Field Test\\"
  #fileLocation = paste(fileLocation, "\\")
  fileLocation = paste(fileLocation, dateOfDirectory, sep = "")
  fileLocation = paste(fileLocation, "\\", sep = "")
  saveName1 = fileLocation
  fileLocation = paste(fileLocation, dateOfDirectory, sep = "") #This top section is just creating the file path for each tank traj file
  fileLocation = paste(fileLocation, "_", sep = "")
  fileLocation = paste(fileLocation, tanks, sep = "")
  #this variable is for the save name, gets reset each iteration
  fileLocation = paste(fileLocation, " trajectories.txt", sep = "")
  
  
  trajData = read.delim(fileLocation)
  trajData = trajData[,1:2]
  trajData = na.omit(trajData)
  trajxCoor = trajData$X1
  trajyCoor = trajData$Y1
  #Difference
  diffxData = c(0)
  diffyData = c(0)
  diffabsxData = c(0)
  diffabsyData = c(0) #This section is just intializing all columns with 0,0 for calculation purposes
  totdistance = c(0)
  totdistancemm = c(0)
  cummsum = c(0)
  speed = c(0)
  #center = scales$
  
  ################################################################################################################################################################################################################
  #
  #Starting inner for loop that handles calculations
  #
  ################################################################################################################################################################################################################
  for(num in 2:length(trajxCoor)){
    #X Coords
    diffx = (trajxCoor[num] - trajxCoor[num-1]) #calculating change in pixels in the x-direction by subtracting each element in the trajxCoor vector from the element after it
    diffxData = append(diffxData, diffx)
    diffabsxData = append(diffabsxData, abs(diffx))
    
    
    #Y Coords
    diffy = (trajyCoor[num] - trajyCoor[num-1])
    diffyData = append(diffyData, diffy)
    diffabsyData = append(diffabsyData, abs(diffy))
    
    #Total distance
    distance = sqrt((diffabsxData[num]^2) + (diffabsyData[num]^2)) #getting the distance traveled with pythagorean theorem
    totdistance = append(totdistance, distance)
    conversionnum = distance / scales$Scale[index] #converting the distance traveled to mm
    totdistancemm = append(totdistancemm, conversionnum)
    
    indivspeed = totdistancemm[num] * fps #getting the speed by multipling it by the camera FPS used to shoot the footage
    speed = append(speed, indivspeed)
  }
  
  ################################################################################################################################################################################################################
  #
  #Saving results to files
  #
  ################################################################################################################################################################################################################
  index = index + 1 #indexing for referencing the scales
  trajData = cbind(trajData, diffxData, diffyData, diffabsxData, diffabsyData, totdistance, totdistancemm) #putting together all data in a single df
  indivcumsum = cumsum(trajData$totdistancemm) #getting the cummalitive sum for the total distance traveled in
  trajData = cbind(trajData, indivcumsum, speed)
  saveName2 = paste(saveName1, "\\traj_results\\", sep="")
  saveName2 = paste(saveName2, tanks, sep="") #creating the file path for creating the .csv file
  saveName2 = paste(saveName2, dateOfDirectory, sep="_")
  saveName2 = paste(saveName2, "_traj_results.csv", sep="")
  write.csv(trajData, saveName2)
  saveName2 = saveName1 #resetting the save name so it does not just keep appending
  #plot(trajData$speed)
}