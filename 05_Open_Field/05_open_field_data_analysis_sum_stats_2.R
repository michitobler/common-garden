rm(list = ls())
library(ggplot2)
#library(gridExtra)
library(stringr)
#source("http://peterhaschke.com/Code/multiplot.R")
#library(svDialogs)
#library(gdata)
library(readxl)
#library(rlang)
#library(gsubfn)
library(data.table)
library(MuMIn)
library(GGally)
library(reshape2)
library(car)
library(dplyr)
library(effects)
library(heplots)


########################################################################################################
#
#
#
########################################################################################################
AllFilesDirectory <- "~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/open_field_test"
#Dates <- str_sub(list.dirs(AllFilesDirectory, recursive = F), -8, -1) #-8 and -1 tell you to take the last eight digits in the string, which corresponds to the date in the path
Dates <- list.dirs(AllFilesDirectory, recursive = F)#[1:8]
fps = 30
scales <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/open_field_test/open_field_test_scales.csv", header = T, na.strings=c("","NA"))
# scales.raw <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/open_field_test/open_field_test_scales.csv", header = T, na.strings=c("","NA"))
# discard.rows <- as.character(rownames(na.omit(scales.raw)))
# scales <- scales.raw[which(!(as.character(rownames(scales.raw)) %in% discard.rows)),]
#scales$tank.date <- paste(scales$Date, scales$Tank, sep = "_")

output.names <- paste(substr(Dates, 107, 115), "open_field_test_output", sep = "_")

output.list <- setNames(vector(length(output.names), mode="list"), output.names)
#for (name in 1:length(output.list)){
#  MultNames <- setNames(vector(length(output.list), mode = "list"), output.list)
#}


for (i in 1:length(Dates)){
  FileNames <- list.files(Dates[i], pattern = "*.txt")
  FileNames <- FileNames[!grepl("foggy", list.files(Dates[i], pattern = "*.txt"))]
  #FileNames <- as.data.frame(list.files(Dates[i], pattern = "*.txt"))
  #NumFiles <- length(FileNames) - 1 
  #overall.output.list <- vector("list", length(Dates))
  curr.output.list <- vector("list", length(FileNames))
  names(curr.output.list) <- FileNames
  
  for (j in 1:length(FileNames)){
    traj <- read.delim(paste(Dates[i], "/", FileNames[j], sep = ""), skipNul = T, row.names = NULL)[,1:2]
    #traj <- traj[!is.na(traj),]
    distance <- c()
    speed <- c()
    accel <- c()
    tot.dist <- c()
    dist.from.center <- c()
    #center <- c(scales$centroidx[j], scales$centroidy[j])
    center <- c(scales$centroidx[match(substr(FileNames[j], 1, 17), scales$Date.Tank)], scales$centroidy[match(substr(FileNames[j], 1, 17), scales$Date.Tank)])
    current.scale <- scales$Scale.pixels.per.mm[match(substr(FileNames[j], 1, 17), scales$Date.Tank)]
    
    for (k in 2:nrow(traj)){
      distance[1] <- 0
      distance[k] <- sqrt((as.numeric(traj$X1[k]) - as.numeric(traj$X1[k - 1]))^2 + (as.numeric(traj$Y1[k]) - as.numeric(traj$Y1[k - 1]))^2) / current.scale #scales$Scale[j]
      speed[k] <- distance[k] * fps
      accel[k] <- speed[k] - speed[k - 1]
      dist.from.center[k] <- sqrt((as.numeric(traj$X1[k]) - center[1])^2 + (as.numeric(traj$Y1[k]) - center[2])^2) / current.scale #scales$Scale[j]
    }
    
    #tot.dist <- cumsum(distance)
    curr.output.list[[j]] <- as.data.frame(cbind(distance, speed, accel, dist.from.center))#, tot.dist))
    
    #overall.output.list[i][[j]] <- curr.output.list
    
  }
  output.list[[i]] <- curr.output.list
}

tank <- c()
trial.date <- c()
pop <- c()
food.treat <- c()
avgspeed <- c()
maxspeed <- c()
minspeed <- c()
#swimming <- c(0)
swimming.time <- c()
#freezing <- c(0)
freezing.time <- c()
#burst.swimming <- c(0)
burst.swimming.time <- c()
tot.dist.traveled <- c()
max.accel <- c()
#exploratory <- c()
avg.d.from.center <- c()
prop.d.from.center <- c()
summary.data.all.tanks <- list()


for (l in 1:length(output.list)){ # loop through all dates
  
  for (m in 1:length(output.list[[l]])){ # loop through all tanks within each date
    input.data <- output.list[[l]][m] #as.data.frame(output.list[[l]][m])
    avgspeed[m] <- mean(input.data[[1]][,2][!is.na(input.data[[1]][,2])])
    #avgspeed[m] <- mean(input.data[,2][!is.na(input.data[,2])])
    #maxspeed[m] <- max(input.data[,2][!is.na(input.data[,2])])
    #minspeed[m] <- min(input.data[,2][!is.na(input.data[,2])])
    maxspeed[m] <- max(input.data[[1]][,2][!is.na(input.data[[1]][,2])])
    max.accel[m] <- max(input.data[[1]][,3][!is.na(input.data[[1]][,3])])
    #minspeed[m] <- min(input.data[[1]][,2][!is.na(input.data[[1]][,2])])
    tank[m] <- str_split(names(output.list[[l]][m]), pattern = "_", simplify = T)[,2]
    #tank[m] <- substr(names(output.list[[l]][m]), 10, 17)
    trial.date[m] <- substr(names(output.list[[l]][m]), 1, 8)
    pop[m] <- substr(names(output.list[[l]][m]), 13, 15)
    food.treat[m] <- substr(names(output.list[[l]][m]), 17, 17)
    tot.dist.traveled[m] <- sum(input.data[[1]][,1][!is.na(input.data[[1]][,1])])
    #input.data <- input.data[[1]][which(!is.na(input.data[[1]][,2])),]
    freezing <- c(0)
    burst.swimming <- c(0)
    swimming <- c(0)
    for (n in 1:length(rownames(input.data[[1]]))){ # loop through each row in the input dataframe
      #if (input.data[,2][n] >= 0 & input.data[,2][n] < 5){
      if (is.na(input.data[[1]]$speed[n])){
        #n = n + 1
        freezing[n] <- 0
        swimming[n] <- 0
        burst.swimming[n] <- 0
      }
      else if (input.data[[1]]$speed[n] >= 0 & input.data[[1]]$speed[n] < 1){
        freezing[n] <- 1
        swimming[n] <- 0
        burst.swimming[n] <- 0
      }
      else if (input.data[[1]]$speed[n] >= 1 & input.data[[1]]$speed[n] < 15){
        freezing[n] <- 0
        swimming[n] <- 1
        burst.swimming[n] <- 0
      }
      else if (input.data[[1]]$speed[n] >= 15){
        freezing[n] <- 0
        swimming[n] <- 0
        burst.swimming[n] <- 1
      }
      
      
    }
    current.diam <- scales$Diameter.mm[match(substr(names(input.data[1]), 1, 17), scales$Date.Tank)]
    freezing.time[m] <- sum(freezing) / fps
    swimming.time[m] <- sum(swimming) / fps
    burst.swimming.time[m] <- sum(burst.swimming) / fps
    avg.d.from.center[m] <- mean(input.data[[1]]$dist.from.center[!is.na(input.data[[1]]$dist.from.center)])
    #exploratory[m] <- 0
    #arena.radius <- scales$ArenaPerimeter / (2 * pi)
    arena.radius <- current.diam / 2
    prop.d.from.center[m] <-  avg.d.from.center[m] / arena.radius # 1 - avg.d.from.center[m] / arena.radius
  }
  summary.data.all.tanks[[l]] <- as.data.frame(cbind(tank, trial.date, pop, 
                                                     food.treat, avgspeed, maxspeed, 
                                                     max.accel, swimming.time, freezing.time, 
                                                     burst.swimming.time, tot.dist.traveled,  
                                                     avg.d.from.center, prop.d.from.center))
  
}
open.field.results <- as.data.frame(rbindlist(summary.data.all.tanks))
open.field.results.each.tank <- distinct(open.field.results)#dplyr::distinct(open.field.results)
open.field.results.each.tank$tank <- as.factor(open.field.results.each.tank$tank)
open.field.results.each.tank$trial.date <- as.Date(open.field.results.each.tank$trial.date, format = "%m-%d-%y")
open.field.results.each.tank$pop <- as.factor(open.field.results.each.tank$pop)
open.field.results.each.tank$food.treat <- as.factor(open.field.results.each.tank$food.treat)
open.field.results.each.tank$avgspeed <- as.numeric(open.field.results.each.tank$avgspeed)
open.field.results.each.tank$maxspeed <- as.numeric(open.field.results.each.tank$maxspeed)
#open.field.results.each.tank$minspeed <- as.numeric(open.field.results.each.tank$minspeed)
open.field.results.each.tank$swimming.time <- as.numeric(open.field.results.each.tank$swimming.time)
open.field.results.each.tank$freezing.time <- as.numeric(open.field.results.each.tank$freezing.time)
open.field.results.each.tank$burst.swimming.time <- as.numeric(open.field.results.each.tank$burst.swimming.time)
open.field.results.each.tank$tot.dist.traveled <- as.numeric(open.field.results.each.tank$tot.dist.traveled)
#open.field.results.each.tank$exploratory <- as.numeric(open.field.results.each.tank$exploratory)
open.field.results.each.tank$avg.d.from.center <- as.numeric(open.field.results.each.tank$avg.d.from.center)
open.field.results.each.tank$prop.d.from.center <- as.numeric(open.field.results.each.tank$prop.d.from.center)
open.field.results.each.tank$max.accel <- as.numeric(open.field.results.each.tank$max.accel)

birth.day.data <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/brood_size_gonopodium_data.csv", header = T)#[1:60,]
birth.day.data <- birth.day.data[which(birth.day.data$Date_fry_born > 0),]
birth.day.data$Pop <- as.factor(birth.day.data$Pop)
birth.day.data$Tank <- as.factor(birth.day.data$Tank)
birth.day.data$Date_food_treatment_began <- as.Date(as.character(birth.day.data$Date_food_treatment_began), format = "%m/%d/%y")
birth.day.data$Date_fry_born <- as.Date(as.character(birth.day.data$Date_fry_born), format = "%m/%d/%y")
#gono_data$Date_gonopodium <- as.Date(as.character(gono_data$Date_gonopodium), format = "%m/%d/%y")
#gono_data <- gono_data[which(gono_data$Date_fry_born > 0),]

birth.day.data$days_maternal_treatment_before_birth <- as.numeric(birth.day.data$Date_fry_born - birth.day.data$Date_food_treatment_began)
birth.day.data$days_maternal_treatment_before_birth[which(birth.day.data$days_maternal_treatment_before_birth < 0)] <- 0


open.field.results.each.tank$bday <- birth.day.data$Date_fry_born[match(open.field.results.each.tank$tank, birth.day.data$Tank)]
open.field.results.each.tank <- open.field.results.each.tank[which(open.field.results.each.tank$bday > 0),]
open.field.results.each.tank$time.mother.in.food.treat <- birth.day.data$days_maternal_treatment_before_birth[match(open.field.results.each.tank$tank, birth.day.data$Tank)]
open.field.results.each.tank$mother.SL.mm <- birth.day.data$Mother_SL_mm[match(open.field.results.each.tank$tank, birth.day.data$Tank)]
open.field.results.each.tank$brood.size <- birth.day.data$Brood_size[match(open.field.results.each.tank$tank, birth.day.data$Tank)]
open.field.results.each.tank$age <- open.field.results.each.tank$trial.date - open.field.results.each.tank$bday
open.field.results.each.tank <- open.field.results.each.tank[which(open.field.results.each.tank$age >= 0),]
#open.field.results.each.tank <- open.field.results.each.tank[which(open.field.results.each.tank$age >= 0),]
#in.df <- open.field.results.each.tank[,c(5,6,8,9,10,11,13)]
# in.df <- open.field.results.each.tank[,c("avgspeed", 
#                                          "maxspeed", 
#                                          "max.accel", 
#                                          "swimming.time", 
#                                          "freezing.time", 
#                                          "burst.swimming.time", 
#                                          "tot.dist.traveled", 
#                                          "prop.d.from.center")]
in.df <- open.field.results.each.tank[,c("avgspeed", 
                                         "maxspeed", 
                                         "max.accel", 
                                         "tot.dist.traveled", 
                                         "prop.d.from.center")]
ggpairs(in.df)

ggplot(open.field.results.each.tank, aes(x = swimming.time, y = tot.dist.traveled, color = food.treat)) + 
  geom_point()
ggplot(open.field.results.each.tank, aes(x = swimming.time, y = tot.dist.traveled, color = pop)) + 
  geom_point()
ggplot(open.field.results.each.tank, aes(x = swimming.time, y = freezing.time, color = trial.date)) + 
  geom_point()


ggplot(open.field.results.each.tank, aes(x = trial.date, y = burst.swimming.time)) + #, color = food.treat)) + 
  geom_point()
ggplot(open.field.results.each.tank, aes(x = age, y = swimming.time)) + #, color = food.treat)) + 
  geom_point()
ggplot(open.field.results.each.tank, aes(x = time.mother.in.food.treat, y = swimming.time)) + #, color = food.treat)) + 
  geom_point()

ggplot(open.field.results.each.tank, aes(x = tot.dist.traveled, y = swimming.time, color = interaction(pop, food.treat))) + 
  geom_point() + 
  geom_smooth(method = "lm")

open.field.pca <- prcomp(in.df, center = T, scale. = T)

# Sample sizes for each group
sum(interaction(open.field.results.each.tank$pop, open.field.results.each.tank$food.treat) == "BON.H")
sum(interaction(open.field.results.each.tank$pop, open.field.results.each.tank$food.treat) == "BON.L")
sum(interaction(open.field.results.each.tank$pop, open.field.results.each.tank$food.treat) == "PSO.H")
sum(interaction(open.field.results.each.tank$pop, open.field.results.each.tank$food.treat) == "PSO.L")

length(grep("BON-H", unique(open.field.results.each.tank$tank)))
length(grep("BON-L", unique(open.field.results.each.tank$tank)))
length(grep("PSO-H", unique(open.field.results.each.tank$tank)))
length(grep("PSO-L", unique(open.field.results.each.tank$tank)))


#open.field.pca <- prcomp(open.field.results.each.tank[,c(5,8,11,13)], center = T, scale. = T)
##open.field.pca <- prcomp(open.field.results.each.tank[,c(5,10,11,13)], center = T, scale. = T)
summary(open.field.pca)
print(open.field.pca)
plot(open.field.pca, type = "l")

open.field.results.each.tank <- cbind(open.field.results.each.tank, open.field.pca$x)

ggplot(data = open.field.results.each.tank, aes(x = PC1, y = PC2, color = interaction(pop, food.treat))) + 
  geom_point() + 
  stat_ellipse() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

#open.field.results.each.tank <- open.field.results.each.tank[which(open.field.results.each.tank$PC1 > -6 & open.field.results.each.tank$PC1 < 7),]
open.field.results.each.tank <- open.field.results.each.tank[which(open.field.results.each.tank$PC2 > -6 & open.field.results.each.tank$PC2 < 6),]

week.of.measure <- c()
for (indiv in 1:nrow(open.field.results.each.tank)){
  if (open.field.results.each.tank$age[indiv] >= 0 & open.field.results.each.tank$age[indiv] <= 7){
    week.of.measure[indiv] <- 1
  } else if (open.field.results.each.tank$age[indiv] > 7 & open.field.results.each.tank$age[indiv] <= 14){
    week.of.measure[indiv] <- 2
  } else if (open.field.results.each.tank$age[indiv] > 14 & open.field.results.each.tank$age[indiv] <= 21){
    week.of.measure[indiv] <- 3
  } else if (open.field.results.each.tank$age[indiv] > 21 & open.field.results.each.tank$age[indiv] <= 28){
    week.of.measure[indiv] <- 4
  } else if (open.field.results.each.tank$age[indiv] > 28 & open.field.results.each.tank$age[indiv] <= 35){
    week.of.measure[indiv] <- 5
  } else if (open.field.results.each.tank$age[indiv] > 35 & open.field.results.each.tank$age[indiv] <= 42){
    week.of.measure[indiv] <- 6
  } else if (open.field.results.each.tank$age[indiv] > 42 & open.field.results.each.tank$age[indiv] <= 49){
    week.of.measure[indiv] <- 7
  } else if (open.field.results.each.tank$age[indiv] > 49 & open.field.results.each.tank$age[indiv] <= 56){
    week.of.measure[indiv] <- 8
  } else if (open.field.results.each.tank$age[indiv] > 56 & open.field.results.each.tank$age[indiv] <= 63){
    week.of.measure[indiv] <- 9
  }
}
open.field.results.each.tank$week.of.measure <- week.of.measure
# open.field.results.each.tank <- open.field.results.each.tank[which(as.numeric(rownames(open.field.results.each.tank)) != 40),]
# open.field.results.each.tank <- open.field.results.each.tank[which(as.numeric(rownames(open.field.results.each.tank)) != 53),]
# open.field.results.each.tank <- open.field.results.each.tank[which(as.numeric(rownames(open.field.results.each.tank)) != 57),]
# open.field.results.each.tank <- open.field.results.each.tank[which(as.numeric(rownames(open.field.results.each.tank)) != 77),]

ggplot(data = open.field.results.each.tank, aes(x = pop, y = avgspeed, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = pop, y = maxspeed, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()


ggplot(data = open.field.results.each.tank, aes(x = pop, y = swimming.time, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = swimming.time, color = interaction(pop, food.treat))) + 
  geom_density() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = pop, y = freezing.time, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = freezing.time, color = interaction(pop, food.treat))) + 
  geom_density() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = pop, y = burst.swimming.time, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = pop, y = tot.dist.traveled, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = pop, y = avg.d.from.center, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = pop, y = prop.d.from.center, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = swimming.time)) + 
  geom_histogram()
ggplot(data = open.field.results.each.tank, aes(x = freezing.time)) + 
  geom_histogram()
ggplot(data = open.field.results.each.tank, aes(x = burst.swimming.time)) + 
  geom_histogram()



#open.field.pca <- prcomp(open.field.results.each.tank[,c(5,8,9,10,11,13)], center = T, scale. = T)




ggplot(data = open.field.results.each.tank, aes(x = PC1, y = PC2, color = pop)) + 
  geom_point() + 
  stat_ellipse() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = PC2, y = PC3, color = interaction(pop,food.treat))) + 
  geom_point() + 
  stat_ellipse() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()


ggplot(data = open.field.results.each.tank, aes(x = PC1, color = interaction(pop, food.treat))) + 
  geom_density() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = PC2, color = interaction(pop, food.treat))) + 
  geom_density() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = PC3, color = interaction(pop, food.treat))) + 
  geom_density() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = PC1, color = pop)) + 
  geom_density() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue", "gold"), name = NULL, labels = NULL) + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC2, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = NULL, labels = NULL) + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC3, fill = interaction(pop, food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = NULL, labels = NULL) + 
  theme_classic()

ggplot(data = open.field.results.each.tank, aes(x = age, y = PC1, color = interaction(pop,food.treat))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = open.field.results.each.tank, aes(x = age, y = PC2, color = interaction(pop,food.treat))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = open.field.results.each.tank, aes(x = age, y = PC3, color = interaction(pop,food.treat))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")


ggplot(data = open.field.results.each.tank, aes(x = mother.SL.mm, y = PC1, color = interaction(pop,food.treat))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = open.field.results.each.tank, aes(x = mother.SL.mm, y = PC2, color = interaction(pop,food.treat))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = open.field.results.each.tank, aes(x = mother.SL.mm, y = PC3, color = interaction(pop,food.treat))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "dodgerblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = open.field.results.each.tank, aes(x = age, y = PC1, color = pop)) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1")) + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = open.field.results.each.tank, aes(x = age, y = PC1, color = interaction(pop, food.treat))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue", "gold")) + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = open.field.results.each.tank, aes(x = age, y = PC1)) + 
  geom_point() + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggpairs(in.df)

ggplot(data = open.field.results.each.tank, aes(x = ordered(interaction(pop,food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = interaction(pop,food.treat))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue", "gold"), name = "Population x Food Treatment") + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = pop, y = PC1, fill = pop)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1")) + 
  theme_classic()
ggplot(data = open.field.results.each.tank, aes(x = food.treat, y = PC1, fill = food.treat)) + 
  geom_boxplot() + 
  theme_classic()
# null.model <- lmer(PC1 ~ time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# pop.model <- lmer(PC1 ~ pop + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# treatment.model <- lmer(PC1 ~ food.treat + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# age.model <- lmer(PC1 ~ age + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# pop.plus.treat.model <- lmer(PC1 ~ pop + food.treat + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# pop.plus.age.model <- lmer(PC1 ~ pop + age + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# age.plus.treat.model <- lmer(PC1 ~ age + food.treat + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# pop.by.treat.model <- lmer(PC1 ~ pop*food.treat + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# pop.by.age.model <- lmer(PC1 ~ pop*age + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# age.by.treat.model <- lmer(PC1 ~ age*food.treat + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# pop.by.age.by.treat.model <- lmer(PC1 ~ pop*food.treat*age + time.mother.in.food.treat + brood.size + (1 | tank), data = open.field.results.each.tank)
# #AICctab(null.model, pop.model, treatment.model, 
# #        age.model, pop.plus.treat.model, pop.plus.age.model, 
# #        age.plus.treat.model, pop.by.treat.model, pop.by.age.model, 
# #        age.by.treat.model, pop.by.age.by.treat.model, base = T, weights = T)
# #Anova(pop.model, type = "III")
# #Anova(null.model, type = "III")
# #summary(pop.by.treat.model)
# 
# 
# null.model2 <- lmer(PC1 ~ (1 | tank), data = open.field.results.each.tank)
# pop.model2 <- lmer(PC1 ~ pop + (1 | tank), data = open.field.results.each.tank)
# treatment.model2 <- lmer(PC1 ~ food.treat + (1 | tank), data = open.field.results.each.tank)
# age.model2 <- lmer(PC1 ~ age + (1 | tank), data = open.field.results.each.tank)
# pop.plus.treat.model2 <- lmer(PC1 ~ pop + food.treat + (1 | tank), data = open.field.results.each.tank)
# pop.plus.age.model2 <- lmer(PC1 ~ pop + age + (1 | tank), data = open.field.results.each.tank)
# age.plus.treat.model2 <- lmer(PC1 ~ age + food.treat + (1 | tank), data = open.field.results.each.tank)
# pop.by.treat.model2 <- lmer(PC1 ~ pop*food.treat + (1 | tank), data = open.field.results.each.tank)
# pop.by.age.model2 <- lmer(PC1 ~ pop*age + (1 | tank), data = open.field.results.each.tank)
# age.by.treat.model2 <- lmer(PC1 ~ age*food.treat + (1 | tank), data = open.field.results.each.tank)
# pop.by.age.by.treat.model2 <- lmer(PC1 ~ pop*food.treat*age + (1 | tank), data = open.field.results.each.tank)
# 
# plot(Effect(c("pop", "food.treat"), pop.by.treat.model2))
# plot(Effect(c("food.treat", "pop"), pop.by.treat.model2))
# 
# AICctab(null.model, pop.model, treatment.model, 
#         age.model, pop.plus.treat.model, pop.plus.age.model, 
#         age.plus.treat.model, pop.by.treat.model, pop.by.age.model, 
#         age.by.treat.model, pop.by.age.by.treat.model, 
#         null.model2, pop.model2, treatment.model2, 
#         age.model2, pop.plus.treat.model2, pop.plus.age.model2, 
#         age.plus.treat.model2, pop.by.treat.model2, pop.by.age.model2, 
#         age.by.treat.model2, pop.by.age.by.treat.model2, base = T, weights = T)
# Anova(pop.model2, type = "III")

PSO <- open.field.results.each.tank[which(open.field.results.each.tank$pop == "PSO"),]
PSO$mom.sl.2 <- c()
for (i in 1:nrow(PSO)){
  if (is.na(PSO$mother.SL.mm[i])){
    PSO$mom.sl.2[i] <- mean(PSO$mother.SL.mm, na.rm = T)}
  else {PSO$mom.sl.2[i] <- PSO$mother.SL.mm[i]}
}

BON <- open.field.results.each.tank[which(open.field.results.each.tank$pop == "BON"),]
BON$mom.sl.2 <- c()
for (i in 1:nrow(BON)){
  if (is.na(BON$mother.SL.mm[i])){
    BON$mom.sl.2[i] <- mean(BON$mother.SL.mm, na.rm = T)}
  else {BON$mom.sl.2[i] <- BON$mother.SL.mm[i]}
}

new.explore <- as.data.frame(rbind(PSO, BON))

#write.csv(new.explore, "~/Downloads/exploratory.behavior.results.csv")


global.mod1 <- lm(PC1 ~ pop * food.treat * age + time.mother.in.food.treat + mom.sl.2 + brood.size + tank, data = new.explore, na.action = "na.fail")
#global.mod1 <- lmer(PC1 ~ pop * food.treat * age + time.mother.in.food.treat + mom.sl.2 + brood.size + (1 | tank), data = new.explore, na.action = "na.fail")
dredge.out1 <- dredge(global.mod1, rank = "AICc", m.lim = c(1, 4))
print(dredge.out1, abbrev.names = TRUE)
best.explore.mod1 <- lm(PC1 ~ tank, data = new.explore)#write.csv(print(dredge.out, abbrev.names = TRUE), "~/Downloads/explore.mod.select.csv")
Anova(best.explore.mod1)
etasq(best.explore.mod1)
# m.avg.out <- model.avg(dredge.out)
# m.avg.out
# summary(m.avg.out)

#interaction is non-significant, so using additive model instead
best.explore.mod1 <- lme4::lmer(PC1 ~ pop + food.treat + (1 | tank), data = new.explore, na.action = "na.fail")
#anova(best.explore.mod1)
summary(best.explore.mod1)
#best.explore.mod <- lmer(PC1 ~ pop * food.treat + (1 | tank), data = new.explore, na.action = "na.fail")
car::Anova(best.explore.mod1, type = "III")
parameters::model_parameters(best.explore.mod1, effects = "fixed", df_method = "wald")
#effectsize::t_to_eta2(t = c(2.74, 2.06), df_error = c(117, 117))
effectsize::eta_squared(best.explore.mod1, partial = T)
#etasq(best.explore.mod1, anova = T, partial = T)
confint(best.explore.mod1)



#best.explore.mod2 <- lmer(PC2 ~ pop + food.treat + (1 | tank), data = new.explore, na.action = "na.fail")

#best.explore.mod <- lmer(PC1 ~ pop * food.treat + (1 | tank), data = new.explore, na.action = "na.fail")
# Anova(best.explore.mod2, type = "III")
# confint(best.explore.mod2)


print(open.field.pca)
summary(open.field.pca)
open.field.pca.eigenvectors <- as.data.frame(open.field.pca$rotation)
open.field.pca.eigenvalues <- open.field.pca$sdev^2

open.field.pca.loadings <- data.frame(matrix(nrow = nrow(open.field.pca.eigenvectors), ncol = ncol(open.field.pca.eigenvectors)))
for (i in 1:length(colnames(open.field.pca.eigenvectors))){
  open.field.pca.loadings[,i] <- open.field.pca.eigenvectors[,i] * sqrt(open.field.pca.eigenvalues[i])
}
#m.avg.out <- model.avg(dredge.out)
#m.avg.out
#summary(m.avg.out)





explore.0 <- data.frame()
#explore.all.others <- data.frame()

for (tank in unique(new.explore$tank)){
  #print(data.zeros.rm$Tank[tank])
  curr.data <- new.explore[which(new.explore$tank == tank),]
  curr.first.obs <- curr.data[which(curr.data$age == min(curr.data$age)),]
  #curr.all.other.obs <- curr.data[which(curr.data$age != min(curr.data$age)),]
  explore.0 <- rbind(explore.0, curr.first.obs)
  #explore.all.others <- rbind(explore.all.others, curr.all.other.obs)
}


ggplot(data = explore.0, aes(x = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_boxplot() + 
  ggtitle("Exploratory Behavior Time 0") + 
  scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL) + 
  theme_classic()

# ggplot(data = explore.all.others, aes(x = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
#   geom_boxplot() + 
#   ggtitle("Exploratory Behavior All other timepoints") + 
#   scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = "Population x Food Treatment") + 
#   theme_classic()

time.0.glob.mod <- lm(PC1 ~ food.treat * pop + time.mother.in.food.treat + mom.sl.2 + brood.size, data = explore.0, na.action = "na.fail")
dredge.0.out <- dredge(time.0.glob.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.0.out, abbrev.names = TRUE)
# t0.avg.out <- model.avg(dredge.0.out)
# summary(t0.avg.out)

best.0.explore.mod <- lm(PC1 ~ food.treat, data = explore.0)
Anova(best.0.explore.mod, type = "III")
confint(best.0.explore.mod)


#Extract the residual effects
eb.ee <- Effect(c("pop", "food.treat"), best.explore.mod1)
eb.pop.ee <- Effect("pop", best.explore.mod1)
eb.food.ee <- Effect("food.treat", best.explore.mod1)

#Create a new data frame
eb.mms <- as.data.frame(cbind(eb.ee$fit, eb.ee$se, eb.ee$x))
eb.pop.mms <- as.data.frame(cbind(eb.pop.ee$fit, eb.pop.ee$se, eb.pop.ee$x))
eb.food.mms <- as.data.frame(cbind(eb.food.ee$fit, eb.food.ee$se, eb.food.ee$x))

eb.mms.NS <- eb.pop.mms$`eb.pop.ee$fit`[which(eb.pop.mms$pop == "BON")]
eb.mms.S <- eb.pop.mms$`eb.pop.ee$fit`[which(eb.pop.mms$pop == "PSO")]
eb.mms.diff <- (eb.mms.S - eb.mms.NS) / eb.mms.NS * 100
eb.mms.diff

eb.food.H <- eb.food.mms$`eb.food.ee$fit`[which(eb.food.mms$food.treat == "H")]
eb.food.L <- eb.food.mms$`eb.food.ee$fit`[which(eb.food.mms$food.treat == "L")]
eb.food.diff <- eb.food.L - eb.food.H
eb.food.diff


options(digits = 3)
break.points <- seq(-2.50, 6.50, by=2.50)
explore.plot <- ggplot() + 
  geom_boxplot(data = new.explore, aes(x = pop, y = PC1, color = food.treat), outlier.alpha = 0) + 
  geom_point(data = new.explore, aes(x = pop, y = PC1, color = food.treat), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  geom_pointrange(data = eb.mms, aes(x = pop, y = eb.ee$fit, ymin=eb.ee$fit-eb.ee$se, ymax=eb.ee$fit+eb.ee$se, color = food.treat), position = position_dodge(1.7), size=1) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(-2.50, 6.50, by=2.50), limits = c(-2.50, 6.50), labels = c("-2.50", "0.00", "2.50", "5.00")) + 
  #ylim(c(-2.5,6.5)) + 
  ylab("Exploratory Behavior (PC1)") + 
  theme(legend.position = "none")

explore.plot






#####################################################################################################################
#
#
#           NOT RUN...This is just for plotting something for the SICB presentation
#
#
####################################################################################################################

model <- get.models(dredge.out, subset = 2)[[1]]
ee <- Effect(c("food.treat", "pop"), model)
ee.df <- as.data.frame(cbind(ee$x, ee$fit, ee$se))
ee.df$pop.treat <- paste(ee.df$pop, ee.df$food.treat, sep = ".")
names(ee.df)[3] <- "fit"
names(ee.df)[4] <- "se"


ggplot(data = open.field.results.each.tank, aes(x = ordered(interaction(pop, food.treat), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = interaction(pop, food.treat))) + 
  geom_boxplot(width = 0.5) + 
  ylim(-2.6, 3.3) + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) + 
  theme_classic()


ggplot(data = ee.df, aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, color = interaction(pop, food.treat))) + 
  geom_point(size = 6) + 
  ylim(-2.6, 3.3) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) +  #name = "Population x Food Treatment") + 
  geom_pointrange(aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, ymin = fit - se, ymax = fit + se), 
                  size=1) +
  theme_classic()



