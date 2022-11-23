rm(list = ls())

library(ggplot2)
library(car)
library(bbmle)
library(effects)
library(readxl)

pred.video.info <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/predator_avoidance/predator_avoidance_video_information.csv", header = T)
pred.video.info <- pred.video.info[which(!is.na(pred.video.info$Frame.C.start.began)),]
#pred.video.info <- pred.video.info[which(pred.video.info$Date > 0),]
folder.with.digitized.points <- "~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/predator_avoidance/DLTdv8a_digitized_points/"
pred.data <- list()

for (i in pred.video.info$Video.file){
  x.y.points.path <- paste(folder.with.digitized.points, "DLTdv8_data_", i, "xypts.csv", sep = "")
  print(x.y.points.path)
  x.y.data <- read.csv(x.y.points.path, header = T)
  x.y.data <- x.y.data[which(x.y.data$pt1_cam1_X != "NaN"),]
  dist <- c()
  speed <- c()
  acceleration <- c()
  #angle <- c()
  #diff.from.straight <- c()
  #angular.veloc <- c()
  for (j in 1:nrow(x.y.data)){
    dist[j] <- sqrt((x.y.data$pt1_cam1_X[j] - x.y.data$pt1_cam1_X[j - 1]) ^ 2 + (x.y.data$pt1_cam1_Y[j] - x.y.data$pt1_cam1_Y[j - 1])^2)
    speed[j] <- dist[j] / (1/pred.video.info$Frame.Rate.In.Video.fps[pred.video.info$Video.file == i])
    acceleration[j] <- speed[j] - speed[j - 1]
    #acceleration[j] <- dist[j] / ((1/pred.video.info$Frame.Rate.In.Video.fps[pred.video.info$Video.file == i])^2)
    #d.0.1 <- sqrt((x.y.data$pt1_cam1_X[j] - x.y.data$pt1_cam1_X[j - 1]) ^ 2 + (x.y.data$pt1_cam1_Y[j] - x.y.data$pt1_cam1_Y[j - 1])^2)
    #d.1.2 <- sqrt((x.y.data$pt1_cam1_X[j + 1] - x.y.data$pt1_cam1_X[j]) ^ 2 + (x.y.data$pt1_cam1_Y[j + 1] - x.y.data$pt1_cam1_Y[j])^2)
    #d.0.2 <- sqrt((x.y.data$pt1_cam1_X[j + 1] - x.y.data$pt1_cam1_X[j - 1]) ^ 2 + (x.y.data$pt1_cam1_Y[j + 1] - x.y.data$pt1_cam1_Y[j - 1])^2)
    #angle[j] <- acos((d.0.1^2 + d.1.2^2 - d.0.2^2) / (2 * d.0.1*d.1.2)) #* 180/pi #to convert from radians to degrees
    #diff.from.straight[j] <- pi - angle[j] 
    #angle[j] <- atan((x.y.data$pt1_cam1_Y[j] - x.y.data$pt1_cam1_Y[j - 1]) / (x.y.data$pt1_cam1_X[j] - x.y.data$pt1_cam1_X[j - 1]))
    #angular.veloc[j] <- diff.from.straight[j] / (1/pred.video.info$Frame.Rate.In.Video.fps[pred.video.info$Video.file == i])
  }
  #dist.new <- dist[!is.na(dist)]
  #cumulative.dist <- cumsum(dist.new)
  #cumulative.dist <- c(0, cumulative.dist)
  #pred.data[[i]] <- cbind(x.y.data, dist, cumulative.dist, speed, acceleration, angle, diff.from.straight, angular.veloc)
  pred.data[[i]] <- cbind(x.y.data, dist, speed, acceleration)
}

#### Now some summary stats
max.veloc <- c()
max.accel <- c()
#max.rot.veloc <- c()
net.dist.traveled <- c()
#total.angle.c.start <- c()

frame.C.start.began <- pred.video.info$Frame.C.start.began
frame.C.start.ended <- pred.video.info$Frame.C.start.ended

for (i in 1:length(pred.data)){
  begin <- frame.C.start.began[i]
  end <- frame.C.start.ended[i]
  in.data <- pred.data[[i]][which(rownames(pred.data[[i]]) %in% c(begin:(end + 5))),] # +5 because that is 1/12th of a second after end of C-start
  max.veloc[i] <- max(in.data$speed[which(!is.na(in.data$speed))])
  max.accel[i] <- max(in.data$acceleration[which(!is.na(in.data$acceleration))])
  #max.rot.veloc[i] <- max(in.data$angular.veloc[which(!is.na(in.data$angular.veloc))])
  net.dist.traveled[i] <- sqrt((in.data$pt1_cam1_X[which(as.numeric(rownames(in.data)) == begin)] - in.data$pt1_cam1_X[which(as.numeric(rownames(in.data)) == (end + 5))]) ^ 2 + (in.data$pt1_cam1_Y[which(as.numeric(rownames(in.data)) == begin)] - in.data$pt1_cam1_Y[which(as.numeric(rownames(in.data)) == (end + 5))])^2)
  #total.angle.c.start[i] <- sum(in.data$diff.from.straight[which(as.numeric(rownames(in.data)) == begin) : which(as.numeric(rownames(in.data)) == end)])
}
tanks <- pred.video.info$Tank
treatments <- pred.video.info$Treatment
populations <- pred.video.info$Pop
dates <- pred.video.info$Date

pred.avoid.df <- as.data.frame(cbind(tanks, 
                                     treatments, 
                                     populations,
                                     dates, 
                                     max.veloc, 
                                     max.accel,
                                     net.dist.traveled))
pred.avoid.df$max.veloc <- as.numeric(pred.avoid.df$max.veloc)
pred.avoid.df$max.accel <- as.numeric(pred.avoid.df$max.accel)
#pred.avoid.df$max.rot.veloc <- as.numeric(pred.avoid.df$max.rot.veloc)
pred.avoid.df$net.dist.traveled <- as.numeric(pred.avoid.df$net.dist.traveled)
pred.avoid.df$populations <- as.factor(pred.avoid.df$populations)
pred.avoid.df$tanks <- as.factor(pred.avoid.df$tanks)
pred.avoid.df$treatments <- as.factor(pred.avoid.df$treatments)
#pred.avoid.df$total.angle.c.start <- as.numeric(pred.avoid.df$total.angle.c.start)

ggplot(data = pred.avoid.df,aes(x = max.veloc, y = max.accel, color = interaction(populations, treatments))) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data = pred.avoid.df,aes(x = net.dist.traveled, y = max.accel, color = interaction(populations, treatments))) +
  geom_point() + 
  geom_smooth(method = "lm")

# tanks.avgd <- aggregate(pred.avoid.df[,5:9], 
#                         by = list(pred.avoid.df$tanks, pred.avoid.df$treatments, pred.avoid.df$populations), 
#                         FUN = mean, 
#                         na.action = na.omit)
#tanks.avgd <- tanks.avgd[c(1:18, 20:26),] #removing an outlier to test

#pca <- prcomp(tanks.avgd[,4:8], center = T, scale. = T)
#pca <- prcomp(tanks.avgd[,c("max.veloc", "max.accel", "net.dist.traveled")], center = T, scale. = T)
pca <- prcomp(pred.avoid.df[,c("max.veloc", "max.accel", "net.dist.traveled")], center = T, scale. = T)
print(pca)
summary(pca)
plot(pca, type = "l")
#tanks.avgd <- cbind(tanks.avgd, pca$x)


birth.data <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/brood_size_gonopodium_data.csv", header = T)
birth.data$Pop <- as.factor(birth.data$Pop)
birth.data$Tank <- as.factor(birth.data$Tank)
birth.data$Date_food_treatment_began <- as.Date(as.character(birth.data$Date_food_treatment_began), format = "%m/%d/%y")
birth.data$Date_fry_born <- as.Date(as.character(birth.data$Date_fry_born), format = "%m/%d/%y")
birth.data$Date_gonopodium <- as.Date(as.character(birth.data$Date_gonopodium), format = "%m/%d/%y")

birth.data$days_maternal_treatment_before_birth <- as.numeric(birth.data$Date_fry_born - birth.data$Date_food_treatment_began)
birth.data$days_maternal_treatment_before_birth[which(birth.data$days_maternal_treatment_before_birth < 0)] <- 0

birth.data$fry_age_at_gonopodium <- as.numeric(birth.data$Date_gonopodium - birth.data$Date_fry_born)
birth.data$Tank <- as.factor(birth.data$Tank)
#birth.data <- birth.data[which(birth.data$Date_fry_born > 0),]

#colnames(tanks.avgd)[1:3] <- c("Tank", "Food.Treatment", "Pop")

# tanks.avgd$birth.date <- birth.data$Date_fry_born[match(tanks.avgd$Tank, birth.data$Tank)]
# tanks.avgd$days_mom_in_food_treat <- birth.data$days_maternal_treatment_before_birth[match(tanks.avgd$Tank, birth.data$Tank)]
# tanks.avgd$mom.SL <- birth.data$Mother_SL_mm[match(tanks.avgd$Tank, birth.data$Tank)]
# tanks.avgd$brood.size <- birth.data$Brood_size[match(tanks.avgd$Tank, birth.data$Tank)]

full.pred.avoid.df <- pred.avoid.df
full.pred.avoid.df$birth.date <- birth.data$Date_fry_born[match(full.pred.avoid.df$tanks, birth.data$Tank)]
full.pred.avoid.df$days_mom_in_food_treat <- birth.data$days_maternal_treatment_before_birth[match(full.pred.avoid.df$tanks, birth.data$Tank)]
full.pred.avoid.df$mom.SL <- birth.data$Mother_SL_mm[match(full.pred.avoid.df$tanks, birth.data$Tank)]
full.pred.avoid.df$brood.size <- birth.data$Brood_size[match(full.pred.avoid.df$tanks, birth.data$Tank)]
full.pred.avoid.df$PC1 <- as.numeric(pca$x[,1])

birth.data.edited <- birth.data[which(birth.data$Date_fry_born > 0),]
full.pred.avoid.df$fry.age.at.test <- c(rep(NA, nrow(full.pred.avoid.df)))
for (i in 1:nrow(full.pred.avoid.df)){
  index <- match(full.pred.avoid.df$tanks[i], birth.data.edited$Tank)
  bday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  current.date <- as.Date(full.pred.avoid.df$dates, format = "%m/%d/%y")[i]
  full.pred.avoid.df$fry.age.at.test[i] <- current.date - bday
}

full.pred.avoid.df <- full.pred.avoid.df[which(full.pred.avoid.df$fry.age.at.test >= 0),]

week.of.measure <- c()
for (indiv in 1:nrow(full.pred.avoid.df)){
  if (full.pred.avoid.df$fry.age.at.test[indiv] >= 0 & full.pred.avoid.df$fry.age.at.test[indiv] <= 7){
    week.of.measure[indiv] <- 1
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 7 & full.pred.avoid.df$fry.age.at.test[indiv] <= 14){
    week.of.measure[indiv] <- 2
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 14 & full.pred.avoid.df$fry.age.at.test[indiv] <= 21){
    week.of.measure[indiv] <- 3
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 21 & full.pred.avoid.df$fry.age.at.test[indiv] <= 28){
    week.of.measure[indiv] <- 4
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 28 & full.pred.avoid.df$fry.age.at.test[indiv] <= 35){
    week.of.measure[indiv] <- 5
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 35 & full.pred.avoid.df$fry.age.at.test[indiv] <= 42){
    week.of.measure[indiv] <- 6
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 42 & full.pred.avoid.df$fry.age.at.test[indiv] <= 49){
    week.of.measure[indiv] <- 7
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 49 & full.pred.avoid.df$fry.age.at.test[indiv] <= 56){
    week.of.measure[indiv] <- 8
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 56 & full.pred.avoid.df$fry.age.at.test[indiv] <= 63){
    week.of.measure[indiv] <- 9
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 63 & full.pred.avoid.df$fry.age.at.test[indiv] <= 70){
    week.of.measure[indiv] <- 10
  } else if (full.pred.avoid.df$fry.age.at.test[indiv] > 70 & full.pred.avoid.df$fry.age.at.test[indiv] <= 77){
    week.of.measure[indiv] <- 11
  }
}
full.pred.avoid.df$week.of.measure <- week.of.measure


ggplot(data = full.pred.avoid.df, aes(x = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = interaction(populations, treatments))) +
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold")) + 
  theme_classic()

ggplot(data = full.pred.avoid.df, aes(x = populations, y = PC1, fill = populations)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1")) + 
  theme_classic()

# ggplot(data = tanks.avgd, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = interaction(Pop, Food.Treatment))) + 
#     geom_boxplot() + 
#     scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) + 
#     theme_classic()


ggplot(data = full.pred.avoid.df, aes(x = days_mom_in_food_treat, y = PC1, color = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = full.pred.avoid.df, aes(x = fry.age.at.test, y = PC1, color = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = full.pred.avoid.df, aes(x = fry.age.at.test, y = PC1, color = populations)) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = full.pred.avoid.df, aes(x = fry.age.at.test, y = PC1)) + 
  geom_point() + 
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = full.pred.avoid.df, aes(x = brood.size, y = PC1, color = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = full.pred.avoid.df, aes(x = mom.SL, y = PC1, color = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")



PSO <- full.pred.avoid.df[which(full.pred.avoid.df$populations == "PSO"),]
PSO$mom.sl.2 <- c()
for (i in 1:nrow(PSO)){
  if (is.na(PSO$mom.SL[i])){
    PSO$mom.sl.2[i] <- mean(PSO$mom.SL, na.rm = T)}
  else {PSO$mom.sl.2[i] <- PSO$mom.SL[i]}
}

BON <- full.pred.avoid.df[which(full.pred.avoid.df$populations == "BON"),]
BON$mom.sl.2 <- c()
for (i in 1:nrow(BON)){
  if (is.na(BON$mom.SL[i])){
    BON$mom.sl.2[i] <- mean(BON$mom.SL, na.rm = T)}
  else {BON$mom.sl.2[i] <- BON$mom.SL[i]}
}

new.pred <- as.data.frame(rbind(PSO, BON))

#write.csv(new.pred, "~/Downloads/predator.avoidance.results.csv")

# null.model <- lm(cbind(PC1, PC2) ~ days_mom_in_food_treat + mom.SL + brood.size, data = tanks.avgd)
# pop.model <- lm(cbind(PC1, PC2) ~ Pop + days_mom_in_food_treat + mom.SL + brood.size, data = tanks.avgd)
# treatment.model <- lm(cbind(PC1, PC2) ~ Food.Treatment + days_mom_in_food_treat + mom.SL + brood.size, data = tanks.avgd)
# pop.plus.treat.model <- lm(cbind(PC1, PC2) ~ Pop + Food.Treatment + days_mom_in_food_treat + mom.SL + brood.size, data = tanks.avgd)
# pop.by.treat.model <- lm(cbind(PC1, PC2) ~ Pop*Food.Treatment + days_mom_in_food_treat + mom.SL + brood.size, data = tanks.avgd)
# AICctab(null.model, pop.model, treatment.model, pop.plus.treat.model, pop.by.treat.model, base = T, weights = T)
# Manova(pop.by.treat.model, type = "III")

#global.mod <- lm(cbind(PC1, PC2) ~ days_mom_in_food_treat + brood.size, data = tanks.avgd, na.action = "na.fail")
#global.mod <- lm(PC1 ~ Pop * Food.Treatment + days_mom_in_food_treat + brood.size, data = tanks.avgd, na.action = "na.fail")
global.mod <- lmer(PC1 ~ populations * treatments * fry.age.at.test + mom.sl.2 + days_mom_in_food_treat + brood.size + (1 | tanks), data = new.pred, na.action = "na.fail")
#global.mod <- lmer(PC1 ~ populations * treatments * fry.age.at.test + days_mom_in_food_treat + brood.size + (1 | tanks), data = full.pred.avoid.df, na.action = "na.fail")
dredge.out <- dredge(global.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.out, abbrev.names = TRUE)
#write.csv(print(dredge.out, abbrev.names = TRUE), "~/Downloads/pred.avoid.mod.select.csv")

# m.avg.out <- model.avg(dredge.out)
# m.avg.out
# summary(m.avg.out)

mod <- lmer(PC1 ~ populations + fry.age.at.test + (1 | tanks), na.action = "na.fail", data = new.pred)
#mod <- lm(PC1 ~ populations + fry.age.at.test, data = full.pred.avoid.df)
Anova(mod, type = "III")
confint(mod)
eta_squared(mod)


ggplot(data = new.pred, aes(x = populations, y = PC1, fill = populations)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1"), name = NULL) +
  theme_classic() 
ggplot(data = new.pred, aes(x = fry.age.at.test, y = PC1, color = populations)) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = new.pred, aes(x = fry.age.at.test, y = PC1, color = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")
#Manova(lm(cbind(PC1, PC2) ~ Food.Treatment*Pop, data = tanks.avgd), test.statistic = "Wilks", type = "III")





avoid.0 <- data.frame()
#avoid.all.others <- data.frame()

for (tank in unique(new.pred$tanks)){
  #print(data.zeros.rm$Tank[tank])
  curr.data <- new.pred[which(new.pred$tanks == tank),]
  curr.first.obs <- curr.data[which(curr.data$fry.age.at.test == min(curr.data$fry.age.at.test)),]
  #curr.all.other.obs <- curr.data[which(curr.data$fry.age.at.test != min(curr.data$fry.age.at.test)),]
  avoid.0 <- rbind(avoid.0, curr.first.obs)
  #avoid.all.others <- rbind(avoid.all.others, curr.all.other.obs)
}


ggplot(data = avoid.0, aes(x = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_boxplot() + 
  ggtitle("Predator Avoidance Time 0") + 
  scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = avoid.all.others, aes(x = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = ordered(interaction(populations, treatments), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_boxplot() + 
  ggtitle("Predator Avoidance All other timepoints") + 
  scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

time.0.glob.mod <- lm(PC1 ~ treatments * populations + mom.sl.2 + days_mom_in_food_treat + brood.size, data = avoid.0, na.action = "na.fail")
dredge.0.out <- dredge(time.0.glob.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.0.out, abbrev.names = TRUE)
# t0.avg.out <- model.avg(dredge.0.out)
# summary(t0.avg.out)

best.0.avoid.mod <- lm(PC1 ~ mom.sl.2, data = avoid.0)
Anova(best.0.avoid.mod, type = "III")
confint(best.0.avoid.mod)



time.AllOthers.glob.mod <- lmer(PC1 ~ fry.age.at.test * treatments * populations + days_mom_in_food_treat + brood.size + (1 | tanks), data = avoid.all.others, na.action = "na.fail")
dredge.AllOthers.out <- dredge(time.AllOthers.glob.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.AllOthers.out, abbrev.names = TRUE)
AllOthers.avg.out <- model.avg(dredge.AllOthers.out)
summary(AllOthers.avg.out)

best.AllOthers.avoid.mod <- lmer(PC1 ~ fry.age.at.test + populations + (1 | tanks), data = avoid.all.others, na.action = "na.fail")
Anova(best.AllOthers.avoid.mod, type = "III")
confint(best.AllOthers.avoid.mod)








#####################################################################################################################
#
#
#           NOT RUN...This is just for plotting something for the SICB presentation
#
#
####################################################################################################################

ee <- Effect(c("Food.Treatment", "Pop"), global.mod)
ee.df <- as.data.frame(cbind(ee$x, ee$fit, ee$se))
ee.df$pop.treat <- paste(ee.df$Pop, ee.df$Food.Treatment, sep = ".")
names(ee.df)[3] <- "fit"
names(ee.df)[4] <- "se"

ggplot(data = tanks.avgd, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = interaction(Pop, Food.Treatment))) + 
  geom_boxplot(width = 0.5) + 
  ylim(-4.1, 2.3) + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) + 
  theme_classic() 

ggplot(data = ee.df, aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, color = interaction(Pop, Food.Treatment))) + 
  geom_point(size = 6) + 
  ylim(-4.1, 2.3) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) +  #name = "Population x Food Treatment") + 
  geom_pointrange(aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, ymin = fit - se, ymax = fit + se), 
                  size=1) +
  theme_classic()




#Extract the residual effects
pa.ee <- Effect(c("populations"), mod)

#Create a new data frame
pa.mms <- as.data.frame(cbind(pa.ee$fit, pa.ee$se, pa.ee$x))

#Plot
pred.avoid.plot <- ggplot() + 
  geom_boxplot(data = new.pred, aes(x = populations, y = PC1, color = treatments), outlier.alpha = 0) + 
  geom_point(data = new.pred, aes(x = populations, y = PC1, color = treatments), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  geom_pointrange(data = pa.mms, aes(x = populations, y = pa.ee$fit, ymin=pa.ee$fit-pa.ee$se, ymax=pa.ee$fit+pa.ee$se), position = position_dodge(1.7), size=1) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(-2, 6, by=2), limits = c(-2.75, 6), labels = c("-2.00", "0.00", "2.00", "4.00", "6.00")) + 
  ylab("Burst Swimming Performance (PC1)") + 
  theme(legend.position = "none")
pred.avoid.plot


