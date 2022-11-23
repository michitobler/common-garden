rm(list = ls())

library(ggplot2)
library(readxl)
library(car)
library(lubridate)
library(tidyverse)
library(R.utils)
library(lme4)
library(bbmle)
library(effects)
library(MuMIn)

#this line enters in data for you to use.
#test.df <- data.frame(pop = rep(rep("BON", 3), rep("PSO", 3), 3), treat = rep(c("H", "L"), 9), date1 = NA, age1 = NA, sizeage1 = rnorm(18, mean = 10, sd = 1), date2 = NA, age2 = NA, sizeage2 = rnorm(18, mean = 12, sd = 1))


data <- read_excel("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/totalGrowth_BON_PSO_Food_Maternal_Effects.xlsx")[,1:7]
unique(data$Tank)
length(unique(data$Tank))

birth.data.df <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/brood_size_gonopodium_data.csv", header = T)
birth.data.edited <- birth.data.df[which(birth.data.df$Date_fry_born > 0),]
#birthdaydata <- read_excel("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/day_of_birth_measurments.xlsx")[,1:7]
#birthdays <- birthdaydata[!duplicated(birthdaydata$Tank),]

#data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data$Date <- as.character(data$Date)
avg_data <- aggregate(data$`Length (mm)`, by = list(data$Tank, data$Date), FUN = mean)
colnames(avg_data) <- c("Tank", "Date", "Avg_SL_mm")
length(unique(avg_data$Date))

avg_data_reordered <- avg_data[order(avg_data$Tank),]
avg_data_reordered$Pop <- substr(avg_data_reordered$Tank, 4, 6)
avg_data_reordered$Treatment <- substr(avg_data_reordered$Tank, 8, 8)
avg_data_reordered$Pop_x_Treat <- paste(avg_data_reordered$Pop, "-", avg_data_reordered$Treatment, sep = "")
avg_data_reordered$Growth <- c(rep(NA, nrow(avg_data_reordered)))
avg_data_reordered$Growth[1] <- 0

for (i in 1:nrow(avg_data_reordered)){
  if (avg_data_reordered$Tank[i+1] == avg_data_reordered$Tank[i]){
    avg_data_reordered$Growth[i+1] = avg_data_reordered$Avg_SL_mm[i+1] - avg_data_reordered$Avg_SL_mm[i]
  }
  else {
    avg_data_reordered$Growth[i+1] = 0
  }
}


avg_data_reordered$Age <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  index <- match(avg_data_reordered$Tank[i], birth.data.edited$Tank)
  bday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  current.date <- as.Date(avg_data_reordered$Date, format = "%Y-%m-%d")[i]
  avg_data_reordered$Age[i] <- current.date - bday
}


avg_data_reordered$Birthday <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  index <- match(avg_data_reordered$Tank[i], birth.data.edited$Tank)
  #birthday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  birthday <- birth.data.edited$Date_fry_born[index]
  birthday <- as.character(as.Date(birthday, format = "%m/%d/%y"))
  avg_data_reordered$Birthday[i] <- birthday
}


avg_data_reordered$brood.size <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  index <- match(avg_data_reordered$Tank[i], birth.data.edited$Tank)
  #birthday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  brood <- birth.data.edited$Brood_size[index]
  avg_data_reordered$brood.size[i] <- brood
}


avg_data_reordered$mom.sl <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  index <- match(avg_data_reordered$Tank[i], birth.data.edited$Tank)
  #birthday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  sl <- birth.data.edited$Mother_SL_mm[index]
  avg_data_reordered$mom.sl[i] <- sl
}


avg_data_reordered$date.food.treatment.began <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  index <- match(avg_data_reordered$Tank[i], birth.data.edited$Tank)
  #birthday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  date.food <- birth.data.edited$Date_food_treatment_began[index]
  date.food <- as.character(as.Date(date.food, format = "%m/%d/%y"))
  avg_data_reordered$date.food.treatment.began[i] <- date.food
}


avg_data_reordered$days.mother.in.food.treatment <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  index <- match(avg_data_reordered$Tank[i], birth.data.edited$Tank)
  #birthday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  mom.date <- birth.data.edited$Date_food_treatment_began[index]
  mom.date <- as.character(as.Date(mom.date, format = "%m/%d/%y"))
  birth.date <- birth.data.edited$Date_fry_born[index]
  birth.date <- as.character(as.Date(birth.date, format = "%m/%d/%y"))
  num.days <- as.Date(birth.date) - as.Date(mom.date)
  avg_data_reordered$days.mother.in.food.treatment[i] <- num.days
}
#avg_data_reordered$days.mother.in.food.treatment <- as.Date(avg_data_reordered$gonopodium_date, format = "%Y-%m-%d") - as.Date(avg_data_reordered$Birthday, format = "%Y-%m-%d")


avg_data_reordered$gonopodium_date <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  index <- match(avg_data_reordered$Tank[i], birth.data.edited$Tank)
  #birthday <- as.Date(birth.data.edited$Date_fry_born, format = "%m/%d/%y")[index]
  gono.date <- birth.data.edited$Date_gonopodium[index]
  gono.date <- as.character(as.Date(gono.date, format = "%m/%d/%y"))
  avg_data_reordered$gonopodium_date[i] <- gono.date
}
avg_data_reordered$age.at.gonopodium.days <- as.Date(avg_data_reordered$gonopodium_date, format = "%Y-%m-%d") - as.Date(avg_data_reordered$Birthday, format = "%Y-%m-%d")



# some offspring weren't measured on their birthday, so we need the earliest observation, not necessarily size at birth
first.observations <- c()
tab <- table(avg_data_reordered$Tank)
for (i in unique(avg_data_reordered$Tank)){
  #first.obs <- avg_data_reordered$Avg_SL_mm[which(min(avg_data_reordered$Date) == unique(avg_data_reordered$Tank)[i])]
  first.obs <- avg_data_reordered$Avg_SL_mm[min(which(avg_data_reordered$Tank == i))]
  first.observations <- append(first.observations, first.obs)
}
earliest.size <- c()
for (i in 1:length(tab)){
  observations <- rep(first.observations[i], tab[[i]])
  earliest.size <- append(earliest.size, observations)
}
avg_data_reordered$earliest.size.measurement.mm <- earliest.size



#avg_data_reordered$Size.at.birth.mm <- c(rep(NA, nrow(avg_data_reordered)))
#for (i in 1:nrow(avg_data_reordered)){
  #index <- match(avg_data_reordered$Date[i], avg_data_reordered$Birthday)
#  size <- avg_data_reordered$Avg_SL_mm
#  avg_data_reordered$Size.at.birth.mm[i] <- size
#}


avg_data_reordered$Time.diff.days <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  if (avg_data_reordered$Growth[i] != 0){
    diff <- avg_data_reordered$Age[i] - avg_data_reordered$Age[i - 1]
    avg_data_reordered$Time.diff.days[i] <- diff
  } else{
    avg_data_reordered$Time.diff.days[i] <- NA
  }
}



avg_data_reordered$Rel.Growth.Rate.body.lengths.per.day <- c(rep(NA, nrow(avg_data_reordered)))
for (i in 1:nrow(avg_data_reordered)){
  #rate <- (avg_data_reordered$Avg_SL_mm[i+1] - avg_data_reordered$Avg_SL_mm[i]) / avg_data_reordered$Avg_SL_mm
  #rate <- ((avg_data_reordered$Avg_SL_mm[i] - avg_data_reordered$earliest.size.measurement.mm[i]) / avg_data_reordered$earliest.size.measurement.mm[i]) / avg_data_reordered$Age[i]
  rate <- ((avg_data_reordered$Avg_SL_mm[i + 1] - avg_data_reordered$Avg_SL_mm[i]) / avg_data_reordered$Avg_SL_mm[i]) / avg_data_reordered$Time.diff.days[i + 1]
  #avg_data_reordered$Rel.Growth.Rate.body.lengths.per.day[i] <- rate
  avg_data_reordered$Rel.Growth.Rate.body.lengths.per.day[i + 1] <- rate
}

earliest.measurement <- avg_data_reordered[which(avg_data_reordered$Growth == 0),]
data.zeros.rm <- avg_data_reordered[which(avg_data_reordered$Date != avg_data_reordered$Birthday & avg_data_reordered$Rel.Growth.Rate.body.lengths.per.day != 0),]
data.zeros.rm <- data.zeros.rm[which(data.zeros.rm$Rel.Growth.Rate.body.lengths.per.day < .2 & data.zeros.rm$Rel.Growth.Rate.body.lengths.per.day > -.05),]



week.of.measure <- c()
for (indiv in 1:nrow(data.zeros.rm)){
  if (data.zeros.rm$Age[indiv] >= 0 & data.zeros.rm$Age[indiv] <= 7){
    week.of.measure[indiv] <- 1
  } else if (data.zeros.rm$Age[indiv] > 7 & data.zeros.rm$Age[indiv] <= 14){
    week.of.measure[indiv] <- 2
  } else if (data.zeros.rm$Age[indiv] > 14 & data.zeros.rm$Age[indiv] <= 21){
    week.of.measure[indiv] <- 3
  } else if (data.zeros.rm$Age[indiv] > 21 & data.zeros.rm$Age[indiv] <= 28){
    week.of.measure[indiv] <- 4
  } else if (data.zeros.rm$Age[indiv] > 28 & data.zeros.rm$Age[indiv] <= 35){
    week.of.measure[indiv] <- 5
  } else if (data.zeros.rm$Age[indiv] > 35 & data.zeros.rm$Age[indiv] <= 42){
    week.of.measure[indiv] <- 6
  } else if (data.zeros.rm$Age[indiv] > 42 & data.zeros.rm$Age[indiv] <= 49){
    week.of.measure[indiv] <- 7
  } else if (data.zeros.rm$Age[indiv] > 49 & data.zeros.rm$Age[indiv] <= 56){
    week.of.measure[indiv] <- 8
  } else if (data.zeros.rm$Age[indiv] > 56 & data.zeros.rm$Age[indiv] <= 63){
    week.of.measure[indiv] <- 9
  } else if (data.zeros.rm$Age[indiv] > 63 & data.zeros.rm$Age[indiv] <= 70){
    week.of.measure[indiv] <- 10
  } else if (data.zeros.rm$Age[indiv] > 70 & data.zeros.rm$Age[indiv] <= 77){
    week.of.measure[indiv] <- 11
  } else if (data.zeros.rm$Age[indiv] > 77 & data.zeros.rm$Age[indiv] <= 84){
    week.of.measure[indiv] <- 12
  } else if (data.zeros.rm$Age[indiv] > 84 & data.zeros.rm$Age[indiv] <= 91){
    week.of.measure[indiv] <- 13
  } else if (data.zeros.rm$Age[indiv] > 91 & data.zeros.rm$Age[indiv] <= 98){
    week.of.measure[indiv] <- 14
  } else if (data.zeros.rm$Age[indiv] > 98 & data.zeros.rm$Age[indiv] <= 105){
    week.of.measure[indiv] <- 15
  } else if (data.zeros.rm$Age[indiv] > 105 & data.zeros.rm$Age[indiv] <= 112){
    week.of.measure[indiv] <- 16
  } else if (data.zeros.rm$Age[indiv] > 112 & data.zeros.rm$Age[indiv] <= 119){
    week.of.measure[indiv] <- 17
  } else if (data.zeros.rm$Age[indiv] > 119 & data.zeros.rm$Age[indiv] <= 126){
    week.of.measure[indiv] <- 18
  } else if (data.zeros.rm$Age[indiv] > 126 & data.zeros.rm$Age[indiv] <= 133){
    week.of.measure[indiv] <- 19
  } else if (data.zeros.rm$Age[indiv] > 133 & data.zeros.rm$Age[indiv] <= 140){
    week.of.measure[indiv] <- 20
  } else if (data.zeros.rm$Age[indiv] > 140 & data.zeros.rm$Age[indiv] <= 147){
    week.of.measure[indiv] <- 21
  }
}
data.zeros.rm$week.of.measure <- week.of.measure




ggplot(data = data.zeros.rm, aes(x = Age, y = log(Rel.Growth.Rate.body.lengths.per.day + .01), group = Tank, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point(size = 3) + 
  ylim(-6, -2) + 
  #geom_smooth() +
  geom_smooth(aes(group = interaction(Pop, Treatment)), 
              method = "lm", 
              formula = y ~ x, 
              se = F, 
              size = 2.5) + 
  theme(legend.position = "none") + 
  #geom_line() + 
  #ylim(-.025, .15) + 
  #geom_smooth(method = "nls", 
  #            formula = Rel.Growth.Rate.body.lengths.per.day ~ Age)
  scale_color_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL) + 
  theme_classic()
#ggplot(data = data.zeros.rm[data.zeros.rm$Rel.Growth.Rate.body.lengths.per.day > -0.4 & data.zeros.rm$Rel.Growth.Rate.body.lengths.per.day < .15,], aes(x = Age, y = Rel.Growth.Rate.body.lengths.per.day, group = Tank, color = Pop_x_Treat)) + 
#  geom_point() + 
#  geom_line() + 
#  theme_classic()

ggplot(data = data.zeros.rm, aes(x = Age, y = Rel.Growth.Rate.body.lengths.per.day, group = Tank, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  geom_line() + 
  #geom_smooth(aes(group = interaction(Pop, Treatment)), 
  #            method = "lm", 
  #            formula = y ~ x, 
  #            se = F) + 
  scale_color_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = data.zeros.rm, aes(x = Age, y = Avg_SL_mm, group = Tank, color = Pop_x_Treat)) + 
  geom_point() + 
  geom_line() + 
  theme_classic() # remove PSO-H outlier

ggplot(data = data.zeros.rm, aes(x = Age, y = Growth, group = Tank, color = Pop_x_Treat)) + 
  geom_point() + 
  geom_line() + 
  theme_classic()

#exp.mod <- lm(log(data.zeros.rm$Rel.Growth.Rate.body.lengths.per.day + 1) ~ data.zeros.rm$Age)
#summary(exp.mod)
#t <- seq(1, length(data.zeros.rm$Age), 1)
#exp.counts <- exp(predict(exp.mod,list(Age=t)))
#plot(data.zeros.rm$Age, data.zeros.rm$Rel.Growth.Rate.body.lengths.per.day)
#lines(t, exp.counts,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")


#ggplot(data = avg_data_reordered, aes(x = Age, y = Growth, group = Tank, color = Pop_x_Treat)) + 
#  geom_point() + 
#  geom_line()

#ggplot(data = avg_data_reordered, aes(x = Age, y = Avg_SL_mm, group = Tank, color = Pop_x_Treat)) + 
#  geom_point() + 
#  geom_line()

#ggplot(data = avg_data_reordered, aes(x = Age, y = Growth, group = Pop_x_Treat, color = Pop_x_Treat)) + 
#  geom_point() + 
#  geom_smooth(method = "lm", formula = y ~ x)

#ggplot(data = avg_data_reordered[which(avg_data_reordered$Age <= 45),], aes(x = Age, y = Growth, group = Pop_x_Treat, color = Pop_x_Treat)) + 
#  geom_point() + 
#  geom_smooth(method = "lm", formula = y ~ x)


# mixed models
data.zeros.rm$Tank <- as.factor(data.zeros.rm$Tank)
data.zeros.rm$Pop <- as.factor(data.zeros.rm$Pop)
data.zeros.rm$Treatment <- as.factor(data.zeros.rm$Treatment)
data.zeros.rm$brood.size <- as.numeric(data.zeros.rm$brood.size)
data.zeros.rm$mom.sl <- as.numeric(data.zeros.rm$mom.sl)
data.zeros.rm$age.at.gonopodium.days <- as.numeric(data.zeros.rm$age.at.gonopodium.days)

# null model
# null <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ days.mother.in.food.treatment + brood.size + mom.sl + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# 
# # Population
# mod1 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Pop + days.mother.in.food.treatment + brood.size + mom.sl + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod1)
# 
# # Treatment
# mod2 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Treatment + days.mother.in.food.treatment + brood.size + mom.sl + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod2)
# 
# # Pop x Treatment interaction
# mod3 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age + days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod3)
# 
# mod4 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Pop*Treatment +  days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod4)
# 
# mod5 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Pop*Age +  days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod5)
# 
# mod6 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age*Treatment +  days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod6)
# 
# mod7 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age*Treatment*Pop +  days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod7)
# 
# mod8 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Pop+Treatment +  days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod8)
# 
# mod9 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Pop+Age + days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod9)
# 
# mod10 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age+Treatment +  days.mother.in.food.treatment + brood.size + mom.sl  + age.at.gonopodium.days + (1 | Tank), data = data.zeros.rm)
# anova(null, mod10)
# 
# mod11 <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age + Treatment + Pop +  days.mother.in.food.treatment + brood.size + mom.sl  +age.at.gonopodium.days +  (1 | Tank), data = data.zeros.rm)
# anova(null, mod11)
# 
# 
# AICctab(null, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, base = T, weights = T)
# summary(mod3)
# Anova(mod3, type = "III")
# # try putting age into models, then pop*age, pop*treatment, age*treatment, pop*age*treatment, pop + age + treatment
# # make models, run AIC, summary models
# summary(null)
# 
# 
# ggplot(data.zeros.rm, aes(x=Rel.Growth.Rate.body.lengths.per.day)) + geom_histogram() + theme_classic() #+ geom_boxplot() + theme_classic() #+ geom_smooth(method=lm, se=TRUE)
# ggplot(data.zeros.rm, aes(x=Pop, y=Rel.Growth.Rate.body.lengths.per.day)) + geom_violin() + geom_point(position = "jitter") + theme_classic() #+ geom_boxplot() + theme_classic() #+ geom_smooth(method=lm, se=TRUE)
# ggplot(data.zeros.rm, aes(x=Pop, y=Rel.Growth.Rate.body.lengths.per.day)) + geom_violin() + geom_point(aes(color = Treatment), position = "jitter") + theme_classic() #+ geom_boxplot() + theme_classic() #+ geom_smooth(method=lm, se=TRUE)
# 
# ggplot(data.zeros.rm, aes(x=mom.sl, y=Rel.Growth.Rate.body.lengths.per.day)) + geom_point() + theme_classic() + geom_smooth()#+ geom_boxplot() + theme_classic() #+ geom_smooth(method=lm, se=TRUE)
# 
# ggplot(data.zeros.rm, aes(x=days.mother.in.food.treatment, y=Rel.Growth.Rate.body.lengths.per.day)) + geom_point() + theme_classic() + geom_smooth()#+ geom_boxplot() + theme_classic() #+ geom_smooth(method=lm, se=TRUE)
# ggplot(data.zeros.rm, aes(x=brood.size, y=Rel.Growth.Rate.body.lengths.per.day)) + geom_point() + theme_classic() + geom_smooth()#+ geom_boxplot() + theme_classic() #+ geom_smooth(method=lm, se=TRUE)
# 
# plot(Effect(c("Age"),mod3))
# plot(Effect(c("Pop", "Treatment"),mod3))
# plot(Effect("Pop", mod1))
# plot(Effect("Treatment", mod2))
# plot(Effect("days.mother.in.food.treatment", null))
# plot(Effect("brood.size", null))
# plot(Effect(c("days.mother.in.food.treatment", "brood.size"), null))
# 
# Pop.x.Treat.effect <- Effect(c("Pop", "Treatment"),mod3) 
#theme_set(theme_bw())
#ggplot(as.data.frame(Pop.x.Treat.effect),
#       aes(Pop,fit,color=Treatment))+
#  geom_line() +geom_ribbon(colour=NA,alpha=0.1,
#                           aes(ymin=lower,ymax=upper))
## colour=NA suppresses edges of the ribbon
#+
  ## add rug plot based on original data
#  geom_rug(data=ee$data,aes(y=NULL),sides="b")

PSO <- data.zeros.rm[which(data.zeros.rm$Pop == "PSO"),]
PSO$mom.sl.2 <- c()
for (i in 1:nrow(PSO)){
  if (is.na(PSO$mom.sl[i])){
    PSO$mom.sl.2[i] <- mean(PSO$mom.sl, na.rm = T)}
  else {PSO$mom.sl.2[i] <- PSO$mom.sl[i]}
}

BON <- data.zeros.rm[which(data.zeros.rm$Pop == "BON"),]
BON$mom.sl.2 <- c()
for (i in 1:nrow(BON)){
  if (is.na(BON$mom.sl[i])){
    BON$mom.sl.2[i] <- mean(BON$mom.sl, na.rm = T)}
  else {BON$mom.sl.2[i] <- BON$mom.sl[i]}
}

new.growth <- as.data.frame(rbind(PSO, BON))
new.growth$Rel.Growth.Rate.body.lengths.per.day <- new.growth$Rel.Growth.Rate.body.lengths.per.day + 0.01
#write.csv(new.growth, "~/Downloads/growth.rate.res.csv")

global.mod <- lmer(log10(Rel.Growth.Rate.body.lengths.per.day) ~ Age * Treatment * Pop + mom.sl.2 + days.mother.in.food.treatment + brood.size + (1 | Tank), data = new.growth, na.action = "na.fail")
#global.mod <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age * Treatment * Pop +  days.mother.in.food.treatment + brood.size + (1 | Tank), data = data.zeros.rm, na.action = "na.fail")
dredge.out <- dredge(global.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.out, abbrev.names = TRUE)
#write.csv(print(dredge.out, abbrev.names = TRUE), "~/Downloads/growth.rate.mod.select.csv")
# summary(dredge.out)
# m.avg.out <- model.avg(dredge.out)
# summary(m.avg.out)





#best.growth.mod <- lmer(Rel.Growth.Rate.body.lengths.per.day ~ Age + (1 | Tank), data = new.growth)
best.growth.mod <- lmer(log10(Rel.Growth.Rate.body.lengths.per.day) ~ Age + (1 | Tank), data = new.growth)
Anova(best.growth.mod, type = "III")
confint(best.growth.mod)
parameters::model_parameters(best.growth.mod, effects = "fixed")
effectsize::t_to_eta2(t = -12.16, df_error = 143)
effectsize::eta_squared(best.growth.mod, partial = T)

x <- new.growth$Rel.Growth.Rate.body.lengths.per.day + .01
y <- log10(x)
z <- 10^(y)
cbind(x,y,z)

#first.measurements <- c()
#all.other.measurements.avgd <- c()
growth.0 <- data.frame()
#colnames(growth.0) <- colnames(data.zeros.rm)
#colnames(growth.all.others) <- colnames(data.zeros.rm)

# for (tank in 1:length(unique(data.zeros.rm$Tank))){
#   print(data.zeros.rm$Tank[tank])
#   curr.tank <- as.character(data.zeros.rm$Tank[tank])
#   curr.data <- data.zeros.rm[which(data.zeros.rm$Tank == curr.tank),]
#   curr.first.obs <- curr.data[which(curr.data$Age == min(curr.data$Age)),]
#   curr.all.other.obs <- curr.data[which(curr.data$Age != min(curr.data$Age)),]
#   growth.0[tank,] <- curr.first.obs
#   growth.all.others <- rbind(growth.all.others, curr.all.other.obs)
# }

for (tank in unique(new.growth$Tank)){
  #print(data.zeros.rm$Tank[tank])
  curr.data <- new.growth[which(new.growth$Tank == tank),]
  curr.first.obs <- curr.data[which(curr.data$Age == min(curr.data$Age)),]
  # curr.oldest.obs <- curr.data[which(curr.data$Age == max(curr.data$Age)),]
  growth.0 <- rbind(growth.0, curr.first.obs)
  # growth.oldest <- rbind(growth.oldest, curr.oldest.obs)
}

# size.oldest <- data.frame()
# for (tank in unique(avg_data_reordered$Tank)){
#   #print(data.zeros.rm$Tank[tank])
#   curr.data <- avg_data_reordered[which(avg_data_reordered$Tank == tank),]
#   curr.last.obs <- curr.data[which(curr.data$Age == max(curr.data$Age)),]
#   # curr.oldest.obs <- curr.data[which(curr.data$Age == max(curr.data$Age)),]
#   size.oldest <- rbind(size.oldest, curr.last.obs)
#   # growth.oldest <- rbind(growth.oldest, curr.oldest.obs)
# }
ggplot(data = growth.0, aes(x = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Rel.Growth.Rate.body.lengths.per.day, fill = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_boxplot() + 
  ggtitle("Time 0") + 
  scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL) + 
  theme_classic()


# ggplot(data = growth.all.others, aes(x = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Rel.Growth.Rate.body.lengths.per.day, fill = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
#   geom_boxplot() + 
#   ggtitle("All other timepoints") + 
#   scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = "Population x Food Treatment") + 
#   theme_classic()
# 
# 
# ggplot(data = size.oldest, aes(x = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Avg_SL_mm, fill = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
#   geom_boxplot() + 
#   ggtitle("Size at Oldest Measurement") + 
#   scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL) + 
#   theme_classic()
# 
# ggplot(data = size.oldest, aes(x = Age, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
#   geom_density() + 
#   ggtitle("Distribution of Ages When Last Size Measurement Taken") + 
#   scale_color_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL) + 
#   theme_classic()

time.0.glob.mod <- lm(log10(Rel.Growth.Rate.body.lengths.per.day) ~ Treatment * Pop + mom.sl.2 + days.mother.in.food.treatment + brood.size, data = growth.0, na.action = "na.fail")
dredge.0.out <- dredge(time.0.glob.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.0.out, abbrev.names = TRUE)
# t0.avg.out <- model.avg(dredge.0.out)
# summary(t0.avg.out)

best.0.growth.mod <- lm(log10(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Pop, data = growth.0)
Anova(best.0.growth.mod, type = "III")
etasq(best.0.growth.mod, type = "III")
confint(best.0.growth.mod)

# time.AllOthers.glob.mod <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age * Treatment * Pop +  days.mother.in.food.treatment + brood.size + (1 | Tank), data = growth.all.others, na.action = "na.fail")
# dredge.AllOthers.out <- dredge(time.AllOthers.glob.mod, rank = "AICc", m.lim=c(1,4))
# print(dredge.AllOthers.out, abbrev.names = TRUE)
# AllOthers.avg.out <- model.avg(dredge.AllOthers.out)
# summary(AllOthers.avg.out)

# best.AllOthers.growth.mod <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age + Pop + (1 | Tank), data = growth.all.others, na.action = "na.fail")
# Anova(best.AllOthers.growth.mod, type = "III")


# oldest.global.mod <- lm(Avg_SL_mm ~ Treatment * Pop * Age + days.mother.in.food.treatment + brood.size, data = size.oldest, na.action = "na.fail")
# dredge.oldest.out <- dredge(oldest.global.mod, rank = "AICc", m.lim=c(1,4))
# print(dredge.oldest.out, abbrev.names = TRUE)
# oldest.avg.out <- model.avg(dredge.oldest.out)
# summary(oldest.avg.out)
# 
# best.oldest.size.mod <- lm(Avg_SL_mm ~ Age + brood.size + Treatment, data = size.oldest)
# Anova(best.oldest.size.mod, type = "III")
# confint(best.oldest.size.mod)
#############
#
#
# Is size at maturity different?
#
#
##############

# summary(lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age + (1 | Tank), data = data.zeros.rm, na.action = "na.fail"))
# best.growth.mod <- lmer(log(Rel.Growth.Rate.body.lengths.per.day + .01) ~ Age + (1 | Tank), data = data.zeros.rm, na.action = "na.fail")
# Anova(best.growth.mod, type = "III")
# confint(best.growth.mod)








summary(best.0.growth.mod)
best.0.growth.mod <- lm(log10(Rel.Growth.Rate.body.lengths.per.day) ~ Pop, data = growth.0)
plot(Effect(c("Age"), best.growth.mod))
#Extract the residual effects
growth.ee <- Effect(c("Age"), best.growth.mod)
growth.birth.ee <- Effect(c("Pop"), best.0.growth.mod)

growth.mms <- as.data.frame(cbind(growth.ee$fit, growth.ee$se, growth.ee$x))
growth.mms

emmeans::emmeans(best.growth.mod, 'Age')

emmeans::emmeans(best.0.growth.mod, "Pop")
growth.birth.mms <- as.data.frame(cbind(growth.birth.ee$fit, growth.birth.ee$se, growth.birth.ee$x))
growth.birth.mms
10^(growth.birth.mms$`growth.birth.ee$fit`)


#Create a new data frame
#mat.birth.mms <- as.data.frame(cbind(mat.birth.ee$fit, mat.birth.ee$se, mat.birth.ee$x))

#Plot
growth.birth.plot <- ggplot() + 
  geom_boxplot(data = growth.0, aes(x = Pop, y = Rel.Growth.Rate.body.lengths.per.day, color = Treatment), outlier.alpha = 0) + 
  geom_point(data = growth.0, aes(x = Pop, y = Rel.Growth.Rate.body.lengths.per.day, color = Treatment), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  geom_pointrange(data = growth.birth.mms, aes(x = Pop, y = 10^(growth.birth.ee$fit), ymin=10^(growth.birth.ee$se), ymax=10^(growth.birth.ee$se)), position = position_dodge(1.7), size=1) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  ylab("Growth Rate (body lengths per day) at Birth") + 
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 0.1, by=0.02), limits = c(-.01, 0.1)) + 
  theme(legend.position = "none")
growth.birth.plot

growth.plot <- ggplot() + 
  geom_boxplot(data = new.growth, aes(x = Pop, y = Rel.Growth.Rate.body.lengths.per.day, color = Treatment), outlier.alpha = 0) + 
  geom_point(data = new.growth, aes(x = Pop, y = Rel.Growth.Rate.body.lengths.per.day, color = Treatment), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  geom_pointrange(data = mat.birth.mms, aes(x = Pop, y = mat.birth.ee$fit, ymin=mat.birth.ee$fit-mat.birth.ee$se, ymax=mat.birth.ee$fit+mat.birth.ee$se), position = position_dodge(1.7), size=1) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  ylab("Overall Growth Rate (body lengths per day)") + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(0, 0.1, by=0.02), limits = c(-.01, 0.1)) + 
  theme(legend.position = "none")
growth.plot

ggplot() + 
  geom_boxplot(data = new.growth, aes(x = Pop, y = log10(Rel.Growth.Rate.body.lengths.per.day + 0.01), color = Treatment), outlier.alpha = 0) + 
  geom_point(data = new.growth, aes(x = Pop, y = log10(Rel.Growth.Rate.body.lengths.per.day + 0.01), color = Treatment), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  #geom_pointrange(data = mat.birth.mms, aes(x = Pop, y = mat.birth.ee$fit, ymin=mat.birth.ee$fit-mat.birth.ee$se, ymax=mat.birth.ee$fit+mat.birth.ee$se), position = position_dodge(1.7), size=1) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  ylab("Overall Growth Rate (log10-body lengths per day)") + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(-2, -1, by=0.5), limits = c(-2.05, -0.9)) + 
  theme(legend.position = "none")

ggplot(data = new.growth, aes(x = Age, y = Rel.Growth.Rate.body.lengths.per.day)) + 
  geom_point()
ggplot(data = new.growth, aes(x = Age, y = log10(Rel.Growth.Rate.body.lengths.per.day + 0.01))) + 
  geom_point()
ggplot(data = new.growth, aes(x = Age, y = exp(Rel.Growth.Rate.body.lengths.per.day))) + 
  geom_point()
