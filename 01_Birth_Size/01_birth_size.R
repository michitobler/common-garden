rm(list = ls())

library(ggplot2)
library(readxl)
library(car)
library(bbmle)
library(effects)
library(MuMIn)
library(gridExtra)
library(cowplot)

# data <- read_excel("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/day_of_birth_measurments.xlsx")[,1:7]
# length(unique(data$Tank))
#avg_size <- ave(data$`Length (mm)`, data$Tank, FUN = mean)
#size_df <- cbind(data, avg_size)
#birth_size <- as.data.frame(cbind(unique(size_df$Tank), unique(size_df$avg_size)))
#colnames(birth_size) <- c("Tank", "Avg_size_mm")
#birth_size$Avg_size_mm <- as.numeric(birth_size$Avg_size_mm)
#colnames(avg_data) <- c("Tank", "Date", "Avg_SL_mm")
#tank.info <- strsplit(birth_size$Tank, "-")

#sites <- c()
##food <- c()
#for (i in 1:length(tank.info)){
#  sites1 <- tank.info[[i]][[2]]
#  sites <- append(sites, sites1)
  
#  food1 <- tank.info[[i]][[3]]
#  food <- append(food, food1)
#}

#size_at_birth_df <- as.data.frame(cbind(birth_size, sites, food))

#ggplot(data = size_at_birth_df, aes(x = sites, y = Avg_size_mm, fill = food)) + 
#  geom_boxplot() + 
#  stat_compare_means(method = "anova") + 
#  ggtitle("Size at Birth (mm)") + 
#  theme_classic()


birthdata <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/brood_size_gonopodium_data.csv", header = T)

#birthdata <- read_excel("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/maternal_effects_data.xlsx")[,1:12]
birthdata <- birthdata[which(birthdata$Date_fry_born > 0),]
birthdata$Date_food_treatment_began <- as.Date(birthdata$Date_food_treatment_began, format = "%m/%d/%y")
birthdata$Date_fry_born <- as.Date(birthdata$Date_fry_born, format = "%m/%d/%y")
birthdata$Date_gonopodium <- as.Date(birthdata$Date_gonopodium, format = "%m/%d/%y")

birthdata$TimeMotherInFoodTreatment.days <- as.numeric(birthdata$Date_fry_born - birthdata$Date_food_treatment_began)
birthdata$TimeMotherInFoodTreatment.days[birthdata$TimeMotherInFoodTreatment.days < 0] <- 0

#birthdata$TimeMotherInFoodTreatment.days <- as.Date(birthdata$Date_fry_born, format = "%m/%d/%y") - as.Date(birthdata$Date_food_treatment_began, format = "%m/%d/%y")

birthdata$FryAgeAtGonopodium.days  <- as.numeric(birthdata$Date_gonopodium - birthdata$Date_fry_born)

#cor(birthdata$Mother_SL_mm, birthdata$Brood_size, use = "complete.obs")

#cor(ave(birthdata$MotherSL.mm, birthdata$TreatGroup, FUN = scale), birthdata$BroodSize, use = "complete.obs")
#birthdata$RelMotherSL <- ave(birthdata$MotherSL.mm, birthdata$TreatGroup, FUN = scale)

#sizemod <- lm(AvgSizeAtBirth.mm ~ Pop + Food + Pop*Food + BroodSize + RelMotherSL, data = birthdata)
##sizemod <- lm(AvgSizeAtBirth.mm ~ Pop + Food + Pop*Food + BroodSize + MotherSL.mm, data = birthdata)
#sizemod <- lm(AvgSizeAtBirth.mm ~ Pop + Food + Pop*Food + MotherSL.mm, data = birthdata)
#sizemod <- lm(AvgSizeAtBirth.mm ~ Pop + Food + Pop*Food, data = birthdata)

#fit <- Anova(sizemod, type = "III")
##fit
#summary.lm(sizemod)

#summary.lm(lm(MotherSL.mm ~ BroodSize, data = birthdata))


total.growth <- read_excel("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/totalGrowth_BON_PSO_Food_Maternal_Effects.xlsx")[,1:6]
agg.growth <- aggregate(total.growth$`Length (mm)`, by = list(total.growth$Tank, total.growth$Date), FUN = "mean")
colnames(agg.growth) <- c("Tank", "Date", "AvgSL.mm")
agg.growth$Tank <- as.character(agg.growth$Tank)
agg.growth$Date <- as.Date(agg.growth$Date)
agg.growth$AvgSL.mm <- as.numeric(agg.growth$AvgSL.mm)

earliest.obs.df <- data.frame(matrix(nrow = length(unique(agg.growth$Tank)), ncol = 3))
colnames(earliest.obs.df) <- c("Tank", "Date", "Avg.SL.mm")


# earliest.obs.df <- data.frame(matrix(nrow = length(unique(agg.growth$Tank)), ncol = 3))
# colnames(earliest.obs.df) <- c("Tank", "Date", "Avg.SL.mm")

for (i in 1:length(unique(agg.growth$Tank)[order(unique(agg.growth$Tank))])){
  print(unique(agg.growth$Tank)[order(unique(agg.growth$Tank))][i])
  curr.data <- agg.growth[which(agg.growth$Tank == unique(agg.growth$Tank)[order(unique(agg.growth$Tank))][i]),]
  curr.min <- curr.data[which(curr.data$Date == min(curr.data$Date)),]
  curr.min$Date <- as.character(curr.min$Date)
  earliest.obs.df[i,] <- curr.min
}

earliest.obs.df$birth.date <- birthdata$Date_fry_born[match(earliest.obs.df$Tank, birthdata$Tank)]
earliest.obs.df$diff.birthdate.earliest.obs <- as.Date(earliest.obs.df$Date, format = "%Y-%m-%d") - as.Date(earliest.obs.df$birth.date, format = "%Y-%m-%d")
size.at.birth <- earliest.obs.df[which(earliest.obs.df$diff.birthdate.earliest.obs < 2),]# == 0),]
size.at.birth$Pop <- substr(size.at.birth$Tank, 4, 6)
size.at.birth$Food.Treatment <- substr(size.at.birth$Tank, 8, 8)
size.at.birth$Time.Mother.in.Food.Treat <- birthdata$TimeMotherInFoodTreatment.days[match(size.at.birth$Tank, birthdata$Tank)]
size.at.birth$Mother.SL.mm <- birthdata$Mother_SL_mm[match(size.at.birth$Tank, birthdata$Tank)]
size.at.birth$Brood.Size <- birthdata$Brood_size[match(size.at.birth$Tank, birthdata$Tank)]
size.at.birth$pop.treat <- paste(size.at.birth$Pop, size.at.birth$Food.Treatment, sep = ".")
PSO <- size.at.birth[which(size.at.birth$Pop == "PSO"),]
PSO$mom.sl.2 <- c()
for (i in 1:nrow(PSO)){
  if (is.na(PSO$Mother.SL.mm[i])){
    PSO$mom.sl.2[i] <- mean(PSO$Mother.SL.mm, na.rm = T)}
  else {PSO$mom.sl.2[i] <- PSO$Mother.SL.mm[i]}
}

BON <- size.at.birth[which(size.at.birth$Pop == "BON"),]
BON$mom.sl.2 <- c()
for (i in 1:nrow(BON)){
  if (is.na(BON$Mother.SL.mm[i])){
    BON$mom.sl.2[i] <- mean(BON$Mother.SL.mm, na.rm = T)}
  else {BON$mom.sl.2[i] <- BON$Mother.SL.mm[i]}
}

new.size.at.birth <- as.data.frame(rbind(PSO, BON))

# Sample Sizes for each group



# null.model <- lm(Avg.SL.mm ~ Time.Mother.in.Food.Treat + Mother.SL.mm + Brood.Size, data = size.at.birth)
# pop.model <- lm(Avg.SL.mm ~ Pop + Time.Mother.in.Food.Treat + Mother.SL.mm + Brood.Size, data = size.at.birth)
# treatment.model <- lm(Avg.SL.mm ~ Food.Treatment + Time.Mother.in.Food.Treat + Mother.SL.mm + Brood.Size, data = size.at.birth)
# pop.plus.treat.model <- lm(Avg.SL.mm ~ Pop + Food.Treatment + Time.Mother.in.Food.Treat + Mother.SL.mm + Brood.Size, data = size.at.birth)
# pop.by.treat.model <- lm(Avg.SL.mm ~ Pop*Food.Treatment + Time.Mother.in.Food.Treat + Mother.SL.mm + Brood.Size, data = size.at.birth)
# 
# AICctab(null.model, pop.model, treatment.model, pop.plus.treat.model, pop.by.treat.model, base = T, weights = T)
# summary(pop.plus.treat.model)
# Anova(pop.plus.treat.model, type = "III")
# plot(Effect("Pop", pop.plus.treat.model))
# plot(Effect("Food.Treatment", pop.plus.treat.model))
# plot(Effect("Time.Mother.in.Food.Treat", pop.plus.treat.model))
# plot(Effect("Mother.SL.mm", pop.plus.treat.model))
# plot(Effect("Brood.Size", pop.plus.treat.model))


ggplot(data = new.size.at.birth, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Avg.SL.mm, fill = interaction(Pop, Food.Treatment))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = "Population x Food Treatment") + 
  theme_classic()

ggplot(data = new.size.at.birth, aes(x = Brood.Size, y = Avg.SL.mm, color = interaction(Pop, Food.Treatment))) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = new.size.at.birth, aes(x = Brood.Size, y = Avg.SL.mm)) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point(aes(color = interaction(Pop, Food.Treatment), size = 1.5)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = new.size.at.birth, aes(x = Mother.SL.mm, y = Avg.SL.mm, color = interaction(Pop, Food.Treatment))) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = new.size.at.birth, aes(x = Mother.SL.mm, y = Avg.SL.mm)) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point(aes(color = interaction(Pop, Food.Treatment), size = 1.5)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = new.size.at.birth, aes(x = Mother.SL.mm, y = Brood.Size)) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point(aes(color = interaction(Pop, Food.Treatment), size = 1.5)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic() + 
  geom_smooth(method = "lm")


#subset.birth <- size.at.birth[which(!is.na(size.at.birth$Mother.SL.mm)),]
global.mod <- lm(Avg.SL.mm ~ Pop * Food.Treatment + mom.sl.2 + Time.Mother.in.Food.Treat + Brood.Size, data = new.size.at.birth, na.action = "na.fail")
#global.mod <- lm(Avg.SL.mm ~ Pop * Food.Treatment + Time.Mother.in.Food.Treat + Brood.Size, data = size.at.birth, na.action = "na.fail")
dredge.out <- dredge(global.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.out, abbrev.names = TRUE)
#write.csv(print(dredge.out, abbrev.names = TRUE), "~/Downloads/birth.size.mod.select.csv")
#model.set <- get.models(dredge.out, subset = weight > 0)
best.size.mod <- lm(Avg.SL.mm ~ Brood.Size + Food.Treatment + mom.sl.2 + Pop, data = new.size.at.birth)
Anova(best.size.mod, type = "III")
heplots::etasq(best.size.mod, partial = T, anova = T)
eta_squared(best.size.mod)
confint(best.size.mod)
#Anova(lm(Avg.SL.mm ~ Brood.Size + Food.Treatment + mom.sl.2 + Pop, data = new.size.at.birth), type = "III")


global.brood.mod <- lm(Brood.Size ~ Pop * Food.Treatment + mom.sl.2 + Time.Mother.in.Food.Treat, data = new.size.at.birth, na.action = "na.fail")
#global.brood.mod <- lm(Brood.Size ~  Pop * Food.Treatment + Time.Mother.in.Food.Treat, data = size.at.birth, na.action = "na.fail")
dredge.brood.out <- dredge(global.brood.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.brood.out, abbrev.names = TRUE)
#write.csv(print(dredge.brood.out, abbrev.names = TRUE), "~/Downloads/brood.size.mod.select.csv")
# m.brood.avg.out <- model.avg(dredge.brood.out)
# summary(m.brood.avg.out)
Anova(lm(Brood.Size ~ Time.Mother.in.Food.Treat + mom.sl.2, data = new.size.at.birth), type = "III")
confint(lm(Brood.Size ~ Time.Mother.in.Food.Treat + mom.sl.2, data = new.size.at.birth))
etasq(lm(Brood.Size ~ Time.Mother.in.Food.Treat + mom.sl.2, data = new.size.at.birth), anova = T, partial = T)

ggplot(data = new.size.at.birth, aes(x = Time.Mother.in.Food.Treat, y = Brood.Size)) + 
  geom_point() + 
  geom_smooth(method = "lm")
size.v.brood.mod <- lm(Avg.SL.mm ~ Brood.Size, data = new.size.at.birth)
summary(size.v.brood.mod)
confint(size.v.brood.mod)
###################
#
#
# Plot fecundity vs offspring size at birth
#
#
####################



# m.avg.out <- model.avg(dredge.out)
# m.avg.out
# summary(m.avg.out)

global.brood.mod <- lm(Brood.Size ~ Pop * Food.Treatment + mom.sl.2 + Time.Mother.in.Food.Treat, data = new.size.at.birth, na.action = "na.fail")
#global.brood.mod <- lm(Brood.Size ~  Pop * Food.Treatment + Time.Mother.in.Food.Treat, data = size.at.birth, na.action = "na.fail")
dredge.brood.out <- dredge(global.brood.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.brood.out, abbrev.names = TRUE)
# m.brood.avg.out <- model.avg(dredge.brood.out)
# summary(m.brood.avg.out)
Anova(lm(Brood.Size ~ Time.Mother.in.Food.Treat + mom.sl.2, data = new.size.at.birth), type = "III")



best.size.mod <- lm(Avg.SL.mm ~ Pop + Food.Treatment + Time.Mother.in.Food.Treat + Brood.Size, data = size.at.birth)
summary(best.size.mod)
Anova(best.size.mod, type = "III")
library(heplots)
etasq(best.size.mod, partial = T, anova = T, type = "III")
confint(best.size.mod)

best.brood.mod <- lm(Brood.Size ~ Pop + Food.Treatment, data = size.at.birth)
Anova(best.brood.mod, type = "III")
confint(best.brood.mod)

write.csv(new.size.at.birth, "~/Downloads/size.at.birth.results.csv")

ggplot(data = size.at.birth, aes(x = Time.Mother.in.Food.Treat, y = Avg.SL.mm, color = interaction(Pop, Food.Treatment))) + 
  geom_point(aes(color = interaction(Pop, Food.Treatment), size = 1.5)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic() + 
  geom_smooth(method = "lm", se = F)
ggplot(data = size.at.birth, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Brood.Size, fill = interaction(Pop, Food.Treatment))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic()

new.standardized.size.at.birth <- new.size.at.birth#[,c("Pop", "Food.Treatment", "Avg.SL.mm", "Brood.Size", "mom.sl.2")]
new.standardized.size.at.birth$zsize <- scale(new.standardized.size.at.birth$Avg.SL.mm)
new.standardized.size.at.birth$zbrood <- scale(new.standardized.size.at.birth$Brood.Size)
# ggplot(data = new.size.at.birth, aes(x = Brood.Size, y = Avg.SL.mm, color = interaction(Pop, Food.Treatment))) + 
#   geom_point(aes(color = interaction(Pop, Food.Treatment))) + 
#   scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
#   geom_smooth(method = "lm") + 
#   theme_classic()
# melt(new.standardized.size.at.birth)
# library(ggplot2)
# library(reshape2)
# 
# df <- data.frame(a = rnorm(10), c = rnorm(10), g = rnorm(10), class = sample(letters[20:23], 10, TRUE))
# df.m <- melt(df)
# ggplot(df.m, aes(class, value, colour = variable)) +
#   geom_point()
# ggplot(data = new.standardized.size.at.birth, aes(x = mom.sl.2, y = zsize)) + 
#   geom_point(aes(size = 1.5, x = mom.sl.2, y = zsize)) + 
#   scale_color_manual(values = c("orange"), name = NULL) + 
#   #geom_point(aes(size = 1.5, x = mom.sl.2, y = zbrood)) +  
#   # scale_color_manual(values = c("grey"), name = NULL) + 
#   theme_classic() + 
#   geom_smooth(aes(x = mom.sl.2, y = zsize), data = new.standardized.size.at.birth, method = "lm", se = F, color = "orange") + 
#   #geom_smooth(aes(x = mom.sl.2, y = zbrood), data = new.standardized.size.at.birth, method = "lm", se = F, color = "grey")
# 
# 
# 
# ggplot() +
#   geom_smooth(aes(x = year, y = slr), data = brest1, 
#               method = "lm", se = FALSE, color = "red") + 
#   geom_smooth(aes(x = year, y = slr), data = brest2, 
#               method = "lm", se = FALSE, color = "blue") + 
#   geom_point(aes(x = year, y = slr), data = brest1, color = "red") + 
#   geom_point(aes(x = year, y = slr), data = brest2, color = "blue")
top.model <- get.models(dredge.out, subset = 4)[[1]]
plot(Effect(c("Food.Treatment", "Pop"), top.model))

# size.at.birth$Pop
# interaction.plot(x.factor = size.at.birth$Pop, 
#                  trace.factor = size.at.birth$Food.Treatment, 
#                  response = size.at.birth$Avg.SL.mm)

# best.mod <- lm(Avg.SL.mm ~ Pop + Food.Treatment + Time.Mother.in.Food.Treat + Brood.Size, data = size.at.birth, na.action = "na.fail")
# best.fit <- Anova(best.mod, type = "II")
# best.fit

#####################################################################################################################
#
#
#           NOT RUN...This is just for plotting something for the SICB presentation
#
#
####################################################################################################################
plot(Effect(c("Pop", "Food.Treatment"), best.size.mod))
#Extract the residual effects
size.ee <- Effect(c("Pop", "Food.Treatment"), best.size.mod)
size.pop.ee <- Effect("Pop", best.size.mod)
#Create a new data frame
size.mms <- as.data.frame(cbind(size.ee$fit, size.ee$se, size.ee$x))
size.pop.mms <- as.data.frame(cbind(size.pop.ee$fit, size.pop.ee$se, size.pop.ee$x))
size.mms.NS <- size.pop.mms$`size.pop.ee$fit`[which(size.pop.mms$Pop == "BON")]
size.mms.S <- size.pop.mms$`size.pop.ee$fit`[which(size.pop.mms$Pop == "PSO")]
size.mms.diff <- (size.mms.S - size.mms.NS) / size.mms.NS * 100

size.food.ee <- Effect("Food.Treatment", best.size.mod)
size.food.mms <- as.data.frame(cbind(size.food.ee$fit, size.food.ee$se, size.food.ee$x))
size.food.mms.H <- size.food.mms$`size.food.ee$fit`[which(size.food.mms$Food.Treatment == "H")]
size.food.mms.L <- size.food.mms$`size.food.ee$fit`[which(size.food.mms$Food.Treatment == "L")]
size.food.mms.diff <- (size.food.mms.L - size.food.mms.H) / size.food.mms.H * 100
size.food.mms.diff

emmeans::emmeans(best.size.mod, 'Pop')
emmeans::emmeans(best.size.mod, 'Food.Treatment')

# best.size.mod <- lm(Avg.SL.mm ~ Pop + Food.Treatment + Time.Mother.in.Food.Treat + Brood.Size, data = size.at.birth)
# summary(best.size.mod)
# Anova(best.size.mod, type = "III")
# library(heplots)
# etasq(best.size.mod, partial = T, anova = T, type = "III")
# confint(best.size.mod)

# 
# #Plot residual effects
# ggplot() + 
#   geom_boxplot(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, color = Food.Treatment), outlier.alpha = 0) + 
#   geom_point(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, color = Food.Treatment), position = position_jitterdodge(), alpha = 0.4) + 
#   geom_pointrange(data = size.mms, aes(x = Pop, y = size.ee$fit, ymin=size.ee$fit-size.ee$se, ymax=size.ee$fit+size.ee$se, color = Food.Treatment), position = position_dodge(1.7), size=1) + 
#   scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
#   theme_classic()
# 
# #geom_pointrange(aes(ymin=sl.ee$fit-sl.ee$se, ymax=sl.ee$fit+sl.ee$se), position = position_dodge(width = -0.2), size=1)
# 
# 
# 
# 
# 
# 
# ggplot(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, color = Food.Treatment)) + 
#   geom_boxplot() + 
#   scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
#   geom_point(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, color = Food.Treatment), position = position_jitterdodge(), alpha = 0.4) + 
#   geom_pointrange(data = size.mms, aes(ymin=size.ee$fit-size.ee$se, ymax=size.ee$fit+size.ee$se), position = position_dodge(1.5), size=1) + 
#   theme_classic()
# 
# 
# 
# 
# ggplot(size.mms, aes(x=Pop, y=size.ee$fit, group=Food.Treatment, color=Food.Treatment)) + 
#   geom_boxplot(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, color = Food.Treatment)) + 
#   geom_point(data=new.size.at.birth, aes(x=Pop, y=Avg.SL.mm, color=Food.Treatment), position = position_jitterdodge(dodge.width = 0), alpha=0.4)+
#   geom_pointrange(aes(ymin=size.ee$fit-size.ee$se, ymax=size.ee$fit+size.ee$se), position = position_dodge(1.5), size=1) +
#   labs(x="Population", y = "Average Standard Length [mm]") +
#   scale_color_manual(values=c("#5DA5DA", "#FAA43A"), name = NULL) +
#   theme_classic()
size.plot <- ggplot() + 
  geom_boxplot(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, color = Food.Treatment), outlier.alpha = 0) + 
  geom_point(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, color = Food.Treatment), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  geom_pointrange(data = size.mms, aes(x = Pop, y = size.ee$fit, ymin=size.ee$fit-size.ee$se, ymax=size.ee$fit+size.ee$se, color = Food.Treatment), position = position_dodge(1.7), size=1) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  theme_classic() + 
  theme(legend.position = "none")
brood.plot <- ggplot() + 
  geom_boxplot(data = new.size.at.birth, aes(x = Pop, y = Brood.Size, color = Food.Treatment), outlier.alpha = 0) + 
  geom_point(data = new.size.at.birth, aes(x = Pop, y = Brood.Size, color = Food.Treatment), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  theme_classic() + 
  theme(legend.position = "none")
size.brood.plot <- ggplot(data = new.size.at.birth, aes(x = Brood.Size, y = Avg.SL.mm)) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point(size = 2.5) + 
  theme_classic() + 
  geom_smooth(method = "lm")
plot_grid(size.plot, brood.plot, size.brood.plot, labels=c("A", "B", "C"), ncol = 3, nrow = 1, respect = T)
pdf("~/Downloads/Figure2_mat_effects.pdf", width = 9, height = 3)
plot_grid(size.plot, brood.plot, size.brood.plot, labels=c("A", "B", "C"), ncol = 3, nrow = 1, respect = T)
dev.off()
#fig2 <- grid.arrange(size.plot, brood.plot, size.brood.plot, nrow = 1, widths = c(1,1,1), heights = 1)












ggplot(data = new.size.at.birth, aes(x = Pop, y = Avg.SL.mm, fill = Food.Treatment)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("orange", "grey"), name = NULL) + 
  theme_classic()

ggplot(data = new.size.at.birth, aes(x = Brood.Size, y = Avg.SL.mm, color = interaction(Pop, Food.Treatment))) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = new.size.at.birth, aes(x = Brood.Size, y = Avg.SL.mm)) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point(aes(color = interaction(Pop, Food.Treatment), size = 1.5)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = new.size.at.birth, aes(x = Mother.SL.mm, y = Avg.SL.mm, color = interaction(Pop, Food.Treatment))) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = "Population x Food Treatment") + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = new.size.at.birth, aes(x = Mother.SL.mm, y = Avg.SL.mm)) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point(aes(color = interaction(Pop, Food.Treatment), size = 1.5)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic() + 
  geom_smooth(method = "lm")
ggplot(data = new.size.at.birth, aes(x = Mother.SL.mm, y = Brood.Size)) + #, color = interaction(Pop, Food.Treatment))) + 
  geom_point(aes(color = interaction(Pop, Food.Treatment), size = 1.5)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) + 
  theme_classic() + 
  geom_smooth(method = "lm")




ee <- Effect(c("Food.Treatment", "Pop"), top.model)
ee.df <- as.data.frame(cbind(ee$x, ee$fit, ee$se))
ee.df$pop.treat <- paste(ee.df$Pop, ee.df$Food.Treatment, sep = ".")
names(ee.df)[3] <- "fit"
names(ee.df)[4] <- "se"


ggplot(sl.mms, aes(x=Cave, y=sl.ee$fit, group=Sex, color=Sex)) + 
  geom_point(data=biometrics.mf, aes(x=Cave, y=Standard.length, color=Sex), position = position_jitterdodge(), alpha=0.4)+
  geom_pointrange(aes(ymin=sl.ee$fit-sl.ee$se, ymax=sl.ee$fit+sl.ee$se), position = position_dodge(width = -0.2), size=1) +
  labs(x="Cave", y = "Standard length [cm]") +
  scale_color_manual(values=c("#FAA43A", "#5DA5DA")) +
  theme_classic()

ggplot(data = size.at.birth, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = ee$fit, color = interaction(Pop, Food.Treatment))) + 
  geom_point(data = size.at.birth, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Avg.SL.mm, color = interaction(Pop, Food.Treatment)), 
             position = position_dodge(width = -0.2), 
             size = 1) + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) + 
  theme_classic()

df.effect <- as.data.frame(cbind(c("BON.H", "BON.L", "PSO.H", "PSO.L"), ee$fit))
names(df.effect) <- c("Treat", "effect")
ggplot(data = size.at.birth, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Avg.SL.mm, color = interaction(Pop, Food.Treatment))) + 
  geom_point() + 
  geom_point(data = df.effect, aes(x = Treat, y = effect), size = 1) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) + 
  theme_classic()

size.at.birth$effect.fit <- c()
for (i in 1:nrow(size.at.birth)){
  if (size.at.birth$pop.treat[i] == "BON.H"){
    size.at.birth$effect.fit[i] <- 8.82163869899259
    size.at.birth$effect.ymin[i] <- 8.82163869899259 - 0.1858048
    size.at.birth$effect.ymax[i] <- 8.82163869899259 + 0.1858048
  }
  if (size.at.birth$pop.treat[i] == "BON.L"){
    size.at.birth$effect.fit[i] <- 9.52175464682006
    size.at.birth$effect.ymin[i] <- 9.52175464682006 - 0.1764921
    size.at.birth$effect.ymax[i] <- 9.52175464682006 + 0.1764921
  }
  if (size.at.birth$pop.treat[i] == "PSO.H"){
    size.at.birth$effect.fit[i] <- 9.72523844682221
    size.at.birth$effect.ymin[i] <- 9.72523844682221 - 0.1784148
    size.at.birth$effect.ymax[i] <- 9.72523844682221 + 0.1784148
  }
  if (size.at.birth$pop.treat[i] == "PSO.L"){
    size.at.birth$effect.fit[i] <- 10.4253543946497
    size.at.birth$effect.ymin[i] <- 10.4253543946497 - 0.2259749
    size.at.birth$effect.ymax[i] <- 10.4253543946497 + 0.2259749
  }
}

ggplot(data = size.at.birth, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Avg.SL.mm, color = interaction(Pop, Food.Treatment), fill = interaction(Pop, Food.Treatment))) + 
  geom_boxplot(width = .5) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold")) +
  geom_point(aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = effect.fit), 
             size = 6, 
             position = position_nudge(x = 0.35)) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold")) +
  geom_pointrange(aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.L", "PSO.H")), y = effect.fit, ymin=effect.ymin, ymax=effect.ymax), 
                  position = position_nudge(x = 0.35), 
                  size=1) +
  theme(legend.position = element_blank()) + 
  #scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) + 
  theme_classic()


ggplot(data = size.at.birth, aes(x = ordered(interaction(Pop, Food.Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Avg.SL.mm, fill = interaction(Pop, Food.Treatment))) + 
  geom_boxplot(width = .5) + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold")) +
  ylim(7.9, 11) + 
  theme_classic()


ggplot(data = ee.df, aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, color = interaction(Pop, Food.Treatment))) + 
  geom_point(size = 6) + 
  ylim(7.9, 11) + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) +  #name = "Population x Food Treatment") + 
  geom_pointrange(aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, ymin = fit - se, ymax = fit + se), 
                  size=1) +
  theme_classic()
