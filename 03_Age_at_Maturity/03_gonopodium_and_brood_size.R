rm(list = ls())

library(ggplot2)
library(readxl)
library(car)
library(lme4)
library(bbmle)
library(effects)
library(MuMIn)

gono_data <- read.csv("~/Desktop/Projects/2019_Maternal_Effects_Pmex_Food_Population/02_Raw_Data/life_history/brood_size_gonopodium_data.csv", header = T)
gono_data$Pop <- as.factor(gono_data$Pop)
gono_data$Tank <- as.factor(gono_data$Tank)
gono_data$Date_food_treatment_began <- as.Date(as.character(gono_data$Date_food_treatment_began), format = "%m/%d/%y")
gono_data$Date_fry_born <- as.Date(as.character(gono_data$Date_fry_born), format = "%m/%d/%y")
gono_data$Date_gonopodium <- as.Date(as.character(gono_data$Date_gonopodium), format = "%m/%d/%y")
gono_data <- gono_data[which(gono_data$Date_fry_born > 0),]

gono_data$days_maternal_treatment_before_birth <- as.numeric(gono_data$Date_fry_born - gono_data$Date_food_treatment_began)
gono_data$days_maternal_treatment_before_birth[which(gono_data$days_maternal_treatment_before_birth < 0)] <- 0

gono_data$fry_age_at_gonopodium <- as.numeric(gono_data$Date_gonopodium - gono_data$Date_fry_born)
gono_data <- gono_data[which(gono_data$fry_age_at_gonopodium > 0),]
gono_data$Tank <- as.factor(gono_data$Tank)
gono_data_subset <- gono_data[which(!is.na(gono_data$Date_gonopodium)),]
gono_data_subset$Tank <- as.factor(gono_data_subset$Tank)
unique(gono_data_subset$Pop)
unique(gono_data_subset$Treatment)
unique(gono_data_subset$PopxTreat)
sum(gono_data_subset$PopxTreat == "BON-L")
sum(gono_data_subset$PopxTreat == "BON-H")
sum(gono_data_subset$PopxTreat == "PSO-L")
sum(gono_data_subset$PopxTreat == "PSO-H")

#brood_data_subset <- gono_data[which(!is.na(gono_data$Brood_size)),]
#sum(brood_data_subset$PopxTreat == "BON-L")
#sum(brood_data_subset$PopxTreat == "BON-H")
#sum(brood_data_subset$PopxTreat == "PSO-L")
#sum(brood_data_subset$PopxTreat == "PSO-H")


ggplot(data = gono_data, aes(x = Brood_size, y = fry_age_at_gonopodium, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = gono_data, aes(x = Mother_SL_mm, y = fry_age_at_gonopodium, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm")

ggplot(data = gono_data, aes(x = days_maternal_treatment_before_birth, y = fry_age_at_gonopodium, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL) +
  theme_classic() + 
  geom_smooth(method = "lm", se = F)



ggplot(data = gono_data, aes(x = Pop, y = fry_age_at_gonopodium, fill = Pop)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "gold")) + #, name = NULL, labels = NULL) + 
  theme_classic()

ggplot(data = gono_data, aes(x = Treatment, y = fry_age_at_gonopodium, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(data = gono_data, aes(x = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fry_age_at_gonopodium, fill = interaction(Pop, Treatment))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) +  #name = "Population x Food Treatment") + 
  theme_classic()

# Anova(lm(fry_age_at_gonopodium ~ Pop, data = gono_data), type = "II")
# Anova(lm(Brood_size ~ Pop*Treatment, data = gono_data), type = "III")
# ggplot(data = gono_data, aes(x = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = Brood_size, fill = interaction(Pop, Treatment))) + 
#   geom_boxplot() + 
#   scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) +  #name = "Population x Food Treatment") + 
#   theme_classic()
#null.model <- lm(fry_age_at_gonopodium ~ days_maternal_treatment_before_birth + Mother_SL_mm + Brood_size, data = gono_data)
#pop.model <- lm(fry_age_at_gonopodium ~ Pop + days_maternal_treatment_before_birth + Mother_SL_mm + Brood_size, data = gono_data)
#treatment.model <- lm(fry_age_at_gonopodium ~ Treatment + days_maternal_treatment_before_birth + Mother_SL_mm + Brood_size, data = gono_data)
#pop.plus.treat.model <- lm(fry_age_at_gonopodium ~ Pop + Treatment + days_maternal_treatment_before_birth + Mother_SL_mm + Brood_size, data = gono_data)
#pop.by.treat.model <- lm(fry_age_at_gonopodium ~ Pop*Treatment + days_maternal_treatment_before_birth + Mother_SL_mm + Brood_size, data = gono_data)
#AICctab(null.model, pop.model, treatment.model, pop.plus.treat.model, pop.by.treat.model, base = T, weights = T)
#Anova(null.model, type = "III")

#plot(Effect("days_maternal_treatment_before_birth", null.model))
#plot(Effect("Mother_SL_mm", null.model))
#plot(Effect("Brood_size", null.model))

PSO <- gono_data[which(gono_data$Pop == "PSO"),]
PSO$mom.sl.2 <- c()
for (i in 1:nrow(PSO)){
  if (is.na(PSO$Mother_SL_mm[i])){
    PSO$mom.sl.2[i] <- mean(PSO$Mother_SL_mm, na.rm = T)}
  else {PSO$mom.sl.2[i] <- PSO$Mother_SL_mm[i]}
}

BON <- gono_data[which(gono_data$Pop == "BON"),]
BON$mom.sl.2 <- c()
for (i in 1:nrow(BON)){
  if (is.na(BON$Mother_SL_mm[i])){
    BON$mom.sl.2[i] <- mean(BON$Mother_SL_mm, na.rm = T)}
  else {BON$mom.sl.2[i] <- BON$Mother_SL_mm[i]}
}

new.gono <- as.data.frame(rbind(PSO, BON))

#write.csv(new.gono, "~/Downloads/maturity.results.csv")

global.mod <- lm(fry_age_at_gonopodium ~ Pop * Treatment + mom.sl.2 + days_maternal_treatment_before_birth + Brood_size, data = new.gono, na.action = "na.fail")
dredge.out <- dredge(global.mod, rank = "AICc", m.lim=c(1,4))
print(dredge.out, abbrev.names = TRUE)
# m.avg.out <- model.avg(dredge.out)
# m.avg.out
# summary(m.avg.out)

best.gono.mod <- lm(fry_age_at_gonopodium ~ Pop, data = new.gono, na.action = "na.fail")
Anova(best.gono.mod, type = "III")
confint(best.gono.mod)
etasq(best.gono.mod, partial = T, anova = T)
#write.csv(print(dredge.out, abbrev.names = TRUE), "~/Downloads/age.maturity.mod.select.csv")


#model.set <- get.models(dredge.out, subset = weight > 0)

# plot(Effect(c("Treatment", "Pop"), global.mod))
# plot(Effect(c("Treatment", "Pop"), model))
# 
# 
# global.mod <- lm(Brood_size ~ Pop * Treatment + days_maternal_treatment_before_birth, data = gono_data, na.action = "na.fail")
# dredge.out <- dredge(global.mod, rank = "AICc", m.lim=c(1,4))
# print(dredge.out, abbrev.names = TRUE)
# summary(dredge.out)
# #model.set <- get.models(dredge.out, subset = weight > 0)
# 
# m.avg.out <- model.avg(dredge.out)
# m.avg.out
# summary(m.avg.out)
#####################################################################################################################
#
#
#           NOT RUN...This is just for plotting something for the SICB presentation
#
#
####################################################################################################################

# ee <- Effect(c("Treatment", "Pop"), model)
# ee.df <- as.data.frame(cbind(ee$x, ee$fit, ee$se))
# ee.df$pop.treat <- paste(ee.df$Pop, ee.df$Treatment, sep = ".")
# names(ee.df)[3] <- "fit"
# names(ee.df)[4] <- "se"
# 
# ggplot(data = gono_data, aes(x = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fry_age_at_gonopodium, fill = interaction(Pop, Treatment))) + 
#   geom_boxplot(width = 0.5) + 
#   scale_fill_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) +  #name = "Population x Food Treatment") + 
#   ylim(30, 83) + 
#   theme_classic()
# 
# ggplot(data = ee.df, aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, color = interaction(Pop, Treatment))) + 
#   geom_point(size = 6) + 
#   ylim(30, 83) + 
#   scale_color_manual(values = c("steelblue", "darkgoldenrod1", "deepskyblue2", "gold"), name = NULL, labels = NULL) +  #name = "Population x Food Treatment") + 
#   geom_pointrange(aes(x = ordered(pop.treat, levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = fit, ymin = fit - se, ymax = fit + se), 
#                   size=1) +
#   theme_classic()
# 

#Extract the residual effects
mat.ee <- Effect(c("Pop"), best.gono.mod)

#Create a new data frame
mat.mms <- as.data.frame(cbind(mat.ee$fit, mat.ee$se, mat.ee$x))
S.mms <- mat.mms$`mat.ee$fit`[which(mat.mms$Pop == "PSO")]
NS.mms <- mat.mms$`mat.ee$fit`[which(mat.mms$Pop == "BON")]
mms.diff <- S.mms - NS.mms
mms.diff

#Plot
gono.plot <- ggplot() + 
  geom_boxplot(data = new.gono, aes(x = Pop, y = fry_age_at_gonopodium, color = Treatment), outlier.alpha = 0) + 
  geom_point(data = new.gono, aes(x = Pop, y = fry_age_at_gonopodium, color = Treatment), position = position_jitterdodge(.15), alpha = 0.4, size = 2.5) + 
  geom_pointrange(data = mat.mms, aes(x = Pop, y = mat.ee$fit, ymin=mat.ee$fit-mat.ee$se, ymax=mat.ee$fit+mat.ee$se), position = position_dodge(1.7), size=1) + 
  scale_color_manual(values = c("#5DA5DA", "#FAA43A"), name = NULL) + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(30, 80, by=10), labels = c("30.0", "40.0", "50.0", "60.0", "70.0", "80.0")) + 
  ylab("Age at Maturity (days)") + 
  theme(legend.position = "none")
gono.plot










