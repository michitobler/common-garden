rm(list = ls())
library(ggplot2)
library(gridExtra)
library(stringr)
library(svDialogs)
library(lme4)
library(bbmle)
library(effects)
library(readxl)
library(data.table)
library(MuMIn)
library(MASS)
library(GGally)
library(reshape2)
library(car)
library(lubridate)
library(tidyverse)
library(R.utils)
library(lme4)
library(lavaan)
library(sem)
library(semPlot)
library(OpenMx)
library(plyr)
#library(tidyverse)
#library(knitr)
library(kableExtra)
library(GGally)

birth.sizes <- read.csv("~/Downloads/size.at.birth.results.csv")
growth.rates <- read.csv("~/Downloads/growth.rate.res.csv")
maturity <- read.csv("~/Downloads/maturity.results.csv")
predator.avoidance <- read.csv("~/Downloads/predator.avoidance.results.csv")
exploratory <- read.csv("~/Downloads/exploratory.behavior.results.csv")
feeding <- read.csv("~/Downloads/feeding.accuracy.results.csv")

colnames(exploratory)[2] <- "Tank"
colnames(predator.avoidance)[2] <- "Tank"

# earliest.pred.avoid <- data.frame(matrix(nrow = length(unique(predator.avoidance$tanks)), ncol = 2))
# names(earliest.pred.avoid) <- c("Tank", "EarliestPredAvoidPC1")
# for (i in 1:length(unique(predator.avoidance$tanks))){
#   curr.df <- as.data.frame(predator.avoidance[which(predator.avoidance$tanks == unique(predator.avoidance$tanks)[i]),])
#   min.data <- curr.df[curr.df$dates == min(curr.df$dates), c("tanks", "PC1")]
#   earliest.pred.avoid[i,] <- min.data
# }
# 
# earliest.growth <- data.frame(matrix(nrow = length(unique(growth.rates$Tank)), ncol = 2))
# colnames(earliest.growth) <- c("Tank", "EarliestGrowthRate")
# for (i in 1:length(unique(growth.rates$Tank))){
#   curr.df <- as.data.frame(growth.rates[which(growth.rates$Tank == unique(growth.rates$Tank)[i]),])
#   min.data <- curr.df[curr.df$Date == min(curr.df$Date), c("Tank", "Rel.Growth.Rate.body.lengths.per.day")]
#   earliest.growth[i,] <- min.data
# }
# 
# earliest.explore <- data.frame(matrix(nrow = length(unique(exploratory$tank)), ncol = 2))
# names(earliest.explore) <- c("Tank", "EarliestExplorePC1")
# for (i in 1:length(unique(exploratory$tank))){
#   curr.df <- as.data.frame(exploratory[which(exploratory$tank == unique(exploratory$tank)[i]),])
#   min.data <- curr.df[curr.df$trial.date == min(curr.df$trial.date), c("tank", "EarliestExplorePC1")]
#   earliest.explore[i,] <- min.data
# }
# 




growth.rates.avg <- aggregate(Rel.Growth.Rate.body.lengths.per.day ~ Tank, growth.rates, mean)
names(growth.rates.avg) <- c("Tank", "AVG.rel.growth.rate.body.lengths.per.day")
predator.avoidance.avg <- aggregate(PC1 ~ Tank, predator.avoidance, mean)
names(predator.avoidance.avg) <- c("Tank", "PC1.pred.avoid")
exploratory.avg <- aggregate(PC1 ~ Tank, exploratory, mean)
names(exploratory.avg) <- c("Tank", "PC1.explore")
feeding.avg <- aggregate(Success.Accuracy ~ Tank, feeding, mean)
names(feeding.avg) <- c("Tank", "feeding.accuracy")

complete.df <- maturity
complete.df$size.at.birth <- birth.sizes$Avg.SL.mm[match(complete.df$Tank, birth.sizes$Tank)]
complete.df$avg.growth.rate <- growth.rates.avg$AVG.rel.growth.rate.body.lengths.per.day[match(complete.df$Tank,growth.rates.avg$Tank)]              
complete.df$avg.pred.avoid <- predator.avoidance.avg$PC1.pred.avoid[match(complete.df$Tank,predator.avoidance.avg$Tank)] 
complete.df$avg.explore <- exploratory.avg$PC1.explore[match(complete.df$Tank, exploratory.avg$Tank)]   
complete.df$avg.feeding <- feeding.avg$feeding.accuracy[match(complete.df$Tank, feeding.avg$Tank)]   

# Putting in avg exploratory PCscore for all PSO-H tanks for 20-PSO-H, which is missing data
pso.h.explore <- complete.df[which(complete.df$PopxTreat == "PSO-H"),]
pso.h.avg.explore <- mean(pso.h.explore$avg.explore, na.rm = T)
complete.df$avg.explore[which(complete.df$Tank == "20-PSO-H")] <- pso.h.avg.explore


in.df <- complete.df[,c("size.at.birth", "Brood_size", "fry_age_at_gonopodium", "avg.growth.rate", "avg.pred.avoid", "avg.explore", "avg.feeding")]       
in.df.metadata <- complete.df
in.df.metadata <- in.df.metadata[complete.cases(in.df),]
in.df <- in.df[complete.cases(in.df),]
ggpairs(in.df)
complete.df.pca <- prcomp(in.df, center = T, scale. = T)
complete.df.pca
summary(complete.df.pca)


pcscores <- as.data.frame(cbind(in.df.metadata, complete.df.pca$x))

eigenvectors <- as.data.frame(complete.df.pca$rotation)
eigenvalues <- complete.df.pca$sdev^2

loads <- data.frame(matrix(nrow = 7, ncol = 7))
for (i in 1:length(colnames(eigenvectors))){
  loads[,i] <- eigenvectors[,i] * sqrt(eigenvalues[i])
}

colnames(loads) <- colnames(complete.df.pca$rotation)
rownames(loads) <- rownames(complete.df.pca$rotation)

pc1 <- c(loads$PC1)
names(pc1) <- rownames(loads)
sort(pc1, decreasing = T)

pc2 <- c(loads$PC2)
names(pc2) <- rownames(loads)
sort(pc2, decreasing = T)


# high_load_elems_pc1 <- c("Tl", "Bi", "Be", "Zn", "Cr", "S", "Co", "Ca")
# high_load_elems_pc2 <- c("Ba", "Sr", "Mn", "Ni", "B", "Si", "Fe", "Cd")
# high_load_elems <- c(high_load_elems_pc1, high_load_elems_pc2)
# important_loads <- loads[which(rownames(loads) %in% high_load_elems),]

ggplot(data = pcscores, aes(x = PC1, y = PC2, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL) + 
  geom_segment(data = loads, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(1/2, "picas")), 
               color = "black", 
               size = .5) + 
  annotate("text", 
           x = loads$PC1, 
           y = loads$PC2, 
           label = rownames(loads), 
           size = 2) + 
  theme_classic() + 
  theme(legend.position = "none")

ggplot(data = pcscores, aes(x = PC2, y = PC3, color = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_point() + 
  scale_color_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL) + 
  geom_segment(data = loads, 
               aes(x = 0, y = 0, xend = PC2, yend = PC3), 
               arrow = arrow(length = unit(1/2, "picas")), 
               color = "black", 
               size = .5) + 
  annotate("text", 
           x = loads$PC2, 
           y = loads$PC3, 
           label = rownames(loads), 
           size = 2) + 
  theme_classic()


# full_mod <- lm(cbind(size.at.birth, Brood_size, fry_age_at_gonopodium, avg.growth.rate, avg.pred.avoid, avg.explore, avg.feeding) ~ 
#                  Pop + 
#                  Treatment + 
#                  Pop * Treatment + 
#                  mom.sl.2 + 
#                  days_maternal_treatment_before_birth, 
#                data = in.df.metadata)
# dredge.full.mod <- dredge(full_mod, rank = "AICc", m.lim = c(1, 4))
# print(dredge.out, abbrev.names = TRUE)
# m.avg.out <- model.avg(dredge.out)
# summary(m.avg.out)
# 
# best.feed.mod <- lmer(asin(sqrt(Success.Accuracy)) ~ Pop + (1 | Tank), data = new.feed, na.action = "na.fail")
# Anova(best.feed.mod, type = "III")
# confint(best.feed.mod)
full_mod <- lm(PC1 ~ 
                 Pop + 
                 Treatment + 
                 Pop * Treatment + 
                 mom.sl.2 + 
                 days_maternal_treatment_before_birth, 
               data = pcscores, 
               na.action = "na.fail")
dredge.full.mod <- dredge(full_mod, rank = "AICc", m.lim = c(1, 4))
print(dredge.full.mod, abbrev.names = TRUE)
#write.csv(print(dredge.full.mod, abbrev.names = TRUE), "~/Downloads/all.phenos.together.mod.select.csv")
best.multivar.mod <- lm(PC1 ~ Pop + Treatment + days_maternal_treatment_before_birth, data = pcscores, na.action = "na.fail")
Anova(best.multivar.mod, type = "III")
confint(best.multivar.mod)
etasq(best.multivar.mod, partial = T, anova = T)

ggplot(data = pcscores, aes(x = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")), y = PC1, fill = ordered(interaction(Pop, Treatment), levels = c("BON.H", "BON.L", "PSO.H", "PSO.L")))) + 
  geom_boxplot() + 
  theme_classic() + 
  scale_fill_manual(values = c("steelblue", "deepskyblue2", "darkgoldenrod1", "gold"), name = NULL)

full_mod2 <- lm(PC2 ~ 
                 Pop + 
                 Treatment + 
                 Pop * Treatment + 
                 mom.sl.2 + 
                 days_maternal_treatment_before_birth, 
               data = pcscores, 
               na.action = "na.fail")
dredge.full.mod2 <- dredge(full_mod2, rank = "AICc", m.lim = c(1, 4))
print(dredge.full.mod2, abbrev.names = TRUE)
#write.csv(print(dredge.full.mod, abbrev.names = TRUE), "~/Downloads/all.phenos.together.mod.select.csv")
best.multivar.mod2 <- lm(PC2 ~ Pop + mom.sl.2 + days_maternal_treatment_before_birth, data = pcscores, na.action = "na.fail")
Anova(best.multivar.mod2, type = "III")
confint(best.multivar.mod2)
etasq(best.multivar.mod2)




############################################################
# Now trying to get all datasets together, and not averaged
############################################################
#base.df <- feeding
pheno1 <- as.data.frame(cbind(feeding$Tank, feeding$Success.Accuracy, feeding$week.of.measure))
names(pheno1) <- c("Tank", "Success.Accuracy", "week.of.measure")
pheno2 <- as.data.frame(cbind(exploratory$Tank, exploratory$PC1, exploratory$week.of.measure))
names(pheno2) <- c("Tank", "ExploratoryPC1", "week.of.measure")
pheno3 <- as.data.frame(cbind(predator.avoidance$Tank, predator.avoidance$PC1, predator.avoidance$week.of.measure))
names(pheno3) <- c("Tank", "Predator.AvoidancePC1", "week.of.measure")
pheno4 <- as.data.frame(cbind(growth.rates$Tank, growth.rates$Rel.Growth.Rate.body.lengths.per.day, growth.rates$week.of.measure))
names(pheno4) <- c("Tank", "GrowthRate", "week.of.measure")

#all.phenos.df <- join_all(dfs = list(feeding, exploratory, predator.avoidance, growth.rates), by = c("Tank", "week.of.measure"))
all.phenos.raw.df <- join_all(dfs = list(pheno3, pheno1, pheno2, pheno4), by = c("Tank", "week.of.measure"))
all.phenos.df <- all.phenos.raw.df[complete.cases(all.phenos.raw.df),]



lda.pop <- lda(Pop ~ size.at.birth + Brood_size + fry_age_at_gonopodium + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding, data = in.df.metadata, CV = F)
#lda.pop <- lda(Pop ~ PC1, data = pcscores, CV = F)
lda.pop
lda.pop.df <- data.frame(
  Pop = in.df.metadata[,"Pop"], 
  lda = predict(lda.pop)$x
)
ggplot(data = lda.pop.df, aes(x = Pop, y = LD1, color = Pop)) + 
  geom_boxplot() + 
  geom_point(position = "jitter") + 
  theme_classic()


lda.food <- lda(Treatment ~ size.at.birth + Brood_size + fry_age_at_gonopodium + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding, data = in.df.metadata, CV = F)
lda.food
lda.food.df <- data.frame(
  food = in.df.metadata[,"Treatment"], 
  lda = predict(lda.food)$x
)
ggplot(data = lda.food.df, aes(x = food, y = LD1, color = food)) + 
  geom_boxplot() + 
  geom_point(position = "jitter") + 
  theme_classic()

lda.length.treat <- lda(days_maternal_treatment_before_birth ~ PC1, data = pcscores, CV = F)
lda.length.treat
lda.length.treat.df <- data.frame(
  length.treat = in.df.metadata[,"days_maternal_treatment_before_birth"], 
  lda = predict(lda.length.treat)$x
)
ggplot(data = lda.length.treat.df, aes(x = length.treat, y = LD1)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_classic()

lda.pop.treat <- lda(interaction(Pop, Treatment) ~ size.at.birth + Brood_size + fry_age_at_gonopodium + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding, data = in.df.metadata, CV = F)
lda.pop.treat
lda.pop.treat.df <- data.frame(
  Pop.Treat = in.df.metadata[,"PopxTreat"], 
  lda = predict(lda.pop.treat)$x
)
ggplot(data = lda.pop.treat.df, aes(x = Pop.Treat, y = lda.LD1, color = Pop.Treat)) + 
  geom_boxplot() + 
  geom_point(position = "jitter") + 
  theme_classic()

ggplot(data = lda.pop.treat.df, aes(x = Pop.Treat, y = lda.LD2, color = Pop.Treat)) + 
  geom_boxplot() + 
  geom_point(position = "jitter") + 
  theme_classic()

# full_fit <- Manova(full_mod, test.statistic = "Wilks", type = "III")
# full_sum <- summary(full_fit, test = "Wilks")
# full_fit
# etasq(full_fit, partial = T, anova = T, test = "Wilks")









###################################
# Structural Equation Modeling
###################################

# model <- '
# Brood_size ~ mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding
# mom.sl.2 ~ Brood_size + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding
# days_maternal_treatment_before_birth ~ Brood_size + mom.sl.2 + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding
# fry_age_at_gonopodium ~ Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding
# size.at.birth ~ Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding
# avg.growth.rate ~ Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.pred.avoid + avg.explore + avg.feeding
# avg.pred.avoid ~ Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.explore + avg.feeding
# avg.explore ~ Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.feeding
# avg.feeding ~ Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore
# '
model <- '
life.hist =~ size.at.birth + Brood_size + fry_age_at_gonopodium + avg.growth.rate
behav =~ avg.pred.avoid + avg.explore + avg.feeding
fitness =~ life.hist + behav

fitness ~ mom.sl.2 + days_maternal_treatment_before_birth
'

model <- '
avg.growth.rate ~ Brood_size + fry_age_at_gonopodium + size.at.birth + avg.pred.avoid + avg.explore + avg.feeding
avg.pred.avoid ~ Brood_size + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.explore + avg.feeding
avg.explore ~ Brood_size + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.feeding
avg.feeding ~ Brood_size + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore
'
model2 <- '
Brood_size ~ fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore + avg.feeding
'
model3 <- '
Brood_size ~ fry_age_at_gonopodium + size.at.birth + avg.pred.avoid + avg.explore + avg.feeding
'

# model <- '
# avg.growth.rate ~ Pop + Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + Treatment + avg.pred.avoid + avg.explore + avg.feeding
# avg.pred.avoid ~ Pop + Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + Treatment + avg.explore + avg.feeding
# avg.explore ~ Pop + Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + Treatment + avg.feeding
# avg.feeding ~ Pop + Brood_size + mom.sl.2 + days_maternal_treatment_before_birth + fry_age_at_gonopodium + size.at.birth + avg.growth.rate + avg.pred.avoid + avg.explore + Treatment
# '
#cov(in.df)
fit <- lavaan::cfa(model, data = pcscores)
varTable(fit)
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit, 'std', layout = 'circle')
semPaths(fit,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

fit2 <- lavaan::cfa(model2, data = in.df)
varTable(fit2)
summary(fit2, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit2, 'std', layout = 'circle')
semPaths(fit2,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

fit3 <- lavaan::cfa(model3, data = in.df)
varTable(fit3)
summary(fit3, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit3, 'std', layout = 'circle')
semPaths(fit3,"std",layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)

anova(fit2, fit3)












