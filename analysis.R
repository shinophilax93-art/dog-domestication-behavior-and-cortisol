install.packages("psych")
install.packages("dplyr")
install.packages("car")
install.packages("lme4")
install.packages("tidyverse")
install.packages("backports")

library(psych)
library(dplyr)
library(car)
library(lme4)
library(tidyverse)
library(backports)

data <- read.csv("data.csv", header=TRUE)

#Binomial test for probe trials

novel_total <- sum(data$NumberOfChoices_probe_novel, na.rm = TRUE)
familiar_total <- sum(data$NumberOfChoices_probe_familiar, na.rm = TRUE)

binom.test(
  x = familiar_total,
  n = novel_total + familiar_total,
  p = 0.5
)

#Between-group Probe trials
probe2 <- subset(
  data[, c(
    "ID",
    "BreedGroup",
    "NumberOfChoices_probe_novel",
    "NumberOfChoices_probe_familiar"
  )],
  BreedGroup != "Mongrel"
)
model <- glm (cbind (NumberOfChoices_probe_novel, NumberOfChoices_probe_familiar) ~ BreedGroup,
              family = binomial, data = probe2)
Anova(model)


#Cook's distance

eu <- subset(data, BreedGroup == "European") #European
jp <- subset(data, BreedGroup == "Japanese") #Japanese

#Corti_Ave
res <- lm(Neophobia ~ CortiConc_Ave, data = eu)
plot(res, which = 4)
cook <- cooks.distance(res)
cook
sort(cook, decreasing = TRUE)
threshold <- 4 / nrow(eu)
threshold
eu$ID[cook > threshold] #dog4

#Neophobia
res2 <- lm(CortiConc_Ave ~ Neophobia, data = eu)
plot(res2, which = 4)
cook2 <- cooks.distance(res2)
cook2
sort(cook2, decreasing = TRUE)
threshold <- 4 / nrow(eu)
threshold
eu$ID[cook > threshold] #dog1

#Corti_SD
res3 <- lm(Neophobia ~ CortiConc_SD, data = jp)
plot(res3, which = 4)
cook3 <- cooks.distance(res3)
cook3
sort(cook3, decreasing = TRUE)
threshold <- 4 / nrow(jp)
threshold
cook <- cooks.distance(res3)
threshold <- 4 / nrow(jp)
jp$ID[cook > threshold] #dog18

#Exclusion of Dog 1, Dog 4, and Dog 18
data[cbind(c(1,3,11),
           c(7,8,9))] <- NA

#Correlation analysis

data2 <- data[, -which (colnames(data) #Remove irrelevant data columns
                        %in% c("ID", "BreedGroup", "NumberOfChoices_probe_familiar"))]
na.omit(data2)
print(data2)
data2 <- as.data.frame(sapply(data2, as.numeric))

#Correlation coefficient
cordata2 = cor(data2, method="spearman", use="complete.obs")

#P-value
cordata2p <- corr.test(data2, method="s", adjust="bonferroni")$p

#Between-group comparison of cortisol

data3 <- subset(
  data,
  BreedGroup %in% c("Japanese", "European")
)

#Average of concentrations
wilcox.test(
  CortiConc_Ave ~ BreedGroup,
  data = data3
)

#Variability of concentrations (SD)
wilcox.test(
  CortiConc_SD ~ BreedGroup,
  data = data3
)
