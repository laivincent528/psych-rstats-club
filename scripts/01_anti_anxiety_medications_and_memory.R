### Project 1: Anti-Anxiety Medications and Memory ###
### Psych #rstats Club ###
### Vincent Lai ###

# Getting started ####

# Install and load pacman package
# install.packages("pacman")
library(pacman)

# Install and load all other packages as necessary
p_load(psych, tidyverse)

# Dataset: Islander Data
# From the UCLA Psychology Department
# Load dataset
islander_data <- read.csv("data/processed/Islander_data.csv")



# Data cleaning ####

# Convert group variables into factors with appropriate labels
islander_data$Happy_Sad_group <- as.factor(islander_data$Happy_Sad_group)
levels(islander_data$Happy_Sad_group) <- c("Happy", "Sad")

islander_data$Dosage <- as.factor(islander_data$Dosage)

islander_data$Drug <- as.factor(islander_data$Drug)
levels(islander_data$Drug) <- c("Alprazolam", "Placebo", "Triazolam")


# Data exploration ####

# Overview of the data
head(islander_data)
str(islander_data)

# Descriptive statistics
describe(islander_data)

# Histogram of difference scores
hist(islander_data$Diff)



# Data analyses ####

## Are there differences in the effects of medication condition on memory? ####
med_mem_anova <- aov(formula = Diff ~ Drug, data = islander_data)
anova(med_mem_anova)

# Post-hoc comparisons
TukeyHSD(med_mem_anova, conf.level = 0.95)

# Check ANOVA assumptions
par(mfrow=c(2,2))
plot(med_mem_anova)
par(mfrow=c(1,1))



## Is there an interaction effect in the effects of different medications and dosages on memory? ####
med_dose_mem_anova <- aov(formula = Diff ~ Drug * Dosage, data = islander_data)
anova(med_dose_mem_anova)

# Interaction plot
interaction.plot(x.factor = islander_data$Drug, trace.factor = islander_data$Dosage, response = islander_data$Diff, fun = mean)

# Check ANOVA assumptions
par(mfrow=c(2,2))
plot(med_dose_mem_anova)
par(mfrow=c(1,1))



# Planned contrasts comparing the effects of drugs and placebo on memory ####

# Omnibus ANOVA
med_mem_anova <- aov(formula = Diff ~ Drug, data = islander_data)
anova(med_mem_anova)
# At least one of the medications has a significantly different effect on memory from the others

# Look at levels of medications
levels(islander_data$Drug)

# Specify groups to compare
c1 <- c(1, -2, 1) # Medications vs. Placebo
c2 <- c(1, -1, 0) # Alprazolam vs. Triazolam
c3 <- c(0, 0, 0)

# Combine above lines into a matrix and specify contrasts of interest
contrasts(islander_data$Drug) <- cbind(c1, c2, c3)

# Planned contrasts ANOVA
med_mem_contrast <- aov(formula = Diff ~ Drug, data = islander_data)

summary.aov(med_mem_contrast, split = list(Drug = list("Medications vs. Placebo" = 1, "Alprazolam vs. Triazolam" = 2)))