### Project 1: Anti-Anxiety Medications and Memory ###
### Psych #rstats Club ###
### Vincent Lai ###

# Getting started ----

# Install and load pacman package
# install.packages("pacman")
library(pacman)

# Install and load all other packages as necessary
p_load(psych, tidyverse)

# Dataset: Islander Data
# From the UCLA Psychology Department
# Load dataset
islander_data <- read.csv("data/processed/Islander_data.csv")



# Data cleaning ----

# Convert group variables into factors with appropriate labels
islander_data$Happy_Sad_group <- as.factor(islander_data$Happy_Sad_group)
levels(islander_data$Happy_Sad_group) <- c("Happy", "Sad")

islander_data$Dosage <- as.factor(islander_data$Dosage)

islander_data$Drug <- as.factor(islander_data$Drug)
levels(islander_data$Drug) <- c("Alprazolam", "Placebo", "Triazolam")



# Data exploration ----

# Overview of the data
head(islander_data)
str(islander_data)

# Descriptive statistics
describe(islander_data)

# Histograms
# Pre-test score
hist(islander_data$Mem_Score_Before)

# Post-test score
hist(islander_data$Mem_Score_After)

# Difference scores
hist(islander_data$Diff)



# Assumptions for correlation ----

# Check for and remove outliers
# Using the IQR method
diff_boxplot <- boxplot(islander_data$Diff) # There are outliers among the difference scores

diff_Q1 <- quantile(islander_data$Diff, 0.25)

diff_Q3 <- quantile(islander_data$Diff, 0.75)

diff_IQR <- IQR(islander_data$Diff)

islander_cleaned <- subset(islander_data, 
                           islander_data$Diff > (diff_Q1 - 1.5 * diff_IQR) & islander_data$Diff < (diff_Q3 + 1.5 * diff_IQR))

boxplot(islander_cleaned$Diff)

# Normality
hist(islander_cleaned$Diff)

qqnorm(islander_cleaned$Diff)
qqline(islander_cleaned$Diff) # Points fall approximately along the reference line, so we can assume normality



# Data analyses ----

## Is age correlated with pre-test scores?
cor.test(x = islander_cleaned$age, y = islander_cleaned$Mem_Score_Before)

## Is age correlated with difference scores?
cor.test(x = islander_cleaned$age, y = islander_cleaned$Diff)

## Are there differences in the effects of medication condition on difference scores? ----
# We perform a one-way ANOVA to assess whether there were differences in difference scores between the drug conditions
med_mem_anova <- aov(formula = Diff ~ Drug, data = islander_cleaned)

anova(med_mem_anova) # Yes, p = .002
# At least one of the medication groups is significantly different in their effect on difference scores

# Post-hoc comparisons
TukeyHSD(med_mem_anova, conf.level = 0.95)
# We have evidence to conclude that placebo was significantly different from Alprazolam and that Triazolam was significantly different from Alprazolam

# Check ANOVA assumptions
par(mfrow=c(2,2))
plot(med_mem_anova)
par(mfrow=c(1,1))



## Is there an interaction effect in the effects of different medications and dosages on difference scores? ----
# We perform a factorial ANOVA to compare mean difference scores across medication and dosage groups 
med_dose_mem_anova <- aov(formula = Diff ~ Drug * Dosage, data = islander_cleaned)

anova(med_dose_mem_anova) # Yes, there is a significant interaction, p < .001
# This suggests that the effect of medication on difference score depends on dosage (or vice versa)

# Interaction plot
# Plot difference scores for each drug across each dosage level
interaction.plot(x.factor = islander_data$Drug, trace.factor = islander_data$Dosage, response = islander_data$Diff, fun = mean)

# Check ANOVA assumptions
par(mfrow=c(2,2))
plot(med_dose_mem_anova)
par(mfrow=c(1,1))
# Assumptions likely not violated



# Planned contrasts comparing the effects of drugs and placebo on difference scores ----

# Omnibus ANOVA
med_mem_anova <- aov(formula = Diff ~ Drug, data = islander_cleaned)

anova(med_mem_anova) # p = .002
# At least one of the medications has a significantly different effect on difference scores from the others

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
# Planned contrasts revealed that the effect of the medications (Alprazolam and Triazolam) on difference scores was different compared to placebo, p = .002
# They also revealed that the effect of Alprazolam on difference scores was different from that of Triazolam, p < .001