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

# Overview of the dataset ####
head(islander_data)

# Descriptive statistics ####
describe(islander_data)

# Visualizing the data ####

# Histogram of difference scores
hist(islander_data$Diff)