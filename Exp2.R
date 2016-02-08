# Packages (may not be using all)
library(broom)
library(dplyr)
library(extrafont)
library(ggplot2)
library(grid)
library(lme4)
library(tidyr)
library(zoo)
library(lmerTest)

# Import fonts
loadfonts(device="win")

# Load trial report and fix NA errors
setwd("C:/Users/Jessica/Documents/Research/data/data_exp2")
df_exp2 <- read.delim('trial_report_282016_fixed.txt', na.strings = c(" ", ".", "NA", ""))
