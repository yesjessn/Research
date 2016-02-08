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

# Trial data
td <- df_exp2 %>%
  filter(!is.na(tnum) &  # Filter out fixation check
           !sub == "UNDEFINED") # Filter out undefined trials

# 0 to NA for tutra
temp <- fix$tuttime == 0
fix$tutra[temp] <- NA

# Hit for similar trials
temp2 <- (fix$sim == "True" & fix$resp == "return")
fix$rtype[temp2] <- "hi"

# False alarm for similar trials
temp3 <- (fix$sim == "True" & fix$resp == "space")
fix$rtype[temp3] <- "fa"