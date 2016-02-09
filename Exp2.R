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
exp2_df <- read.delim('trial_report_282016_fixed.txt', na.strings = c(" ", ".", "NA", ""))

# Trial data
td <- exp2_df %>%
  filter(!is.na(tnum) &         # Filter out fixation check
           !sub == "UNDEFINED") # Filter out undefined trials

# 0 to NA for tutra
temp <- td$tuttime == 0
td$tutra[temp] <- NA

# Hit for similar trials
temp2 <- (td$sim == "True" & td$resp == "return")
td$rtype[temp2] <- "hi"

# False alarm for similar trials
temp3 <- (td$sim == "True" & td$resp == "space")
td$rtype[temp3] <- "fa"


exp2_df2 <- td %>%
  group_by(sub) %>%
  mutate(tutra2 = na.locf(tutra,
                          fromLast  = TRUE,
                          na.rm     = FALSE)) %>%                                 # Add TUT scores from last 
  filter(!tutra2 == "NA") %>%                                                     # Remove any trials without a TUT score to fill with
  group_by(sub) %>%
  mutate(crrate = sum(rtype == 'cr') / (sum(rtype == 'cr') + sum(rtype == 'fa')), # Correct rejection rate
         hirate = sum(rtype == 'hi') / (sum(rtype == 'hi') + sum(rtype == 'mi')), # Hit rate
         farate = sum(rtype == 'fa') / (sum(rtype == 'cr') + sum(rtype == 'fa')), # False alarm rate
         mirate = sum(rtype == 'mi') / (sum(rtype == 'hi') + sum(rtype == 'mi')), # Miss rate
         dp     = qnorm(hirate) - qnorm(farate),                                  # Dprime 
         mtut   = mean(tutra2)) %>%                                               # Mean TUT
  group_by(sub, rtype) %>%
  mutate(mrtr  = mean(fixed),                                                     # Mean reaction time for correct rejection, false alarm, hit, and miss
         mtutr = mean(tutra2))                                                    # Mean TUT for correct rejection, false alarm, hit, and miss
