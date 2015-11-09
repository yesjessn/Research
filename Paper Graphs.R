# Paper Graphs
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

# Load trial report and fix errors
setwd("C:/Users/Jessica/Documents/Research/data")
df <- read.delim('trial_report_32015.txt', na.strings = c(" ", ".", "NA", ""))
df$rtype[df$rtype == "correj"] <- "cr"
df$rtype[df$rtype == "falsealarm"] <- "fa"
df$rtype[df$rtype == "hit"] <- "hi"
df$rtype[df$rtype == "miss"] <- "mi"

# Load IA report and fix errors
setwd("C:/Users/Jessica/Documents/Research/data")
df2 <- read.delim('IA_report_4282015.txt', na.strings = c(" ", ".", "NA", ""))

m <- merge(df, df2)

# Remove subjects below 40 TUTs (Remove: 1_zw and 45_sjk)
filter <- m %>%
  filter(!RECORDING_SESSION_LABEL == "1_zw",
         !RECORDING_SESSION_LABEL == "45_sjk")

# Trial data
td <- filter %>%
  filter(!is.na(tnum))

# Add TUT scores from last and remove any trials without a TUT score to fill with
df3 <- td %>%
  group_by(sub) %>%
  mutate(tutra2 = na.locf(tutra,
                          fromLast  = TRUE,
                          na.rm     = FALSE)) %>%
  filter(!is.na(tutra2))

# Physiological Data----------------------
pd <- df3 %>%
  group_by(sub) %>%
  mutate(crrate = sum(rtype == 'cr') / (sum(rtype == 'cr') + sum(rtype == 'fa')), # Correct Rejection Rate
         hirate = sum(rtype == 'hi') / (sum(rtype == 'hi') + sum(rtype == 'mi')), # Hit Rate
         farate = sum(rtype == 'fa') / (sum(rtype == 'cr') + sum(rtype == 'fa')), # False Alarm Rate
         mirate = sum(rtype == 'mi') / (sum(rtype == 'hi') + sum(rtype == 'mi')), # Miss Rate
         dp     = qnorm(hirate) - qnorm(farate),                                  # Dprime 
         mtut   = mean(tutra2),                                                   # Mean TUT
         mrt    = mean(rt)) %>%                                                   # Mean Reaction Time
  group_by(sub, rtype) %>%
  mutate(mrtr  = mean(rt),                                                        # Mean Reaction Time for Correct Rejection, False Alarm, Hit, and Miss
         mtutr = mean(tutra2))                                                    # Mean TUT for Correct Rejection, False Alarm, Hit, and Miss

  # Centered Data
  cd <- pd %>%
    ungroup() %>%
    select(sub, rtype, mrtr, mtutr) %>%
    unique() %>%
    mutate(mcrtr = mrtr - mean(mrtr)) # Mean Centered Reaction Time for Correct Rejection, False Alarm, Hit, and Miss
           
  # Table 1: number of participants with rtype for tut level
  t <- pd %>%
    group_by(sub) %>%
    select(sub, rtype, tnum) %>%
    unique() %>%
    mutate(crs = sum(rtype == 'cr'),
           fas = sum(rtype == 'fa'),
           his = sum(rtype == 'hi'),
           mis = sum(rtype == 'mi')) %>%
    ungroup() %>%
    summarise(cr = mean(crs),
              fa = mean(fas),
              hi = mean(his),
              mi = mean(mis))

  # Graph 1: number of subjects with mean tut level
  g <- pd %>%
    ungroup() %>%
    select(sub, mtut) %>%
    unique()

  ggplot(g, aes(mtut))+
    geom_histogram(breaks = c(0, 1, 2, 3, 4, 5),
                   colour = "white",
                   fill   = "#00003C")+
    labs(list(x = "Mean TUT Score",
              y = "Number of Participants"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29))

  # Graph 2: dprime versus mean TUT and remove subject 12_sw because of 0 false alarm rate
  g2 <- pd %>%
    filter(!sub == "12_sw") %>% 
    select(mtut, dp) %>%
    unique()
  cor.test(g2$mtut, g2$dp)
  
  ggplot(g2, aes(mtut, dp))+
    geom_point(colour = "#00003C",
               size   = 2)+
    geom_smooth(colour  = "#00003C",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "D'"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29))
  
  # Linear regression for mean TUT score and mean centered RT by correct rejection, false alarm, hit, and miss
  lr <- lm(mtutr ~ mcrtr * rtype, cd)
  summary(lr)
  
  # Miscellaneous Graph: mean RT versus mean TUT score by correct rejection, false alarm, hit, and miss
  mg <- pd %>%
    select(sub, mtutr, mrtr, rtype) %>%
    unique()
  
  mgcr <- mg %>%
    filter(rtype == "cr")
  cor.test(mgcr$mtut, mgcr$mrt)
  
  mgfa <- mg %>%
    filter(rtype == "fa")
  cor.test(mgfa$mtut, mgfa$mrt)
  
  mgh <- mg %>%
    filter(rtype == "hi")
  cor.test(mgh$mtut, mgh$mrt)
  
  mgm <- mg %>%
    filter(rtype == "mi")
  cor.test(mgm$mtut, mgm$mrt)
  
  # Changing names of rtype
  levels(mg$rtype)[levels(mg$rtype)=="cr"] <- "Correct Rejection"
  levels(mg$rtype)[levels(mg$rtype)=="fa"] <- "False Alarm"
  levels(mg$rtype)[levels(mg$rtype)=="hi"] <- "Hit"
  levels(mg$rtype)[levels(mg$rtype)=="mi"] <- "Miss"
  
  ggplot(mg, aes(mtutr, mrtr))+
    geom_point(colour = "#00003C",
               size   = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour  = "#00003C",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Reaction Time (ms)"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29))

# Eye Data: between subjects----------------------
edb <- pd %>%
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT) %>%  # IA Run Count-Trial Total Visited IA Count
  filter(r < 25) %>%
  ungroup() %>%
  group_by(sub) %>%
  mutate(msc  = mean(SACCADE_COUNT),                              # Mean Saccade Count
         mafd = mean(AVERAGE_FIXATION_DURATION, na.rm = TRUE),    # Mean Average Fixation Duration
         mviac = mean(VISITED_INTEREST_AREA_COUNT)) %>%           # Mean Visited Interest Area Count
  mutate(mr = mean(r, na.rm = TRUE))                              # Mean Refixations
  
  # Centered Data
  cd2 <- edb %>%
    ungroup() %>%
    select(mtut, msc, mafd, mviac, mr) %>%
    unique() %>%
    mutate(mcmsc  = msc - mean(msc),      # Mean Centered Mean Saccade Count
           mcmafd = mafd - mean(mafd),    # Mean Centered Mean Average Fixation Duration
           mcmiac = mviac - mean(mviac),  # Mean Centered Mean Visited Interest Area Count
           mcmr   = mr - mean(mr))        # Mean Centered Mean Refixations
  
  # Table 2: correlation matrix of  mean TUT score, mean centered saccade count, mean centered average fixation druation, mean centered visited interest area count, and mean centered refixations
  cm <- cd2 %>%
    select(mtut, mcmsc, mcmafd, mcmiac, mcmr) %>%
    unique()
  cor(cm)
  cor.test(cm$mcmafd, cm$mtut)

  # Table 3: linear regression of mean TUT score, mean centered fixation duration, and mean centered visited interest area count
  lm <- lm(mtut ~ mcmafd + mcmiac, cm)
  summary(lm)
  
  # Graphs
  ggplot(cd2, aes(mtut, msc))+
    geom_point(colour = "#00003C",
               size   = 2)+
    geom_smooth(colour  = "#00003C",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Saccade Count"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29))
  
  ggplot(cd2, aes(mtut, mafd))+
    geom_point(colour = "#00003C",
               size   = 2)+
    geom_smooth(colour  = "#00003C",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Average Fixation Duration (ms)"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29))
  ggplot(cd2, aes(mtut, mr))+
    geom_point(colour = "#00003C",
               size   = 2)+
    geom_smooth(colour  = "#00003C",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Refixations"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29))
  
  ggplot(cd2, aes(mtut, mviac))+
    geom_point(colour = "#00003C",
               size   = 2)+
    geom_smooth(colour  = "#00003C",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Visited Interest Area Count"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "IrisUPC",
                                           size   = 29))
    
  
# Eye Data: within subject----------------------
edw <- pd %>%
  group_by(sub) %>%
  mutate(sc   = SACCADE_COUNT,                                # Saccade Count
         afd  = AVERAGE_FIXATION_DURATION,                    # Average Fixation Duration
         viac = VISITED_INTEREST_AREA_COUNT) %>%              # Visited Interest Area Count
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT)  # IA Run Count-Trial Total Visited IA Count

# Centered Data and Remove Abnormalities
cd3 <- edw %>%
 ungroup() %>%
  filter(r < 25, sc > 0, !afd == "NA") %>%
  select(sub, tnum, rt, sc, afd, viac, r, tutra2) %>%
  unique() %>%
  group_by(sub) %>%
  mutate(mcrt   = rt - mean(rt),     # Mean Centered Reaction Time
         mcafd  = afd - mean(afd),   # Mean Centered Average Fixation Duration
         mcsc   = sc - mean(sc),     # Mean Centered Saccade Count
         mcviac = viac - mean(viac), # Mean Centered Visited Interest Area Count
         mcr    = r - mean(r))       # Mean Centered Refixations

  # Linear mixed-effects model for TUT, RT, mean centered saccade count, mean centered average fixation duration, mean centered visited interest area count, and mean centered refixations
  lmem <- lmer(tutra2 ~ mcrt + (mcrt|sub), cd3)
  summary(lmem)

  lmem2 <- lmer(tutra2 ~ mcsc + (mcsc|sub), cd3)
  summary(lmem2)
  
  lmem <- lmer(tutra2 ~ mcafd + (mcafd|sub), cd3)
  summary(lmem)
  
  lmem3 <- lmer(tutra2 ~ mcviac + (mcviac|sub), cd3)
  summary(lmem3)
  
  lmem4 <- lmer(tutra2 ~ mcr + (mcr|sub), cd3)
  summary(lmem4)


# New IA Report----------
dfn <- read.delim('IA_report_8102015.txt', na.strings = c(" ", ".", "NA", ""))
df$rtype[df$rtype == "correj"] <- "cr"
df$rtype[df$rtype == "falsealarm"] <- "fa"
df$rtype[df$rtype == "hit"] <- "hi"
df$rtype[df$rtype == "miss"] <- "mi"

# Remove subjects below 40 TUTs (Remove: 1_zw and 45_sjk)
fn <- dfn %>%
  filter(!RECORDING_SESSION_LABEL == "1_zw",
         !RECORDING_SESSION_LABEL == "45_sjk")

# Trial data
tdn <- fn %>%
  filter(!is.na(tnum))

# Add TUT scores from last and remove any trials without a TUT score to fill with
dfn2 <- tdn %>%
  group_by(sub) %>%
  mutate(tutra2 = na.locf(tutra,
                          fromLast  = TRUE,
                          na.rm     = FALSE)) %>%
  filter(!is.na(tutra2))

# See Unique Refixations
dfn3 <- dfn2 %>%
  select(sub, tutra2, tcat, IA_LABEL, IA_RUN_COUNT) %>%
  filter(IA_RUN_COUNT > 1) %>%
  group_by(sub) %>%
  count(sub, tcat, IA_LABEL) %>%
  filter(IA_LABEL == "fixation")
