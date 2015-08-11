# Paper Graphs
library(broom)
library(dplyr)
library(extrafont)
library(ggplot2)
library(grid)
library(lme4)
library(tidyr)
library(zoo)

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
  select(sub, rtype, mtut, mrt, mrtr, mtutr) %>%
  unique() %>%
  mutate(mrtrc = mrtr - mean(mrtr), # Mean Centered Reaction Time for Correct Rejection, False Alarm, Hit, and Miss
         mrtc  = (mrt - mean(mrt))) # Mean Centered Reaction Time
         
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
    select(sub, mtut) %>%
    unique()

  ggplot(g, aes(mtut))+
    geom_histogram(breaks = c(0, 1, 2, 3, 4, 5),
                   colour = "dimgrey",
                   fill   = "black")+
    labs(list(x = "Mean TUT Score",
              y = "Number of Participants"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))

  # Graph 2: dprime versus mean TUT and remove subject 12_sw because of 0 false alarm rate
  g2 <- pd %>%
    filter(!sub == "12_sw") %>% 
    select(mtut, dp) %>%
    unique()
  cor.test(g2$mtut, g2$dp)
  
  ggplot(g2, aes(mtut, dp))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    geom_smooth(colour  = "black",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "D'"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))
  
  # Linear regression for mean TUT score and centered mean RT by correct rejection, false alarm, hit, and miss
  lr <- lm(mtutr ~ mrtrc * rtype, cd)
  summary(lr)
  
  # Graph 3: mean RT versus mean TUT score
  g3 <- cd %>%
    select(sub, mtut, mrtc) %>%
    unique()
  cor.test(g3$mtut, g3$mrtc)
  
  ggplot(g3, aes(mtut, mrtc))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    geom_smooth(colour  = "black",
                method  = "lm")+
    labs(list(x = "Mean TUT Score",
              y = "Mean Reaction Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))
  

  
# Eye Data: between subjects----------------------
edb <- pd %>%
  mutate(rts = rt/1000) %>%
  group_by(sub) %>%
  mutate(scrt   = SACCADE_COUNT/rts,                              # Saccade Count/RT
         mscrt  = mean(scrt),                                     # Mean Saccade Count/RT
         afdrt  = AVERAGE_FIXATION_DURATION/rts,                  # Average Fixation Duration/RT
         mafdrt = mean(afdrt, na.rm = TRUE),                      # Mean Average Fixation Duration/RT
         viacrt = VISITED_INTEREST_AREA_COUNT/rts,                # Visited Interest Area Count/RT
         miacrt = mean(viacrt)) %>%                               # Mean Visited Interest Area Count/RT
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT) %>%  # IA Run Count-Trial Total Visited IA Count
  group_by(sub) %>%
  mutate(mr = mean(r, na.rm = TRUE))                              # Mean Refixations

# Centered Data and remove abnormalities
cd2 <- edb %>%
  ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tcat, rtype, mtut, mscrt, mafdrt, miacrt, mr) %>%
  unique() %>%
  mutate(msc = (mscrt - mean(mscrt)),    # Mean Centered Mean Saccade Count/RT
         mfc = (mafdrt - mean(mafdrt)),  # Mean Centered Mean Average Fixation Duration/RT 
         mvc = (miacrt - mean(miacrt)))  # Mean Centered Mean Visited Interest Area Count/RT

  # Table 2: correlation matrix of centered mean TUT score, centered mean saccade count/RT, cenetered mean average fixation druation/RT, and centered mean refixations
  cm <- cd2 %>%
    select(mscrt, mafdrt, mr) %>%
    unique()
    cor(cm)

  # Linear regression for centered mean TUT score, centered mean saccade count/RT, and refixations
  lr2a <- cd2 %>%
    select(sub, mtut, msc, mr) %>%
    unique()

  lr2b <-lm(mtut ~ msc + mr, lr2a)
  summary(lr2b)

  # Graph 4a: centered mean saccade count/rt versus centered mean TUT score
  g4a <- cd2 %>%
    select(mtut, msc) %>%
    unique()
  cor.test(g4a$mtut, g4a$msc)
  
  ggplot(g4a, aes(mtut, msc))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    geom_smooth(colour = "black",
                method  = "lm")+
    labs(list(x = "Mean TUT Score",
              y = "Mean Saccade Count\nper Unit Time"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))
  
  # Graph 5a: centered mean refixations versus centered mean TUT score
  g5a <- cd2 %>%
    select(mtut, mr) %>%
    unique()
  cor.test(g5a$mtut, g5a$mr)
  
  ggplot(g5a, aes(mtut, mr))+
    geom_point(colour = "dimgrey",
               size = 2)+
    geom_smooth(colour = "black",
                method = "lm",
                se = FALSE)+
    labs(list(x = "Mean TUT Score", 
              y = "Mean Refixations"))+
    theme(axis.title.x      = element_text(vjust = -0.2),
          axis.title.y      = element_text(vjust = 1.2),
          legend.text       = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22),
          panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(colour = "white"),
          panel.grid.minor  = element_line(colour = "white"),
          text              = element_text(face   = "bold",
                                           family = "Times New Roman",
                                           size   = 22))
  
# Eye Data: within subject----------------------
edw <- pd %>%
  mutate(rts = rt/1000) %>%
  group_by(sub) %>%
  mutate(scrt  = SACCADE_COUNT/rts,                           # Saccade Count/RT
         afdrt = AVERAGE_FIXATION_DURATION/rts) %>%           # Average Fixation Duration/RT
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT)  # IA Run Count-Trial Total Visited IA Count

# Centered Data and remove abnormalities
cd3 <- edw %>%
 ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tnum, r, scrt, afdrt, tutra2) %>%
  unique() %>%
  group_by(sub) %>%
  mutate(sc = (scrt - mean(scrt)),                  # Mean Centered Saccade Count/RT
         fc = (afdrt - mean(afdrt, na.rm = TRUE)))  # Mean Centered Average Fixation Duration/RT
 
  # Table 3: correlation matrix of centered mean TUT score, centered mean saccade count/RT, cenetered mean average fixation druation/RT, and centered mean refixations 
  c2 <- cd3 %>%
    filter(!sub == "31_lc") %>%
    ungroup() %>%
    group_by(sub) %>%
    select(sc, fc, r) %>%
    do(tidy(cor.test(~sc + r, .))) %>% 
    mutate(comp = 'sc_rc')
  
  bigdf <- bind_rows(c1, c2, c3)
  
  acm <- bigdf %>%
    group_by(comp) %>%
    summarise(cs = mean(estimate),
              ps = mean(p.value))
  
  
  # Linear mixed-effects model for TUT, centered saccade count/RT, and refixations
  lmem <- lmer(tutra2 ~ sc + r + (sc + r|sub), cd3)
  summary(lmem)
  

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