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
         mtut   = mean(tutra2)) %>%                                               # Mean TUT
  group_by(sub, rtype) %>%
  mutate(mrt = mean(rt))                                                          # Mean Reaction Time for Correct Rejection, False Alarm, Hit, and Miss

  # Table 1: number of participants with rtype for tut level
  t <- pd %>%
    count(rtype, tutra2) %>%
    spread(rtype, n)

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

  # Graph 3: mean RT versus mean TUT score by correct rejection, false alarm, hit, and miss
  g3 <- pd %>%
    select(sub, mtut, mrt, rtype) %>%
    unique()

  g3cr <- g3 %>%
    filter(rtype == "cr")
  cor.test(g3cr$mtut, g3cr$mrt)

  g3fa <- g3 %>%
    filter(rtype == "fa")
  cor.test(g3fa$mtut, g3fa$mrt)

  g3h <- g3 %>%
    filter(rtype == "hi")
  cor.test(g3h$mtut, g3h$mrt)

  g3m <- g3 %>%
    filter(rtype == "mi")
  cor.test(g3m$mtut, g3m$mrt)
  
    # Changing names of rtype
    levels(g3$rtype)[levels(g3$rtype)=="cr"] <- "Correct Rejection"
    levels(g3$rtype)[levels(g3$rtype)=="fa"] <- "False Alarm"
    levels(g3$rtype)[levels(g3$rtype)=="hi"] <- "Hit"
    levels(g3$rtype)[levels(g3$rtype)=="mi"] <- "Miss"
  
  ggplot(g3, aes(mtut, mrt))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour  = "black",
                method  = "lm",
                se      = FALSE)+
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
  group_by(sub) %>%
  mutate(scrt   = SACCADE_COUNT/rt,                               # Saccade Count/RT
         mscrt  = mean(scrt),                                     # Mean Saccade Count/RT
         afdrt  = AVERAGE_FIXATION_DURATION/rt,                   # Average Fixation Duration/RT
         mafdrt = mean(afdrt, na.rm = TRUE),                      # Mean Average Fixation Duration/RT
         viacrt = VISITED_INTEREST_AREA_COUNT/rt,                 # Visited Interest Area Count/RT
         miacrt = mean(viacrt)) %>%                               # Mean Visited Interest Area Count/RT
  group_by(sub, rtype) %>%
  mutate(mscrtr = mean(scrt),                                     # Mean Saccade Count/RT for Correct Rejection, False Alarm, Hit, and Miss
         mfdrtr = mean(afdrt, na.rm = TRUE)) %>%                  # Mean Average Fixation Duration/RT for Correct Rejection, False Alarm, Hit, and Miss
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT) %>%  #IA Run Count-Trial Total Visited IA Count
  group_by(sub) %>%
  mutate(mr = mean(r, na.rm = TRUE)) %>%                          # Mean Refixations
  group_by(sub, rtype) %>% 
  mutate(mrr = mean(r, na.rm = TRUE))                             # Mean Refixations for Correct Rejection, False Alarm, Hit, and Miss

# Z-normalized Data and remove abnormalities
zd <- edb %>%
  ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tcat, rtype, mtut, mscrt, mafdrt, miacrt, mr, mscrtr, mfdrtr, mrr) %>%
  unique() %>%
  mutate(mtc = (mtut - mean(mtut))/sd(mtut),        # Centered Mean TUT Score
         msc = (mscrt - mean(mscrt))/sd(mscrt),     # Centered Mean Saccade Count/RT
         mfc = (mafdrt - mean(mafdrt))/sd(mafdrt),  # Centered Mean Average Fixation Duration/RT 
         mvc = (miacrt - mean(miacrt)/sd(miacrt)),  # Centered Mean Visited Interest Area Count/RT
         mrc = (mr - mean(mr))/sd(mr),              # Centered Mean Refixations
         mscr = (mscrtr - mean(mscrtr))/sd(mscrtr), # Centered Mean Saccade Count/RT for Correct Rejection, False Alarm, Hit, and Miss
         mfcr = (mfdrtr - mean(mfdrtr))/sd(mfdrtr), # Centered Mean Average Fixation Duration/RT for Correct Rejection, False Alarm, Hit, and Miss
         mrcr = (mrr - mean(mrr))/sd(mrr))          # Centered Mean Refixations for Correct Rejection, False Alarm, Hit, and Miss

  # Table 2: correlation matrix of centered mean TUT score, centered mean saccade count/RT, cenetered mean average fixation druation/RT, and centered mean refixations
  cm <- zd %>%
    select(mtut, msc, mfc, mrc) %>%
    unique()
    cor(cm)

  # Linear regression for centered mean TUT score, centered mean saccade count/RT, and centered refixations
  lr <- lm(mtut ~ msc + mrc, cm)
  summary(lr)

  # Graph 4a: centered mean saccade count/rt versus centered mean TUT score
  g4a <- zd %>%
    select(tcat, mtc, msc) %>%
    unique()
  cor.test(g4a$mtc, g4a$msc)
  
  ggplot(g4a, aes(mtc, msc, color = tcat))+
    geom_point(
               size   = 2)+
    geom_smooth(colour = "black",
                method  = "lm",
                se      = FALSE)+
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
  g5a <- zd %>%
    select(mtc, mrc) %>%
    unique()
  cor.test(g5a$mtc, g5a$mrc)
  
  ggplot(g5a, aes(mtc, mrc))+
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
  group_by(sub) %>%
  mutate(scrt   = SACCADE_COUNT/rt,                           # Saccade Count/RT
         afdrt  = AVERAGE_FIXATION_DURATION/rt) %>%           # Average Fixation Duration/RT
  group_by(sub, tnum) %>%
  mutate(r = sum(IA_RUN_COUNT)-TRIAL_TOTAL_VISITED_IA_COUNT)  # IA Run Count-Trial Total Visited IA Count

# Z-normalized Data and remove abnormalities
zd2 <- edw %>%
 ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tnum, r, scrt, afdrt, tutra2) %>%
  unique() %>%
  group_by(sub) %>%
  mutate(tc = (tutra2 - mean(tutra2))/sd(tutra2),                           # Centered TUT Score
         sc = (scrt - mean(scrt))/sd(scrt),                                 # Centered Saccade Count/RT
         fc = (afdrt - mean(afdrt, na.rm = TRUE))/sd(afdrt, na.rm = TRUE),  # Centered Average Fixation Duration/RT
         rc = (r - mean(r))/sd(r))                                          # Centered Refixations
 
  # Table 3: correlation matrix of centered mean TUT score, centered mean saccade count/RT, cenetered mean average fixation druation/RT, and centered mean refixations 
  corsub1 <- zd2 %>%
    filter(!sub == "31_lc") %>%
    ungroup() %>%
    group_by(sub) %>%
    select(tc, sc, fc, rc) %>%
    do(tidy(cor.test(~tc + sc, .))) %>% 
    mutate(comp = 'tc_sc')
  
  bigdf <- bind_rows(corsub1, corsub2, corsub3)
  
  cor <- bigdf %>%
    group_by(comp) %>%
    summarise(cs = (mean(estimate)))
  
  
  
  
  
  
  # Graph: mean saccade count/rt versus mtut by correct rejection, false alarm, hit, and miss
  ct3b <- edr %>%
    subset(select = c(sub, mtut, mscrtr, rtype)) %>%
    unique()
  
  ct3cr <- ct3b %>%
    filter(rtype == "cr")
  cor.test(ct3cr$mtut, ct3cr$mscrt)
  
  ct3fa <- ct3b %>%
    filter(rtype == "fa")
  cor.test(ct3fa$mtut, ct3fa$mscrt)
  
  ct3h <- ct3b %>%
    filter(rtype == "hi")
  cor.test(ct3h$mtut, ct3h$mscrt)
  
  ct3m <- ct3b %>%
    filter(rtype == "mi")
  cor.test(ct3m$mtut, ct3m$mscrt)
  
    # Changing names of rtype
    levels(ct3b$rtype)[levels(ct3b$rtype)=="cr"] <- "Correct Rejection"
    levels(ct3b$rtype)[levels(ct3b$rtype)=="fa"] <- "False Alarm"
    levels(ct3b$rtype)[levels(ct3b$rtype)=="hi"] <- "Hit"
    levels(ct3b$rtype)[levels(ct3b$rtype)=="mi"] <- "Miss"
  
  ggplot(ct3b, aes(mtut, mscrtr))+
    geom_point(colour = 'dimgrey',
               size   = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour = "black",
                method  = "lm",
                se      = FALSE)+
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

  # Graph: mean average fixation duration/rt
  ct4a <- ed %>%
    subset(select = c(sub, mtut, mafdrt)) %>%
    unique()
  
  ggplot(ct4a, aes(mtut, mafdrt))+
    geom_point(colour = 'dimgrey',
               size   = 2)+
    geom_smooth(colour = "black",
                method  = "lm",
                se      = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Average Fixation\nDuration per Unit Time"))+
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

  # Graph: mean average fixation duration/rt versus mtut by correct rejection, false alarm, hit, and miss
  ct4b <- edr %>%
    subset(select = c(sub, mtut, mfrtr, rtype)) %>%
    unique()
  
  ct4cr <- ct4b %>%
    filter(rtype == "cr")
  cor.test(ct4cr$mtut, ct4cr$mfrtr)
  
  ct4fa <- ct4b %>%
    filter(rtype == "fa")
  cor.test(ct4fa$mtut, ct4fa$mfrtr)
  
  ct4h <- ct4b %>%
    filter(rtype == "hi")
  cor.test(ct4h$mtut, ct4h$mfrtr)
  
  ct4m <- ct4b %>%
    filter(rtype == "mi")
  cor.test(ct4m$mtut, ct4m$mfrtr)

    # Changing names of rtype
    levels(ct4b$rtype)[levels(ct4b$rtype)=="cr"] <- "Correct Rejection"
    levels(ct4b$rtype)[levels(ct4b$rtype)=="fa"] <- "False Alarm"
    levels(ct4b$rtype)[levels(ct4b$rtype)=="hi"] <- "Hit"
    levels(ct4b$rtype)[levels(ct4b$rtype)=="mi"] <- "Miss"
  
  ggplot(ct4b, aes(mtut, mfrtr))+
    geom_point(colour = "dimgrey",
               size   = 2)+
    facet_wrap(~rtype)+
    geom_smooth(colour = "black",
                method = "lm", 
                se = FALSE)+
    labs(list(x = "Mean TUT Score",
              y = "Mean Average Fixation\nDuration per Unit Time"))+
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

  # Graph: mean visited interest area count/rt v mean TUT
  ct5 <- ed2 %>%
    subset(select = c(sub, mtut, miacrt)) %>%
    unique()
  cor.test(ct5$mtut, ct5$miacrt)
  
  ggplot(ct5, aes(mtut, miacrt))+
    geom_point(colour = "dimgrey",
               size = 2)+
    geom_smooth(colour = "black",
                method = "lm",
                se = FALSE)+
    labs(list(x = "Mean TUT Score", 
              y = "Mean Visited Interest Area\nCount per Unit Time"))+
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

# Graph: mean interest area count v mean average fixation duration per unit time
ct6 <- ed2 %>%
  subset(select = c(sub, mafdrt, miarc)) %>%
  unique()
cor.test(ct6$mafdrt, ct6$miarc)

ggplot(ct6, aes(mafdrt, miarc))+
  geom_point(colour = "dimgrey",
             size = 2)+
  geom_smooth(colour = "black",
              method = "lm",
              se = FALSE)+
  labs(list(x = "Mean Average Fixation Duration Per Unit Time", 
            y = "Mean Interest Area Run Count"))+
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

  

  # Graph: mean IA run count-trial total visited IA count versus mtut by correct rejection, false alarm, hit, and miss
  ct5b <- ed4r %>%
    subset(select = c(sub, mtut, mrr, rtype)) %>%
    unique()

  ct5cr <- ct5b %>%
    filter(rtype == "cr")
  cor.test(ct5cr$mtut, ct5cr$mrr)
  
  ct5fa <- ct5b %>%
    filter(rtype == "fa")
  cor.test(ct5fa$mtut, ct5fa$mrr)
  
  ct5h <- ct5b %>%
    filter(rtype == "hi")
  cor.test(ct5h$mtut, ct5h$mrr)
  
  ct5m <- ct5b %>%
    filter(rtype == "mi")
  cor.test(ct5m$mtut, ct5m$mrr)
  
    # Changing names of rtype
    levels(ct5b$rtype)[levels(ct5b$rtype)=="cr"] <- "Correct Rejection"
    levels(ct5b$rtype)[levels(ct5b$rtype)=="fa"] <- "False Alarm"
    levels(ct5b$rtype)[levels(ct5b$rtype)=="hi"] <- "Hit"
    levels(ct5b$rtype)[levels(ct5b$rtype)=="mi"] <- "Miss"
  
  ggplot(ct5b, aes(mtut, mrr))+
    geom_point(colour = "dimgrey",
               size = 2)+
    facet_wrap(~rtype)+
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


# Linear Model
lm <- ed4 %>%
  ungroup() %>%
  filter(r< 25, scrt > 0) %>%
  select(sub, mscrt, mafdrt, mtut, mr) %>%
  unique() %>%
  mutate(tc = (mtut - mean(mtut))/sd(mtut),
         sc = (mscrt - mean(mscrt))/sd(mscrt),
         fc = (mafdrt - mean(mafdrt, na.rm = TRUE))/sd(mafdrt, na.rm = TRUE),
         rc = (mr - mean(mr))/sd(mr))

cor(select(lm, mr, mscrt, mafdrt, mtut))
#cor.test(~ mscrt + mafdrt, new)

sr <- lm(tc ~ rc + sc, lm)
summary(sr)

lmr <- ed4r %>%
  ungroup() %>%
  filter(r< 25, scrtr > 0) %>%
  select(sub, mscrtr, mfrtr, mtut, mrr, rtype) %>%
  unique() %>%
  mutate(tc = (mtut - mean(mtut))/sd(mtut),
         sc = (mscrtr - mean(mscrtr))/sd(mscrtr),
         fc = (mfrtr - mean(mfrtr, na.rm = TRUE))/sd(mfrtr, na.rm = TRUE),
         rc = (mrr - mean(mrr))/sd(mrr))

srr <- lm(tc ~ rc*rtype + sc*rtype, lmr)
summary(srr)

ggplot(lm, aes(tc, sc))+
  geom_point(colour = 'dimgrey',
             size   = 2)+
  geom_smooth(colour = "black",
              method  = "lm",
              se      = FALSE)+
  labs(list(x = "Mean TUT Score",
            y = "Mean Saccade Count"))+
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

ggplot(lmr, aes(tc, sc))+
  geom_point(colour = 'dimgrey',
             size   = 2)+
  facet_wrap(~rtype)+
  geom_smooth(colour = "black",
              method  = "lm",
              se      = FALSE)+
  labs(list(x = "Mean TUT Score",
            y = "Mean Saccade Count"))+
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
  
# Linear Mixed-Effects Model
lmem <- ed4 %>%
  ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tnum, r, scrt, afdrt, tutra2) %>%
  unique() %>%
  group_by(sub) %>%
  mutate(rc = (r - mean(r))/sd(r),
         sc = (scrt - mean(scrt))/sd(scrt),
         fc = (afdrt - mean(afdrt, na.rm = TRUE))/sd(afdrt, na.rm = TRUE),
         tc = (tutra2 - mean(tutra2))/sd(tutra2))


  
  
 # mean r values or look at distribution on a plot 
all <- lmer(tc ~ sc + fc + rc + (sc + fc + rc|sub), lmem)
summary(all)
  
lmemr <- ed4r %>%
  ungroup() %>%
  filter(r < 25, scrt > 0) %>%
  select(sub, tnum, r, scrt, afdrt, tutra2, rtype) %>%
  unique() %>%
  group_by(sub) %>%
  mutate(rc = (r - mean(r))/sd(r),
         sc = (scrt - mean(scrt))/sd(scrt),
         fc = (afdrt - mean(afdrt, na.rm = TRUE))/sd(afdrt, na.rm = TRUE),
         tc = (tutra2 - mean(tutra2))/sd(tutra2))

allr <- lmer(tc ~ sc*rtype + fc*rtype + rc*rtype + (sc*rtype + fc*rtype + rc*rtype|sub), lmemr)
summary(allr)

anova(rc, scrc)

cc <- ed4 %>%
  select(sub, tcat, hirate, crrate, farate, mirate) %>%
  unique()

ggplot(cc, aes(tcat, crrate))+
  geom_boxplot(colour = "dimgrey")+
  labs(list(x = "Category",
            y = "Correct Rejection Rate"))+
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

ggplot(cc, aes(tcat, hirate))+
  geom_boxplot(colour = "dimgrey")+
  labs(list(x = "Category",
            y = "Hit Rate"))+
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
