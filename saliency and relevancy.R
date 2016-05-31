# Packages (may not be using all)
library(dplyr)
library(extrafont)
library(ggplot2)
library(reshape2)
library(splitstackshape)
library(zoo)


# Import fonts
loadfonts(device="win")

setwd("C:/Users/Jessica/Documents/Research/data/data_exp2")
cr_df <- read.csv('Cats_recorder.csv') %>%
  select(tnum, name, position)
fr <- read.delim('fixation_report_4122016.txt', na.strings = c(" ", ".", "NA", ""))

names(cr_df)[names(cr_df)=="tnum"] <- "recorder_trial"

cr_df$position[cr_df$position==6] <- "position_6"
cr_df$position[cr_df$position==7] <- "position_7"
cr_df$position[cr_df$position==8] <- "position_8"
cr_df$position[cr_df$position==9] <- "position_9"
cr_df$position[cr_df$position==4] <- "position_4"
cr_df$position[cr_df$position==5] <- "position_5"
cr_df$position[cr_df$position==0] <- "position_0"
cr_df$position[cr_df$position==1] <- "position_1"
cr_df$position[cr_df$position==2] <- "position_2"
cr_df$position[cr_df$position==3] <- "position_3"


# Trial data-----
td2 <- fr %>%
  filter(!is.na(tnum) &       # Filter out fixation check
           !sub == "UNDEFINEDnull") # Filter out undefined trials

# Filter nearest neighbors------
filter2 <- td2 %>%
  filter(CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE < 2) %>%
  mutate(tutra2 = na.locf(tutra,
                          fromLast  = TRUE,
                          na.rm     = FALSE)) %>%
  filter(!tutra2 == "NA")

# Hit for similar trials------
temp <- (filter2$sim == "True" & 
           filter2$resp == "return")
filter2$rtype[temp] <- "hi"

# False alarm for similar trials------
temp2 <- (filter2$sim == "True" & 
            filter2$resp == "space")
filter2$rtype[temp2] <- "fa"

# Cats Data-----
df_c <- filter2 %>%
  filter(tcateg == "Cats") %>%
  group_by(sub, tnum, CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL) %>%
  mutate(afd = mean(CURRENT_FIX_DURATION)) %>%
  select(sub, tcateg, recorder_trial, CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL, tutra2, afd)

ranks <- read.csv("ranks_edited2.csv", stringsAsFactors=FALSE)

ranks2 <- ranks %>%
  melt(id.vars = c("recorder_trial"))

# Rename columns
names(ranks2)[names(ranks2)=="variable"] <- "saliency_rank"
names(ranks2)[names(ranks2)=="value"] <- "position"

# Sychronize position numbers with position numbers in cats_recorder
ranks2 <- transform(ranks2, saliency_rank = as.character(saliency_rank))
ranks2$saliency_rank[ranks2$saliency_rank=="X1"] <- 1
ranks2$saliency_rank[ranks2$saliency_rank=="X2"] <- 2
ranks2$saliency_rank[ranks2$saliency_rank=="X3"] <- 3
ranks2$saliency_rank[ranks2$saliency_rank=="X4"] <- 4
ranks2$saliency_rank[ranks2$saliency_rank=="X5"] <- 5
ranks2$saliency_rank[ranks2$saliency_rank=="X6"] <- 6
ranks2$saliency_rank[ranks2$saliency_rank=="X7"] <- 7
ranks2$saliency_rank[ranks2$saliency_rank=="X8"] <- 8
ranks2$saliency_rank[ranks2$saliency_rank=="X9"] <- 9
ranks2$saliency_rank[ranks2$saliency_rank=="X10"] <- 10

ranks2$position[ranks2$position==1] <- "position_6"
ranks2$position[ranks2$position==2] <- "position_7"
ranks2$position[ranks2$position==3] <- "position_8"
ranks2$position[ranks2$position==4] <- "position_9"
ranks2$position[ranks2$position==5] <- "position_4"
ranks2$position[ranks2$position==6] <- "position_5"
ranks2$position[ranks2$position==7] <- "position_0"
ranks2$position[ranks2$position==8] <- "position_1"
ranks2$position[ranks2$position==9] <- "position_2"
ranks2$position[ranks2$position==10] <- "position_3"


re_df <- merge(cr_df, ranks2)


split <- cSplit(df_c, "CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL", ".")

names(split)[names(split)=="CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL_1"] <- "relevancy_rank"
names(split)[names(split)=="CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL_2"] <- "name"


# Remove views on fixation cross
split2 <- split %>%
  filter(!is.na(name))

split2 <- transform(split2, relevancy_rank = as.character(relevancy_rank))
split2$relevancy_rank[split2$relevancy_rank=="Cats"] <- 0
split2$relevancy_rank[split2$relevancy_rank=="Dogs"] <- 1
split2$relevancy_rank[split2$relevancy_rank=="Hoofed_Animals"] <- 1
split2$relevancy_rank[split2$relevancy_rank=="Rodentia"] <- 1
split2$relevancy_rank[split2$relevancy_rank=="Reptiles"] <- 2
split2$relevancy_rank[split2$relevancy_rank=="Insects"] <- 2
split2$relevancy_rank[split2$relevancy_rank=="Arachnids"] <- 2
split2$relevancy_rank[split2$relevancy_rank=="Flowers"] <- 3
split2$relevancy_rank[split2$relevancy_rank=="Vegetables"] <- 3
split2$relevancy_rank[split2$relevancy_rank=="Fruits"] <- 3
split2$relevancy_rank[split2$relevancy_rank=="Kitchen_Tools"] <- 4
split2$relevancy_rank[split2$relevancy_rank=="Office_Tools"] <- 4
split2$relevancy_rank[split2$relevancy_rank=="Auto_Mechanic_Tools"] <- 4
split2$relevancy_rank[split2$relevancy_rank=="Tops"] <- 5
split2$relevancy_rank[split2$relevancy_rank=="Bottoms"] <- 5
split2$relevancy_rank[split2$relevancy_rank=="Shoes"] <- 5
split2$relevancy_rank[split2$relevancy_rank=="Boats"] <- 6
split2$relevancy_rank[split2$relevancy_rank=="Cars"] <- 6
split2$relevancy_rank[split2$relevancy_rank=="Aircraft"] <- 6


t_c <-  merge(split2, re_df, by=c("recorder_trial", "name"))


# Contour graph (x = relevancy y = saliency z = fixation duration)
fd_g <- t_c %>%
  group_by(sub, relevancy_rank, saliency_rank) %>%
  mutate(mafd = mean(afd)) %>%
  ungroup() %>%
  select(sub, relevancy_rank, saliency_rank, mafd) %>%
  unique() %>%
  ungroup() %>%
  group_by(relevancy_rank, saliency_rank) %>%
  mutate(mafd2 = mean(mafd)) 

fd_g <- transform(fd_g, relevancy_rank = as.numeric(relevancy_rank))
fd_g <- transform(fd_g, saliency_rank = as.numeric(saliency_rank))

ggplot() +
  geom_point(data = fd_g,
             aes(relevancy_rank,
                 saliency_rank,
                 colour = mafd2,
                 size = mafd2))+
  scale_colour_gradient(low="yellow", high="blue")+
  scale_size(range=c(1,10)) +
  guides(color=guide_legend(), size = guide_legend())+
  theme(axis.title.x      = element_text(vjust = -0.2),
        axis.title.y      = element_text(vjust = 1.2),
        legend.title      = element_blank(),
        legend.text       = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 29),
        panel.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(colour = "white"),
        panel.grid.minor  = element_line(colour = "white"),
        text              = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 29))
non-looks = 0 (average fixation duration 0 ms) <- maybe do this later
likihood = don't look at it , it is a 0; divided by 10 (10 items show up) or number of looks in the trial
  1 if they looked at it, 0 if they didn't: answer would be between 1-0 aka a likihood of looking
mean within subject (bin objects that have the same saliency and relevancy score) then mean across subject
category between 0-6

temp_df <- split2 %>%
  select(recorder_trial, sub) %>%
  unique()

temp_df2 <- merge(temp_df, re_df, by=c("recorder_trial"), all=TRUE, allow.cartesian=TRUE)

temp_df3 <- split2 %>%
  select(recorder_trial, sub, tcateg, name)

l_df <- merge(temp_df2, temp_df3, by=c("recorder_trial", "sub", "name"), all = TRUE) %>%
  mutate(relevancy_rank = name)

l_df <- transform(l_df, relevancy_rank = as.character(relevancy_rank))
l_df$relevancy_rank[grepl("cat",l_df$relevancy_rank)]<-0
l_df$relevancy_rank[grepl("dog",l_df$relevancy_rank)]<-1
l_df$relevancy_rank[grepl("hoo",l_df$relevancy_rank)]<-1
l_df$relevancy_rank[grepl("rod",l_df$relevancy_rank)]<-1
l_df$relevancy_rank[grepl("rep",l_df$relevancy_rank)]<-2
l_df$relevancy_rank[grepl("ins",l_df$relevancy_rank)]<-2
l_df$relevancy_rank[grepl("ara",l_df$relevancy_rank)]<-2
l_df$relevancy_rank[grepl("flo",l_df$relevancy_rank)]<-3
l_df$relevancy_rank[grepl("veg",l_df$relevancy_rank)]<-3
l_df$relevancy_rank[grepl("fru",l_df$relevancy_rank)]<-3
l_df$relevancy_rank[grepl("kit",l_df$relevancy_rank)]<-4
l_df$relevancy_rank[grepl("off",l_df$relevancy_rank)]<-4
l_df$relevancy_rank[grepl("aut",l_df$relevancy_rank)]<-4
l_df$relevancy_rank[grepl("top",l_df$relevancy_rank)]<-5
l_df$relevancy_rank[grepl("bot",l_df$relevancy_rank)]<-5
l_df$relevancy_rank[grepl("sho",l_df$relevancy_rank)]<-5
l_df$relevancy_rank[grepl("boa",l_df$relevancy_rank)]<-6
l_df$relevancy_rank[grepl("car",l_df$relevancy_rank)]<-6
l_df$relevancy_rank[grepl("air",l_df$relevancy_rank)]<-6


l_df <- transform(l_df, tcateg = as.numeric(tcateg))
l_df[is.na(l_df)] <- 0
l_df$tcateg[l_df$tcateg=="Cats"] <- 1
names(l_df)[names(l_df)=="tcateg"] <- "viewed"
l_df <- transform(l_df, name = as.character(name))

# Likihood graph
l_g <- l_df %>%
  group_by(recorder_trial, name) %>%
  mutate(lp = mean(viewed)) %>%
  ungroup() %>%
  select(recorder_trial, name, relevancy_rank, saliency_rank, lp) %>%
  unique()

l_g <- transform(l_g, relevancy_rank = as.numeric(relevancy_rank))
l_g <- transform(l_g, saliency_rank = as.numeric(saliency_rank))

ggplot() +
  geom_point(data = l_g,
             aes(relevancy_rank,
                 saliency_rank,
                 colour = lp,
                 size = lp))+
  scale_colour_gradient(low="yellow", high="blue")+
  scale_size(range=c(1,10)) +
  guides(color=guide_legend(), size = guide_legend())+
  theme(axis.title.x      = element_text(vjust = -0.2),
        axis.title.y      = element_text(vjust = 1.2),
        legend.title      = element_blank(),
        legend.text       = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 29),
        panel.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(colour = "white"),
        panel.grid.minor  = element_line(colour = "white"),
        text              = element_text(face   = "bold",
                                         family = "Times New Roman",
                                         size   = 29))
