

#Library import

library(ggplot2)
library(readr)
library(dplyr)
library(GGally)
library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(tidyr)
library(hms)
library(paletteer)
library(zoo)
library(grid)
library(patchwork)
library (signal)


#Loading mortality and survivorship data from master excel (change to match your directory):
SurvivorshipSheet <- "C:\\Users\\drich\\Downloads\\Bass Mortality S26_2.xlsx"
SurvivorshipSheetWC <- read_excel(SurvivorshipSheet, sheet = "Ward Creek")
SurvivorshipSheetCMT <- read_excel(SurvivorshipSheet, sheet = "CMAST")
SurvivorshipSheetDAF <- read_excel(SurvivorshipSheet, sheet = "Duke Aquafarm")
SurvivorshipSheetStump <- read_excel(SurvivorshipSheet, sheet = "Stump Sound")
SurvivorshipSheetNelson <- read_excel(SurvivorshipSheet, sheet = "Nelson Bay")

SurvivorshipWC <- as.data.frame(SurvivorshipSheetWC)
SurvivorshipCMT <- as.data.frame(SurvivorshipSheetCMT)
SurvivorshipDAF <- as.data.frame(SurvivorshipSheetDAF)
SurvivorshipStump <- as.data.frame(SurvivorshipSheetStump)
SurvivorshipNelson <- as.data.frame(SurvivorshipSheetNelson)

#Setting site:
SurvivorshipWC$Source <- "Ward Creek"
SurvivorshipCMT$Source <- "CMAST"
SurvivorshipDAF$Source <- "Duke Aquafarm"
SurvivorshipStump$Source <- "Stump Sound"
SurvivorshipNelson$Source <- "Nelson Bay"


#formatting bag tags
SurvivorshipWC$BagTag <- as.character(SurvivorshipWC$BagTag)
SurvivorshipCMT$BagTag <- as.character(SurvivorshipCMT$BagTag)
SurvivorshipDAF$BagTag <- as.character(SurvivorshipDAF$BagTag)
SurvivorshipStump$BagTag <- as.character(SurvivorshipStump$BagTag)
SurvivorshipNelson$BagTag <- as.character(SurvivorshipNelson$BagTag)

#This is the final dataframe from which we will filter:
S_Combined <- bind_rows(SurvivorshipWC, SurvivorshipCMT, SurvivorshipDAF, SurvivorshipStump, SurvivorshipNelson)


#Calculating and plotting error bars (binning data by month):
SummarySubset <- S_Combined#[S_Combined$Source == "Duke Aquafarm",]        #uncomment the bracket to select a certain site, strain, etc.
SummarySubset$Month <- month(SummarySubset$Date, label = TRUE)

SummaryGroup <- SummarySubset %>%
  group_by(Strain, Month) %>%
  summarise_at(vars(Mortality_Count), list(mean=mean, sd = sd)) %>%
  as.data.frame()

statplot <- ggplot(SummaryGroup) +  #Swap "Strain" with "Source" as needed below
  ylim(0, 50) +
  geom_line(aes(x=Month, y = mean, color = Strain, group = Strain, ymin=mean-sd, ymax=mean+sd), position = position_dodge(width=5)) +
  labs(title = "", x = "Month", y = "Mean mortality count") +
  #by site
  #scale_color_manual(values = c("CMAST" = "darkgreen", "Duke Aquafarm" = "red", "Ward Creek" = "blue", "Stump Sound" = "orange", "Nelson Bay" = "turquoise"))
  #by strain
  scale_color_manual(values = c("New River" = "purple", "Triploid" = "gold", "Stump Sound" = "orange", "Cedar Island" = "blue"))

print (statplot)

ggsave("/home/moviac/Documents/School/Duke/Bass/Winter 2026/PosterFigs/MortBars.png", plot = statplot, width = 6, height = 4)
#swap to your save directory

