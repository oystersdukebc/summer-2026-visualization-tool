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
library(zoo)
library(grid)
library(patchwork)

SurvivorshipSheet <- "/home/moviac/Documents/School/Duke/Bass/Winter 2026/Bass Mortality S26_2.xlsx"
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

#Setting site and combining dataframes:
SurvivorshipWC$Source <- "Ward Creek"
SurvivorshipCMT$Source <- "CMAST"
SurvivorshipDAF$Source <- "Duke Aquafarm"
SurvivorshipStump$Source <- "Stump Sound"
SurvivorshipNelson$Source <- "Nelson Bay"

SurvivorshipWC$BagTag <- as.character(SurvivorshipWC$BagTag)
SurvivorshipCMT$BagTag <- as.character(SurvivorshipCMT$BagTag)
SurvivorshipDAF$BagTag <- as.character(SurvivorshipDAF$BagTag)
SurvivorshipStump$BagTag <- as.character(SurvivorshipStump$BagTag)
SurvivorshipNelson$BagTag <- as.character(SurvivorshipNelson$BagTag)

#Combining data, binning by month

S_Combined <- bind_rows(SurvivorshipWC, SurvivorshipCMT, SurvivorshipDAF, SurvivorshipStump, SurvivorshipNelson)
S_Combined$Month <- month(S_Combined$Date, label = TRUE)

#Function
Mortality <- function(dframe) {
  splot = ggplot(dframe, aes(x = Month, y = Mortality_Count, fill = Strain)) +  #Swap "Strain" to "Source" as needed
    ylim(-5, 110) +     #adjust this as needed to fit data
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Mortality distributions of four strains of eastern oyster, CMAST farm site", x = "Month", y = "Mortality count") +
    scale_fill_manual(values = c("New River" = "purple", "Triploid" = "gold", "Stump Sound" = "orange", "Cedar Island" = "blue"))+
    facet_grid(.~ Strain)
  
  
  return(splot)
}

MortCurve <-Mortality(S_Combined[S_Combined$Source == "CMAST",])

MortCurve

#ggsave("/home/moviac/Documents/School/Duke/Bass/Winter 2026/PosterFigs/CMASTMortBxes.png", plot = MortCurve, width = 10, height = 5)


