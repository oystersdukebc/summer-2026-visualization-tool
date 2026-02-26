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

#Importing growth data
DAFGrowthSheet <- read_excel("/home/moviac/Documents/School/Duke/Bass/R/Workspace/LocalData/Growth_12Nov25Filtered.xlsx", sheet = "DUML")
CMTGrowthSheet <- read_excel("/home/moviac/Documents/School/Duke/Bass/R/Workspace/LocalData/Growth_12Nov25Filtered.xlsx", sheet = "CMAST")
WCGrowthSheet <- read_excel("/home/moviac/Documents/School/Duke/Bass/R/Workspace/LocalData/Growth_12Nov25Filtered.xlsx", sheet = "Ward Creek")


#Turn the sheets into a  combined and separate dataframe set
DAFGrowth <- as.data.frame(DAFGrowthSheet)
CMTGrowth <- as.data.frame(CMTGrowthSheet)
WCGrowth <- as.data.frame(WCGrowthSheet)

combined_growth <- bind_rows(WCGrowth, CMTGrowth, DAFGrowth)

#Filtering out baby oysters under 60mm
combined_growth_filtered <- combined_growth[combined_growth$L_mm >= 60,]

# Boxplot function:
generate_boxplots <- function(dataframe) {
  # Check if the required columns are present in the dataframe
  if (!all(c("Strain", "Stage", "L_mm") %in% colnames(dataframe))) {
    stop("The dataframe must contain 'Strain', 'Stage', and 'L_mm' columns.")
  }
  
  #Factor stage and strain so they display in proper order
  dataframe$Stage <-factor(dataframe$Stage, levels = c("May", "July", "September"))
  
  # Create a box plot for each combination of color and month
  p <- ggplot(dataframe, aes(x = Stage, y = L_mm, fill = Site)) +
    geom_boxplot() +
    scale_fill_manual(values = c("CMAST" = "darkolivegreen3", "Duke Aquafarm" = "brown2", "Ward Creek" = "cornflowerblue")) +
    labs(title = "Shell length (mm) of eastern oysters over time at three farm sites", x = "Month", y = "Shell Length (mm)") +
    theme_minimal() +
    facet_grid(.~ Site)  # This line creates separate panels for each unique color
  
  return(p)
}

GrowthSites <- generate_boxplots(combined_growth_filtered)   #[combined_growth_filtered$Strain == 'Triploid',])

GrowthSites

#ggsave("/home/moviac/Documents/School/Duke/Bass/Winter 2026/PosterFigs/OverallBoxesGrowth.png", plot = GrowthSites, bg = "White", width = 10, height = 6)

