

knitr::opts_chunk$set(echo = TRUE)

##Clones Repository to R
system("git clone  https://github.com/oystersdukebc/summer-2026-visualization-tool.git")

##Pulls in repository and sets it as the working directory

setwd("summer-2026-visualization-tool")
system("git pull origin main")  # or replace 'main' with the correct branch

##Setting the "Cleaned" folder as the working directory

setwd("Cleaned")

rm(list = ls())

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)
library(shinyWidgets)

setwd("C:/Users/annab/OneDrive/Desktop/Cleaned")

# time interval function
add_time_columns <- function(df){
  df %>%
    mutate(
      Month = month(DateTime),
      Day = day(DateTime)
    )
}

filter_time <- function(df, month_val, biweek_val){
  if(month_val != "full"){
    df <- df %>% filter(Month == as.numeric(month_val))
  }
  
  if(biweek_val == "1"){
    df <- df %>% filter(Day >= 1 & Day <= 15)
  }
  
  if(biweek_val == "2"){
    df <- df %>% filter(Day >= 16)
  }
  
  df
}

# load data
load_do <- function(file, farm){
  read_excel(file) %>%
    mutate(DateTime = ymd_hms(DateTime),
           DO = as.numeric(DomgL),
           Temp_C = as.numeric(Temp_C),
           Farm = farm) %>%
    select(DateTime, Farm, DO, Temp_C) %>%
    add_time_columns()
}

load_ph <- function(file, farm){
  read_excel(file) %>%
    mutate(DateTime = ymd_hms(DateTime),
           pH = as.numeric(pH),
           Farm = farm) %>%
    select(DateTime, Farm, pH) %>%
    add_time_columns()
}

load_sal <- function(file, farm){
  read_excel(file) %>%
    mutate(DateTime = ymd_hms(DateTime),
           Salinity = as.numeric(Sal_ppt),
           Farm = farm) %>%
    select(DateTime, Farm, Salinity) %>%
    add_time_columns()
}

cmast_do   <- load_do("CMAST_DO_Cleaned.xlsx","CMAST")
cmast_ph   <- load_ph("CMAST_pH_Cleaned.xlsx","CMAST")
cmast_sal  <- load_sal("CMAST_Con_Cleaned.xlsx","CMAST")

stump_do   <- load_do("StumpSound_DO_Cleaned.xlsx","Stump Sound")
stump_ph   <- load_ph("StumpSound_pH_Cleaned.xlsx","Stump Sound")
stump_sal  <- load_sal("StumpSound_Con_Cleaned.xlsx","Stump Sound")

ward_do    <- load_do("WardCreek_DO_Cleaned.xlsx","Ward Creek")
ward_ph    <- load_ph("WardCreek_pH_Cleaned.xlsx","Ward Creek")
ward_sal   <- load_sal("WardCreek_Con_Cleaned.xlsx","Ward Creek")

duml_do    <- load_do("DUML_DO_Cleaned.xlsx","DUML")
duml_ph    <- load_ph("DUML_pH_Cleaned.xlsx","DUML")
duml_sal   <- load_sal("DUML_Con_Cleaned.xlsx","DUML")

nelson_do  <- load_do("Nelson_DO_Cleaned.xlsx","Nelson Bay")
nelson_ph  <- load_ph("Nelson_pH_Cleaned.xlsx","Nelson Bay")
nelson_sal <- load_sal("Nelson_Con_Cleaned.xlsx","Nelson Bay")

ysi <- read_excel("YSI.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         Temp_C = as.numeric(Temp_C),
         Salinity = as.numeric(Sal_ppt),
         pH = as.numeric(pH),
         DO = as.numeric(DomgL),
         Farm = as.character(Farm)) %>%
  select(DateTime, Farm, Temp_C, Salinity, pH, DO) %>%
  add_time_columns()

farm_list  <- c("CMAST","Stump Sound","Ward Creek","DUML","Nelson Bay")
param_list <- c("Temperature","Dissolved Oxygen","pH","Salinity")

colors <- c(
  # CMAST
  "Temp CMAST" = "#729ECE",
  "DO CMAST" = "#ED665D",
  "pH CMAST" = "#67BF5C",
  "Salinity CMAST" = "#ED97CA",
  
  # Stump
  "Temp Stump" = "#AD8BC9",
  "DO Stump" = "#CDCC5D",
  "pH Stump" = "#A8786E",
  "Salinity Stump" = "#FF9E4A",
  
  # Ward
  "Temp Ward" = "#6DCCDA",
  "DO Ward" = "#ff7f0e",
  "pH Ward" = "#1f9e89",
  "Salinity Ward" = "#bc80bd",
  
  # DUML
  "Temp DUML" = "#d62728",
  "DO DUML" = "#1f77b4",
  "pH DUML" = "#9467bd",
  "Salinity DUML" = "#bcbd22",
  
  # Nelson
  "Temp Nelson" = "#2ca02c",
  "DO Nelson" = "#17becf",
  "pH Nelson" = "#EE554E",
  "Salinity Nelson" = "#8c564b"
)

# UI
ui <- fluidPage(
  
  titlePanel("Summer 2026 Data Visualization Tool"),
  
  tabsetPanel(
    id="month_tabs",
    type="tabs",
    tabPanel("Full Summer", value="full"),
    tabPanel("May", value="5"),
    tabPanel("June", value="6"),
    tabPanel("July", value="7"),
    tabPanel("August", value="8"),
    tabPanel("September", value="9")
  ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("Select Time Interval"),
      radioButtons("biweek",
                   NULL,
                   choices=c("Full Month"="full",
                             "First Biweek"="1",
                             "Second Biweek"="2"),
                   selected="full"),
      
      hr(),
      
      h4("Select Farms"),
      checkboxGroupInput("farm_select",NULL,farm_list,selected=farm_list),
      
      hr(),
      
      h4("Select Parameters"),
      checkboxGroupInput("param_select",NULL,param_list,selected=param_list),
      
      hr(),
      
      materialSwitch("show_ysi","Display YSI Data",value=FALSE),
      
      hr(),
      
      h4("Edit Plot Title:"),
      textInput("custom_title", NULL, "Environmental Data")
    ),
    
    mainPanel(
      plotlyOutput("plot",height="700px")
    )
  )
)

# Server
server <- function(input, output){
  
  output$plot <- renderPlotly({
    
    month_val  <- input$month_tabs
    biweek_val <- input$biweek
    
    p <- plot_ly()
    
    all_y_values <- c()
    
    add_trace_auto <- function(data, yvar, name, farm_name){
      
      data_filtered <- filter_time(data, month_val, biweek_val)
      
      if(nrow(data_filtered) > 0 && yvar %in% colnames(data_filtered)){
        
        all_y_values <<- c(all_y_values, data_filtered[[yvar]])
        
        trace_color <- colors[name]
        
        p <<- add_lines(
          p,
          data = data_filtered,
          x = ~DateTime,
          y = as.formula(paste0("~", yvar)),
          name = paste(farm_name, sub(" .*", "", name)),
          line = list(color = trace_color, width = 2)
        )
        
        # YSI
        if(input$show_ysi){
          
          ysi_filtered <- ysi %>%
            filter(Farm == farm_name) %>%
            filter_time(month_val, biweek_val)
          
          if(nrow(ysi_filtered) > 0 && yvar %in% colnames(ysi_filtered)){
            
            p <<- add_markers(
              p,
              data = ysi_filtered,
              x = ~DateTime,
              y = as.formula(paste0("~", yvar)),
              name = paste0(name, " (YSI)"),
              marker = list(
                symbol = "circle",
                size = 6,
                color = trace_color,
                line = list(
                  color = "black",
                  width = 1.5
                )
              ),
              showlegend = FALSE
            )
          }
        }
      }
    }
    
    farms  <- input$farm_select
    params <- input$param_select
    
    show_param <- function(pn) pn %in% params
    
    if("CMAST" %in% farms){
      if(show_param("Temperature")) add_trace_auto(cmast_do,"Temp_C","Temp CMAST", "CMAST")
      if(show_param("Dissolved Oxygen")) add_trace_auto(cmast_do,"DO","DO CMAST", "CMAST")
      if(show_param("pH")) add_trace_auto(cmast_ph,"pH","pH CMAST", "CMAST")
      if(show_param("Salinity")) add_trace_auto(cmast_sal,"Salinity","Salinity CMAST", "CMAST")
    }
    
    if("Stump Sound" %in% farms){
      if(show_param("Temperature")) add_trace_auto(stump_do,"Temp_C","Temp Stump", "Stump Sound")
      if(show_param("Dissolved Oxygen")) add_trace_auto(stump_do,"DO","DO Stump", "Stump Sound")
      if(show_param("pH")) add_trace_auto(stump_ph,"pH","pH Stump", "Stump Sound")
      if(show_param("Salinity")) add_trace_auto(stump_sal,"Salinity","Salinity Stump", "Stump Sound")
    }
    
    if("Ward Creek" %in% farms){
      if(show_param("Temperature")) add_trace_auto(ward_do,"Temp_C","Temp Ward", "Ward Creek")
      if(show_param("Dissolved Oxygen")) add_trace_auto(ward_do,"DO","DO Ward", "Ward Creek")
      if(show_param("pH")) add_trace_auto(ward_ph,"pH","pH Ward", "Ward Creek")
      if(show_param("Salinity")) add_trace_auto(ward_sal,"Salinity","Salinity Ward", "Ward Creek")
    }
    
    if("DUML" %in% farms){
      if(show_param("Temperature")) add_trace_auto(duml_do,"Temp_C","Temp DUML", "DUML")
      if(show_param("Dissolved Oxygen")) add_trace_auto(duml_do,"DO","DO DUML", "DUML")
      if(show_param("pH")) add_trace_auto(duml_ph,"pH","pH DUML", "DUML")
      if(show_param("Salinity")) add_trace_auto(duml_sal,"Salinity","Salinity DUML", "DUML")
    }
    
    if("Nelson Bay" %in% farms){
      if(show_param("Temperature")) add_trace_auto(nelson_do,"Temp_C","Temp Nelson", "Nelson Bay")
      if(show_param("Dissolved Oxygen")) add_trace_auto(nelson_do,"DO","DO Nelson", "Nelson Bay")
      if(show_param("pH")) add_trace_auto(nelson_ph,"pH","pH Nelson", "Nelson Bay")
      if(show_param("Salinity")) add_trace_auto(nelson_sal,"Salinity","Salinity Nelson", "Nelson Bay")
    }
    
    # interactive y-axis rescale
    y_range <- NULL
    if(length(all_y_values) > 0){
      y_range <- range(all_y_values, na.rm=TRUE)
    }
    
    p %>% layout(
      title=input$custom_title,
      xaxis=list(title="Date"),
      yaxis=list(title="Value", range=y_range),
      legend=list(orientation="h",
                  itemwidth = 80)
    )
  })
}

shinyApp(ui, server)
