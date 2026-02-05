library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(readr)
library(readxl)

# Load data
# CMAST
cmast_do <- read_excel("data/CMAST_DO_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         DO = as.numeric(DomgL),
         Temp_C = as.numeric(Temp_C),
         Farm = "CMAST") %>%
  select(DateTime, Farm, DO, Temp_C)

cmast_ph <- read_excel("data/CMAST_pH_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         pH = as.numeric(pH),
         Farm = "CMAST") %>%
  select(DateTime, Farm, pH)

cmast_sal <- read_excel("data/CMAST_Con_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         Salinity = as.numeric(Sal_ppt),
         Farm = "CMAST") %>%
  select(DateTime, Farm, Salinity)

# Stump Sound
stump_do <- read_excel("data/StumpSound_DO_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         DO = as.numeric(DomgL),
         Temp_C = as.numeric(Temp_C),
         Farm = "Stump Sound") %>%
  select(DateTime, Farm, DO, Temp_C)

stump_ph <- read_excel("data/StumpSound_pH_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         pH = as.numeric(pH),
         Farm = "Stump Sound") %>%
  select(DateTime, Farm, pH)

stump_sal <- read_excel("data/StumpSound_Con_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         Salinity = as.numeric(Sal_ppt),
         Farm = "Stump Sound") %>%
  select(DateTime, Farm, Salinity)

# Ward Creek
ward_do <- read_excel("data/WardCreek_DO_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         DO = as.numeric(DomgL),
         Temp_C = as.numeric(Temp_C),
         Farm = "Ward Creek") %>%
  select(DateTime, Farm, DO, Temp_C)

ward_ph <- read_excel("data/WardCreek_pH_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         pH = as.numeric(pH),
         Farm = "Ward Creek") %>%
  select(DateTime, Farm, pH)

ward_sal <- read_excel("data/WardCreek_Con_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         Salinity = as.numeric(Sal_ppt),
         Farm = "Ward Creek") %>%
  select(DateTime, Farm, Salinity)

# DUML
duml_ph <- read_excel("data/DUML_pH_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         pH = as.numeric(pH),
         Farm = "DUML") %>%
  select(DateTime, Farm, pH)

duml_do <- read_excel("data/DUML_DO_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         DO = as.numeric(DomgL),
         Temp_C = as.numeric(Temp_C),
         Farm = "DUML") %>%
  select(DateTime, Farm, DO, Temp_C)

duml_sal <- read_excel("data/DUML_Con_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         Salinity = as.numeric(Sal_ppt),
         Farm = "DUML") %>%
  select(DateTime, Farm, Salinity)

# Nelson Bay
nelson_do <- read_excel("data/Nelson_DO_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         DO = as.numeric(DomgL),
         Temp_C = as.numeric(Temp_C),
         Farm = "Nelson Bay") %>%
  select(DateTime, Farm, DO, Temp_C)

nelson_ph <- read_excel("data/Nelson_pH_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         pH = as.numeric(pH),
         Farm = "Nelson Bay") %>%
  select(DateTime, Farm, pH)

nelson_sal <- read_excel("data/Nelson_Con_Final.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         Salinity = as.numeric(Sal_ppt),
         Farm = "Nelson Bay") %>%
  select(DateTime, Farm, Salinity)

ysi <- read_excel("data/YSI.xlsx") %>%
  mutate(DateTime = ymd_hms(DateTime),
         Temp_C = as.numeric(Temp_C),
         Salinity = as.numeric(Sal_ppt),
         pH = as.numeric(pH),
         DO = as.numeric(DomgL),
         Farm = as.character(Farm)) %>%
  select(DateTime, Farm, Temp_C, Salinity, pH, DO)

all_farms <- list(
  "CMAST" = list(
    do  = cmast_do,
    ph  = cmast_ph,
    sal = cmast_sal,
    ysi = ysi %>% filter(Farm == "CMAST")
  ),
  
  "Stump Sound" = list(
    do  = stump_do,
    ph  = stump_ph,
    sal = stump_sal,
    ysi = ysi %>% filter(Farm == "Stump Sound")
  ),
  
  "Ward Creek" = list(
    do  = ward_do,
    ph  = ward_ph,
    sal = ward_sal,
    ysi = ysi %>% filter(Farm == "Ward Creek")
  ),
  
  "DUML" = list(
    do  = duml_do,
    ph  = duml_ph,
    sal = duml_sal,
    ysi = ysi %>% filter(Farm == "DUML")
  ),
  
  "Nelson Bay" = list(
    do  = nelson_do,
    ph  = nelson_ph,
    sal = nelson_sal,
    ysi = ysi %>% filter(Farm == "Nelson Bay")
  )
)

farm_list <- c("CMAST", "Stump Sound", "Ward Creek", "DUML", "Nelson Bay")
param_list <- c("Temperature", "Dissolved Oxygen", "pH", "Salinity")

# Assign colors
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
  "Temp Ward Creek" = "#6DCCDA",
  "DO Ward Creek" = "#ff7f0e",
  "pH Ward Creek" = "#1f9e89",
  "Salinity Ward Creek" = "#bc80bd",
  
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
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Farms:"),
      checkboxGroupInput(
        "farm_select", NULL,
        choices = farm_list,
        selected = farm_list
      ),
      
      br(),
      
      h4("Select Parameters:"),
      checkboxGroupInput(
        "param_select", NULL,
        choices = param_list,
        selected = param_list
      ),
      
      br(),
      
      h4("Edit Plot Title:"),
      textInput("custom_title", NULL, "Full Summer Environmental Data")
    ),
    
    mainPanel(
      plotlyOutput("plot", height = "650px")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$plot <- renderPlotly({
    p <- plot_ly()
    
    add_trace_safe <- function(p, data, x, y, name){
      if(!is.null(data[[y]])) {
        p <- add_lines(p, x = data[[x]], y = data[[y]],
                       name = name,
                       line = list(color = colors[name]))
      }
      p
    }
    
    add_ysi_safe <- function(p, data, x, y, line_name){
      if(!is.null(data[[y]]) && nrow(data) > 0){
        p <- add_markers(
          p,
          x = data[[x]],
          y = data[[y]],
          name = paste(line_name, "(YSI)"),
          marker = list(
            symbol = "circle",
            size = 6,
            color = colors[[line_name]],  # match the line color
            line = list(
              color = "black",           # black outline
              width = 1.5
            )
          )
        )
      }
      p
    }
    
    farms <- input$farm_select
    params <- input$param_select
    
    show_param <- function(param_name) param_name %in% params

    if("CMAST" %in% farms){
      y <- ysi %>% filter(Farm == "CMAST")
      
      if(show_param("Temperature")){
        p <- add_trace_safe(p, cmast_do, "DateTime", "Temp_C", "Temp CMAST")
        p <- add_ysi_safe(p, y, "DateTime", "Temp_C", "Temp CMAST")
      }
      if(show_param("Dissolved Oxygen")){
        p <- add_trace_safe(p, cmast_do, "DateTime", "DO", "DO CMAST")
        p <- add_ysi_safe(p, y, "DateTime", "DO", "DO CMAST")
      }
      if(show_param("pH")){
        p <- add_trace_safe(p, cmast_ph, "DateTime", "pH", "pH CMAST")
        p <- add_ysi_safe(p, y, "DateTime", "pH", "pH CMAST")
      }
      if(show_param("Salinity")){
        p <- add_trace_safe(p, cmast_sal, "DateTime", "Salinity", "Salinity CMAST")
        p <- add_ysi_safe(p, y, "DateTime", "Salinity", "Salinity CMAST")
      }
    }
    
    if("Stump Sound" %in% farms){
      y <- ysi %>% filter(Farm == "Stump Sound")
      
      if(show_param("Temperature")){
        p <- add_trace_safe(p, stump_do, "DateTime", "Temp_C", "Temp Stump")
        p <- add_ysi_safe(p, y, "DateTime", "Temp_C", "Temp Stump")
      }
      if(show_param("Dissolved Oxygen")){
        p <- add_trace_safe(p, stump_do, "DateTime", "DO", "DO Stump")
        p <- add_ysi_safe(p, y, "DateTime", "DO", "DO Stump")
      }
      if(show_param("pH")){
        p <- add_trace_safe(p, stump_ph, "DateTime", "pH", "pH Stump")
        p <- add_ysi_safe(p, y, "DateTime", "pH", "pH Stump")
      }
      if(show_param("Salinity")){
        p <- add_trace_safe(p, stump_sal, "DateTime", "Salinity", "Salinity Stump")
        p <- add_ysi_safe(p, y, "DateTime", "Salinity", "Salinity Stump")
      }
    }
    
    if("Ward Creek" %in% farms){
      y <- ysi %>% filter(Farm == "Ward Creek")
      
      if(show_param("Temperature")){
        p <- add_trace_safe(p, ward_do, "DateTime", "Temp_C", "Temp Ward Creek")
        p <- add_ysi_safe(p, y, "DateTime", "Temp_C", "Temp Ward Creek")
      }
      if(show_param("Dissolved Oxygen")){
        p <- add_trace_safe(p, ward_do, "DateTime", "DO", "DO Ward Creek")
        p <- add_ysi_safe(p, y, "DateTime", "DO", "DO Ward Creek")
      }
      if(show_param("pH")){
        p <- add_trace_safe(p, ward_ph, "DateTime", "pH", "pH Ward Creek")
        p <- add_ysi_safe(p, y, "DateTime", "pH", "pH Ward Creek")
      }
      if(show_param("Salinity")){
        p <- add_trace_safe(p, ward_sal, "DateTime", "Salinity", "Salinity Ward Creek")
        p <- add_ysi_safe(p, y, "DateTime", "Salinity", "Salinity Ward Creek")
      }
    }
    
    if("DUML" %in% farms){
      y <- ysi %>% filter(Farm == "DUML")
      
      if(show_param("Temperature")){
        p <- add_trace_safe(p, duml_do, "DateTime", "Temp_C", "Temp DUML")
        p <- add_ysi_safe(p, y, "DateTime", "Temp_C", "Temp DUML")
      }
      if(show_param("Dissolved Oxygen")){
        p <- add_trace_safe(p, duml_do, "DateTime", "DO", "DO DUML")
        p <- add_ysi_safe(p, y, "DateTime", "DO", "DO DUML")
      }
      if(show_param("pH")){
        p <- add_trace_safe(p, duml_ph, "DateTime", "pH", "pH DUML")
        p <- add_ysi_safe(p, y, "DateTime", "pH", "pH DUML")
      }
      if(show_param("Salinity")){
        p <- add_trace_safe(p, duml_sal, "DateTime", "Salinity", "Salinity DUML")
        p <- add_ysi_safe(p, y, "DateTime", "Salinity", "Salinity DUML")
      }
    }
    
    if("Nelson Bay" %in% farms){
      y <- ysi %>% filter(Farm == "Nelson Bay")
      
      if(show_param("Temperature")){
        p <- add_trace_safe(p, nelson_do, "DateTime", "Temp_C", "Temp Nelson")
        p <- add_ysi_safe(p, y, "DateTime", "Temp_C", "Temp Nelson")
      }
      if(show_param("Dissolved Oxygen")){
        p <- add_trace_safe(p, nelson_do, "DateTime", "DO", "DO Nelson")
        p <- add_ysi_safe(p, y, "DateTime", "DO", "DO Nelson")
      }
      if(show_param("pH")){
        p <- add_trace_safe(p, nelson_ph, "DateTime", "pH", "pH Nelson")
        p <- add_ysi_safe(p, y, "DateTime", "pH", "pH Nelson")
      }
      if(show_param("Salinity")){
        p <- add_trace_safe(p, nelson_sal, "DateTime", "Salinity", "Salinity Nelson")
        p <- add_ysi_safe(p, y, "DateTime", "Salinity", "Salinity Nelson")
      }
    }
    
    p %>% layout(
      title = input$custom_title,
      xaxis = list(type = "date", title = "DateTime"),
      yaxis = list(title = "Value"),
      legend = list(title = list(text = "Parameter at Farm"))
    )
  })
}

shinyApp(ui, server)
