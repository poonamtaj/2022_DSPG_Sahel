#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# "C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/yearData2.csv"
library(shiny)
library(leaflet)
library(readxl)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(directlabels)
library(ggrepel)
library(plotly)


annualPrecip <- read_csv("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/yearData1.csv")

ui <- fluidPage(
  titlePanel("Precipitation Graph"),
    mainPanel(strong("Annual Precipitation"),
              plotlyOutput("plot1")
              
    )    
  )       


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    
    
    annualPrecip %>%
      ggplot(aes(x = Year, y = Precipitation, color = Region)) +
      geom_line()+ 
      scale_color_viridis_d(option = "H") +
      labs(title = "Annual Precipitation", 
           color =  "Region", x = "Year", 
           y = "Total Precipitation (mm)") + 
      theme_classic() +
      plotly()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
