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
  sidebarLayout(
    sidebarPanel(p("Demo")),
    mainPanel(strong("Annual Precipitation"),
              plotlyOutput("plot1")
              
    )    
  )       
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    
    
    annualPrecip %>%
      ggplot(aes(x = year, y = total_precip_annual_admin1, group = as.factor(admin1Name), color = as.factor(admin1Name))) +
      geom_line()+
      theme(axis.text.x = element_text(angle = 315)) +
      scale_colour_discrete(guide = 'none') +
      scale_x_discrete(expand=c(0, 1)) +
      geom_dl(aes(label = admin1Name), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
      scale_color_viridis_d(option = "H") +
      labs(title = "Annual Precipitation", color =  "Region") +
      xlab("Time(Year)") + 
      plotly()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
