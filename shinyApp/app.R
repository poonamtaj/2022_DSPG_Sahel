#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(rgdal)
library(stringr)
library(shinyjs)
library(dplyr)
library(sf)
library(gpclib)
library(maptools)
library(shinydashboard)
library(ggpolypath)
library(ggplot2)
library(plotly)
library(ggrepel)
library(hrbrthemes)
library(rmapshaper)
library(magrittr)
ui <- navbarPage(title = "DSPG 2022",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 ## Tab Overview -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   br(""),
                                   h1(strong("Sensing Drought in the Sahel for household resilience")),
                                   h4("Data Science for the Public Good Program"),
                                   h4("Virginia Tech"),
                                   h4("Department of Agricultural and Applied Economics")
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Overview"), align = "center")),
                                   column(4,
                                          h2(strong("Introduction to Niger"), align = "center")),
                                   column(4,
                                          h2(strong("Recent History"), align = "center"),
                                   )
                          )
                 ),
                 navbarMenu("Data & Methodology", 
                            tabPanel("Data", 
                                     fluidPage(
                                       h3(strong("Description of the DATA")),
                                       withMathJax())),
                            tabPanel("Methodology", 
                                     fluidPage(
                                       box(
                                         withMathJax(),
                                         title = h3(strong("Methodology")),
                                         width = 12)))),
                 tabPanel("Drought Index",
                          fluidPage(
                            h3(strong("NDVI , Precipitation")),
                            withMathJax())),
                 tabPanel("Welfare Index",
                          fluidPage(
                            h3(strong("LSMS , Food Insecurity")),
                            withMathJax())),
                 tabPanel("Takeaways",
                          fluidPage(
                            h3(strong("Takeaways")),
                            withMathJax())),
                 tabPanel("References",
                          fluidPage(
                            h3(strong("References")),
                            withMathJax())),
                 tabPanel("Our Team",
                          fluidPage(
                            h3(strong("Team")),
                            withMathJax()))
)
# Define server logic required to draw a histogram
server <- function(input, output) {
}
# Run the application 
shinyApp(ui = ui, server = server)