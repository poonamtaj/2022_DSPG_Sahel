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

remove_chart_clutter <- 
  theme(    
    panel.grid.major = element_blank(),      # Remove panel grid lines
    panel.grid.minor = element_blank(),      # Remove panel grid lines
    panel.background = element_blank(),      # Remove panel background
    axis.line = element_line(colour = "grey"),       # Add axis line
    axis.title.y = element_text(angle = 0, vjust = 0.5),      # Rotate y axis so don't have to crank head
    legend.position="bottom"
  ) 

# Setting working directory and reading data
# TODO: coauthors -- change the file here for your path

mydata <-read_excel("./data/Données EVIAM 15 17 insécurite alimentaire.xlsx")
Niger_level2 <- st_read(
  "./data/wb_niger_admin2_shapefile/niger_admin2.shp")


# adjusting the discrepancy in some names due to French accent

mydata$departement_name <- sub("Aguie", "Aguié", mydata$departement_name)
mydata$departement_name <- sub("Bankilare", "Bankilaré", mydata$departement_name)
mydata$departement_name <- sub("Filingue", "Filingué", mydata$departement_name)
mydata$departement_name <- sub("Gotheye", "Gothèye", mydata$departement_name)
mydata$departement_name <- sub("Goure", "Gouré", mydata$departement_name)
mydata$departement_name <- sub("Guidan-Roumdji", "Guidan Roumdji", mydata$departement_name)
mydata$departement_name <- sub("Illela", "Illéla", mydata$departement_name)
mydata$departement_name <- sub("Kantche", "Kantché", mydata$departement_name)
mydata$departement_name <- sub("Maine-Soroa", "Maïné Soroa", mydata$departement_name)
mydata$departement_name <- sub("Tera", "Téra", mydata$departement_name)
mydata$departement_name <- sub("Tibiri .Doutchi.", "Tibiri", mydata$departement_name)
mydata$departement_name <- sub("Tillaberi", "Tillabéri", mydata$departement_name)


#Converting to a function
make_maps<-function(var1,var2,val,title) {
  #Converting data from wide to long to use facet wrap
  
  data_sub<-mydata%>%select(departement_name,var1,var2)%>% rename("2015" = var1,"2017" = var2)
  data_sub_long<-gather(data_sub,key="year", value=val, 2:3)
  
  
  # Make Maps side by side
  data_merged_shp=merge(x=Niger_level2,y=data_sub_long,by.x="admin2Name",by.y="departement_name",all=TRUE)
  map_out<-ggplot() + 
    geom_sf(data = data_merged_shp, size = 1, color = "NA", aes(fill=val)) + 
    ggtitle(title) + scale_fill_viridis_c()+
    coord_sf()+
    labs(fill = "Percent of Population")+theme_classic()+remove_chart_clutter+
    facet_wrap(~year)
  return(map_out)  
}

#Atrisk maps
map_at_risk <- make_maps(var1="arisque_part_15",var2="arisque_part_17",val="ATRISK",title="Population at  Risk of Food Insecurity by department (admin2)")

#Moderate Risk maps 
map_at_moderate_risk <- make_maps(var1="moderate_part_15",var2="moderate_part_17",val="MODERATERISK",title="Population at Moderate Risk of Food Insecurity by department (admin2)")

#Severe Risk maps
map_at_severe_risk <- make_maps(var1="severe_part_15",var2="severe_part_17",val="SEVERERISK",title="Population at Severe Risk of Food Insecurity by department (admin2)")

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
                            h3(strong("Welfare Index")),
                            withMathJax()),
                          tabsetPanel(
                            tabPanel("Food Insecurity",
                                     column(4,
                                            h4(strong("Description")),
                                            p("Decription of food insecurity."),
                                            p("As background information suggests, it is the drought prone area ,
                                                hence food insecurity is also prevalent in Niger.Food insecurity is the state of 
                                                being without reliable access to a sufficient quantity of affordable, nutritious food.
                                                World Bank data on food insecurity gives us the three main variables to understand magnitude of the food insecurity. 
                                                population at risk, population at moderate risk , population at severe risk for 
                                                two years 2015 and 2017."),
                                            p("We can observe that how food insecurity is changing from 2015 to 2017 and it appears to be 
                                                 concentrated in the southwest region ( which is about 40-50% of the population).")
                                     ),
                                     column(8,
                                            h4(strong("Maps"),align="center"),
                                            plotOutput("food_insecurity_out1",height="500px"),
                                            plotOutput("food_insecurity_out2",height="500px"),
                                            plotOutput("food_insecurity_out3",height="500px"),
                                     )),
                            tabPanel("LSMS",
                                     column(4,
                                            h4(strong("Description")),
                                            p("Description of the LSMS data"),
                                     ),
                                     column(8,
                                            h4(strong("Maps")),
                                     ))
                            
                          )),
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
  
  output$food_insecurity_out1<-renderPlot({map_at_risk})
  output$food_insecurity_out2<-renderPlot({map_at_moderate_risk})
  output$food_insecurity_out3<-renderPlot({map_at_severe_risk})
}
# Run the application 
shinyApp(ui = ui, server = server)