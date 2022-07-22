#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######### DSPG SAHEL Project -----------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

#####------------------------------------------------------------- data -------------------------------------------------------------------#####

# Setting working directory and reading data
# TODO: coauthors -- change the file here for your path

mydata <-read_excel("./data/Données EVIAM 15 17 insécurite alimentaire.xlsx")
Niger_level2 <- st_read(
  "./data/wb_niger_admin2_shapefile/niger_admin2.shp")

mydt_ndvi_md <-read_csv("./data/admin2ndvi_md.csv")
#nigerMapAdmin2 <- st_read("./data/shapefiles/niger_admin2.shp")

# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerShpMerged_admin2_md = full_join(Niger_level2, 
                                     mydt_ndvi_md,
                                     by=c("admin2Pcod" = "admin2Pcod"))
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
  map_out<-ggplot(data = data_merged_shp,size = 1, color = "NA", aes(fill=val, name=admin2Name,
                                                                     text=paste("Département: ",admin2Name,"<br>","% Population: ",val)))+
    geom_sf() +
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



#####------------------------------------------------------ ui for shiny app --------------------------------------------------------#######

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
                                                   h2(strong("Project Overview"), align = "center"),
                                                   p("Decades of economic research has shown that without effective social protection, 
                                          extreme weather in sub-Saharan Africa has resulted in people resorting to harmful coping 
                                          strategies, including removing children from school, skipping meals, and selling off assets. 
                                          These coping strategies further perpetuate the poverty cycle, preventing future generations from
                                          achieving a higher quality of life. Weather trends in the past few decades have also shown that 
                                          the number of people exposed to drought in the Sahel has increased."),
                                                   p("The Data Science for the Public 
                                          Good (DSPG) project seeks to help break the link between drought and distress, by identifying where 
                                          droughts have created greatest harms. The research team is supporting stakeholder The World Bank’s 
                                          Sahel Adaptive Protection Program, who is looking to use readily available data on drought conditions 
                                          to determine the most efficient use of funding to aid those at greatest risk at a more rapid pace than 
                                          before. Through this analysis, The World Bank seeks to take a more proactive approach to social protection 
                                          by developing targeting mechanisms, identifying the most vulnerable and quickly scaling up programs when needed.
                                          The DSPG team will be using publicly available data on historical drought indicators, including precipitation and 
                                          biomass indices from remote sensing data, and comparing them with historical welfare measures, focusing on the country 
                                            of Niger. ")), 
                                            column(4,
                                                   h2(strong("Introduction to The Sahel"), align = "center"),
                                                   p("The Sahel is a band of territory in Africa that stretches the length of the continent, from the Atlantic coast of Senegal
                                            and Mauritania to the Red Sea coast of Eritrea, acting as a buffer zone between the Sahara Desert in the North and the
                                            Savannas in the South. The country of Niger is located in the middle of the Sahel, neighboring Mali, Chad, and Nigeria.
                                            Niger is split into three distinct zones, a desert zone in the North, an intermediate zone in the center, and a cultivated
                                            zone in the South where a greater part of the 22-million person population is concentrated. As over 80% of the land is 
                                            covered by uninhabitable desert, 94% of the population lives on just 35% of the land. Niger does not have many large cities,
                                            only roughly 19% of the population lives in urban areas."),
                                                   img(src = "niger.png", class = "topimage", width = "60%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                                   p("Niger was incorporated into French West Africa in 1896, becoming a colony in 1922 following frequent rebellions. The country
                                            withdrew from the French Community in 1960, proclaiming independence. Hamani Diori led a single-party dictatorship until he was
                                            overthrown in a coup in 1974. Niger has long struggled to maintain peace within its changing government, with minimal resources
                                            and insufficient funding. Niger’s present republic is responsible for regulating its 7 regions, further divided into 36 departments
                                            and 265 communes. "),
                                                   p("The economy in Niger centers on subsistence (rain-fed) crops, livestock, and uranium deposits. Agriculture contributes 40% of the
                                            GDP and provides livelihoods for over 80% of the population. Despite the reliance on the agriculture industry, agricultural production
                                            is constrained to the short rainy season, with limited access to irrigation. Niger is highly exposed to climatic shocks such as drought;
                                            the frequency and severity of these shocks is expected to increase due to climate change. Considering the dependence on agriculture and
                                            the frequency of drought, Niger is a high poverty and food insecure country with low living standards, leaving the country as one of the
                                            poorest and least developed in the world, it ranked last in the UNDP 2019 Human Development Index.")),
                                            column(4,
                                                   h2(strong("Recent History"), align = "center"),
                                                   p("Under the constitution established in 2010, Niger is a republic with a president acting as head of state serving five year terms, elected
                                            by popular vote. Education in Niger is free, but is severely underutilized due to high rates of poverty, leaving the country with a literacy
                                            rate of just 19%. The healthcare system is also inadequate, due to a lack of both financial and human capital. Niger is one of the poorest
                                            countries in the world, with a GDP per capita of 895 USD in 2015. The desert terrain and frequent drought faced by the people of Niger create
                                            significant obstacles in alleviating poverty. Weather shocks were found to decrease household consumption by 31-48%, create large movement in
                                            food prices, and have a negative effect on technology adoption."),
                                                   p("Niger is a member of multiple international organizations, including the United Nations, International Monetary Fund, World Bank, etcetera; and
                                            receives substantial humanitarian assistance. In 2011, the Government of Niger began a national safety net system organized in the Office of the
                                            Prime Minister, with the objective of developing multi-year safety nets and shock-responsive interventions. This system is supported by the Sahel
                                            Adaptive Social Protection Program (SASPP), launched by the World Bank in 2014 with the goal of developing adaptive social protection systems in
                                            six Sahel countries to help poor and vulnerable households become more resilient to the effects of climate change. Presently, the program is entering
                                            a new phase focused on strengthening these social protection systems, expanding the reach of existing shock response cash transfer programs. The cash
                                            transfer program involves providing small, regular, and unconditional transfers targeted to poor households, selected based on a proxy-means test. This
                                            design rationales that it would help households better prepare themselves against future shocks; creating a more proactive system rather than only
                                            responding
                                            to existing shocks."),
                                            )
                                   )
                 ),
                 
                 ## Tab Data & Methodology -----------------------------------------------------------
                 tabPanel("Data",
                          fluidPage(
                            h1(strong("Description of Data Sources")),
                            h3(strong("Precipitation")),
                            p("Estimating rainfall variations over space and time is a key tool of predicting drought and conducting environmental monitoring. Using historical context allows
                              researchers to evaluate the severity of rainfall deficits.  Climate Hazards Group InfraRed Precipitation with Station (CHIRPS) data is a quasi-global rainfall dataset
                              spanning 50°S-50°N and all longitudes ranging from 1981 to present, showing gridded rainfall time series for trend analysis and seasonal drought monitoring."),
                            img(src = "precipitation.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                            h3(strong("Normalized Difference Vegetation Index")),
                            p("The Normalized Difference Vegetative Index (NDVI) dataset is unique in that it bridges the gap between satellite imagery and internal vegetative processes. Satellite
                              sensors measure wavelengths of light absorbed and reflected by green plants; certain pigments in plant leaves strongly absorb wavelengths of visible (red) light. The leaves
                              themselves reflect wavelengths of near-infrared light, invisible to the human eye."),
                            img(src = "ndviequation.png", class = "topimage", width = "15%", style = "display: block; margin-left: auto; margin-right: auto;"),
                            p("NDVI values range from +1.0 to -1.0; areas with little vegetation show very low values
                              (for example, 0.1 or less), whereas sparse vegetation such as shrubs and grassland may result in moderate values (approximately 0.2 to 0.5), and high NDVI values 
                              (approximately 0.6 to 0.9) correspond to dense vegetation found in tropical forests. NDVI is strongly correlated to the overall health of plant foliage, making the dataset
                              ideal for researchers to create images and products to roughly measure vegetation type, amount, and condition on land surfaces across the world. NDVI values can be aggregated
                              over time to establish “normal” growing conditions in a region for a given time of year. Further analysis can then characterize the health of vegetation in that place relative
                              to the norm. When analyzed through time, NDVI can reveal where vegetation is thriving and where it is under stress, as well as changes in vegetation due to human activities such
                              as deforestation, natural disturbances such as wild fires, or changes in plants' phenological stage."),
                            img(src = "ndvi.png", class = "topimage", width = "15%", style = "display: block; margin-left: auto; margin-right: auto;"),
                            h3(strong("Living Standards Measurement Study")),
                            p("The Living Standards Measurement Study (LSMS) is a survey program conducted by the World Bank, with the goal of strengthening household survey systems and improving the quality
                              of microdata. LSMS data allows a higher degree of accuracy in research and policy development, collecting measures of household and individual wellbeing. LSMS data from Niger has
                              been utilized in this research to study expenditure, by category: food expenditure, non-food expenditure, and total expenditure.  ")),
                          img(src = "lsms.png", class = "topimage", width = "25%", style = "display: block; margin-left: auto; margin-right: auto;")
                          ),
                 
                 ## Tab Drought Index ---------------------------------------------------------------
                 tabPanel("Drought Index",
                          fluidPage(
                            h3(strong("NDVI , Precipitation")),
                            withMathJax()),
                          column(4, 
                                 h4(strong("Description")),
                                 p("Overall, there is relatively little variation across this time span, 
                                   indicating that there was little change in vegetation. The reason why the maps may all look the same is that there is little growth and 
                                   change over time due to unpredictable rainfall and frequent drought. 
                                   It seems that around 2013 and 2014 the lower regions had an increased vegetation level, 
                                   which is reflected by increased rainfall during the same years.")
                          ),
                          column(8,
                                 h4(strong("NDVI Maps")),
                                 leafletOutput("my_leaf", height = "500px")),
                 ),
                
                 ## Tab Welfare Index --------------------------------------------------------------
                  tabPanel("Welfare Index",
                          fluidPage(
                            h3(strong("Welfare Index")),
                            withMathJax()),
                          tabsetPanel(
                            tabPanel("Food Insecurity",
                                     column(4,
                                            h4(strong("Description")),
                                            p("Food insecurity is defined as when people lack access to 
                                                enough wholesome food and don't get the nutrition they require
                                                to grow normally and live active, healthy lives. Food shortages, 
                                                low purchasing power, improper food distribution, or improper food 
                                                utilization at the household level could all be contributing 
                                                factors to this predicament."),
                                            p("These maps show the food insecurity data at the Department level(67 departments) 
                                                which comes from the EVIAM surveys (joint survey on vulnerability to household 
                                                food insecurity in Niger ) for the years 2015 and 2017. There are three main 
                                                variables to understand magnitude of the food insecurity: population at risk , 
                                                population at moderate risk , and population at severe risk."),
                                            p("If we focus on the graphs , 
                                                it is observed that food insecurity is concentrated in southwest regions 
                                                in both 2015 and 2017.If we look at the differences between 2105 and 2017,more share of population 
                                                being food insecure
                                                in 2017 as compared to 2015. Hence, over the period from 2015 to 2017, there is an
                                                upward trend in the percentages of populations with severe,
                                                moderate and at risk food insecurity."),
                                            p("To view the share of the food insecure population at different departments ,
                                                hover over the respective department in the map.")
                                     ),
                                     column(8,
                                            h4(strong("Maps"),align="center"),
                                            plotlyOutput("food_insecurity_out1",height="500px"),
                                            plotlyOutput("food_insecurity_out2",height="500px"),
                                            plotlyOutput("food_insecurity_out3",height="500px"),
                                     )),
                            tabPanel("LSMS",
                                     column(4,
                                            h4(strong("Description")),
                                            p("Description of the LSMS data"),
                                     ),
                                     column(8,
                                            h4(strong("Maps"),align="center"),
                                            radioButtons("food_expenditure", "Select Administrative levels:", width="100%", choices = c(
                                              "Département (Admin 2)"="Admin2","Commune (Admin 3)"="Admin3")),
                                            plotOutput("food_expenditure_out"),
                                     ))
                            
                          )),
                 
                 ## Tab Takeaways ---------------------------------------------------------------
                 tabPanel("Takeaways",
                          fluidPage(
                            h3(strong("Takeaways")),
                            withMathJax())),
                 
                 ## Tab References --------------------------------------------------------------
                 tabPanel("References", value = "references",
                          column(3),
                          column(6, 
                                 h1(strong("References"), align = "center"),
                                 p("Brown, J. (n.d.). NDVI, the foundation for Remote Sensing Phenology active. NDVI, the Foundation for Remote Sensing Phenology | U.S. Geological Survey. Retrieved July 15, 2022,
                                   from https://www.usgs.gov/special-topics/remote-sensing-phenology/science/ndvi-foundation-remote-sensing-phenology "),
                                 p("Central Intelligence Agency. (2022, July 1). Niger - The World Factbook. Central Intelligence Agency. Retrieved July 13, 2022, from https://www.cia.gov/the-world-factbook/countries/niger/#geography "),
                                 p("CHIRPS: Rainfall Estimates from Rain Gauge and Satellite Observations. Climate hazards center - UC santa barbara. (n.d.). Retrieved July 15, 2022, from https://chc.ucsb.edu/data/chirps "),
                                 p("Laya, D. and Fuglestad, . Finn (2021, April 29). Niger. Encyclopedia Britannica. https://www.britannica.com/place/Niger"),
                                 p("Nicole, H. (2019, December 6). The organizations and steps used in reducing poverty in Niger. The Borgen Project. Retrieved July 13, 2022, from https://borgenproject.org/reducing-poverty-in-niger/ "),
                                 p("Premand, P., & Stoeffler, Q. (2020, November). Do cash transfers foster resilience? - world bank. Do Cash Transfers Foster Resilience? . Retrieved July 13, 2022, from https://documents1.worldbank.org/curated/en/281821605039063267/pdf/Do-Cash-Transfers-Foster-Resilience-Evidence-from-Rural-Niger.pdf "),
                                 p("Sahel adaptive Social Protection Program (ASPP). World Bank. (2020, June 1). Retrieved July 13, 2022, from https://www.worldbank.org/en/programs/sahel-adaptive-social-protection-program-trust-fund "),
                                 p("U.S. Department of State. (2021, June 16). U.S. relations with Niger - United States Department of State. U.S. Department of State. Retrieved July 13, 2022, from https://www.state.gov/u-s-relations-with-niger/ ")
                          )),
                 
                 ## Tab Team ---------------------------------------------------------------------
                 tabPanel("Our Team",
                          fluidPage(
                            fluidRow
                            (style = "margin-left: 100px; margin-right: 100px;",
                              h1(strong("Our Team"), align = "center"),
                              br(),
                              h4(strong("Virginia Tech Data Science for the Public Good"), align = "center"),
                              p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics'), 
                                "In its third year, the program engages students from across the country to collaborate on projects that address current social issues both locally and globally. DSPG young 
                                       scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged
                                       to improve quality of life and inform public policy. For more information on program highlights, application information, and our annual symposium, please visit", 
                                a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official Data Science for the Public Good website.', target = "_blank")),
                              p("", style = "padding-top:10px;")
                            ),
                            fluidRow(
                              
                              column(4, align = "center",
                                     h4(strong("Graduate Fellow")), tags$br(),
                                     tags$br(), img(src = "fellow-poonam.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="37%", height="37%"),
                                     tags$br(), p(a(href = 'https://www.linkedin.com/in/poonam-tajanpure-72a64523b/', 'Poonam Tajanpure', target = '_blank'), "(Virginia Tech, Agricultural Engineering PhD)")
                              ),
                              column(4, align = "center",
                                     h4(strong("Undergraduate Interns")), tags$br(),
                                     tags$br(), img(src = "team-riley.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="37%", height="37%"),
                                     tags$br(), p(a(href = 'https://www.linkedin.com/in/riley-rudd-1166a3192/', 'Riley Rudd', target = '_blank'), "(Virginia Tech, Economics)"),
                                     tags$br(), img(src = "team-milind.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="37%", height="37%"),
                                     tags$br(), p(a(href = 'https://www.linkedin.com/in/milindgupta27/', 'Milind Gupta', target = '_blank'), "(Virginia Tech, Computer Engineering)"),
                                     tags$br(), img(src = "team-catherine.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="37%", height="37%"),
                                     tags$br(), p(a(href = 'https://www.linkedin.com/in/catherineback2/', 'Catherine Back', target = '_blank'), "(UCSD, Data Science)")
                              ),
                              column(4, align = "center",
                                     h4(strong("Faculty Advisors")), tags$br(),
                                     img(src = "faculty-elinor.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", height = "37%", width = "37%"),  tags$br(),
                                     p(a(href = 'https://aaec.vt.edu/people/faculty/benami-elinor.html', 'Dr. Elinor Benami', target = '_blank'), "(Virginia Tech, Department of Agricultural and Applied Economics)"),  tags$br(),
                                     img(src = "faculty-susan.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", height = "37%", width = "37%"),  tags$br(),
                                     p(a(href = 'https://aaec.vt.edu/people/faculty/chen-susan.html', 'Dr. Susan Chen', target = '_blank'), "(Virginia Tech, Department of Agricultural and Applied Economics)"),  tags$br()
                              ),
                            )))            
                 
)


#####-------------------------------------- Define server  -------------------------------------#####

server <- function(input, output) {
  output$food_insecurity_out1<-renderPlotly(ggplotly({map_at_risk},tooltip="text"))
  output$food_insecurity_out2<-renderPlotly(ggplotly({map_at_moderate_risk},tooltip = "text"))
  output$food_insecurity_out3<-renderPlotly(ggplotly({map_at_severe_risk},tooltip = "text"))
  
  food_expenditure<-reactive({
    input$food_expenditure
  })
  output$food_expenditure_out<-renderImage({
    if(food_expenditure()=="Admin2"){
      list(src='www/Admin2_Foodexpenditure.png', align = "center",width=800,height=500)
    }
    else if (food_expenditure()=="Admin3"){
      list(src='www/Admin3_Foodexpenditure.png', align = "center",width=800,height=500)
    }
  })
  
  output$my_leaf <- renderLeaflet({
    mypal <- colorNumeric(
      palette = "viridis",
      domain = nigerShpMerged_admin2_md$peak_ndvi)
    
    leaflet(nigerShpMerged_admin2_md) %>%
      addTiles() %>%  
      addPolygons(color = ~mypal(peak_ndvi), weight = 1, smoothFactor = 0.5, 
                  label = paste("Department -", nigerShpMerged_admin2_md$admin2Name.x,":", "Peak NDVI", round(nigerShpMerged_admin2_md$peak_ndvi, digits = 3)),
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE)) %>% 
      addLegend(pal = mypal,position = "bottomright",values = nigerShpMerged_admin2_md$peak_ndvi,
                opacity = .6,title = paste("Peak NDVI")) 
    
    
    
    
    
  })
}

#####---------------------------------- Run the application ----------------------------------#####
shinyApp(ui = ui, server = server)