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
library(rsconnect)

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

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 1)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY------
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2022 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic'); 
           }
           "
# Setting working directory and reading data
# TODO: coauthors -- change the file here for your path
### precipitation data
annualPrecip <- read_csv("./data/yearData1_precip.csv")
annualPrecip_md <- read_csv("./data/yearAdmin1_md.csv")

### NDVI data
#mydt_ndvi_md <-read_csv("./data/admin2ndvi_md.csv")
annualndvi <- read_csv("./data/ndviyearData_admin1.csv")
annualndvi_md <- read_csv("./data/ndviyearAdmin1_md.csv")
### food insecurity data
mydata <- read_excel("./data/food_insecurity_15_17.xlsx")

### shapefile
Niger_level2 <- st_read("./data/wb_niger_admin2_shapefile/niger_admin2.shp")


########## Food Insecurity 2015, 2017
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

ui <- navbarPage(title = "SAHEL DSPG 2022",
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
                 tabPanel("Data and Methodology",
                          fluidPage(
                            h1(strong("Data Sources")),
                            h3(strong("Precipitation")),
                            fluidRow(
                              column(4,
                                     p("Estimating rainfall variations over space and time is a key tool of predicting drought and conducting environmental monitoring. Using historical context allows
                                   researchers to evaluate the severity of rainfall deficits.  Climate Hazards Group InfraRed Precipitation with Station (CHIRPS) data is a quasi-global rainfall dataset
                                   spanning 50°S-50°N and all longitudes ranging from 1981 to present, showing gridded rainfall time series for trend analysis and seasonal drought monitoring."),
                              ),
                              column(8,
                                     img(src = "precipitation.png", class = "topimage", width = "45%", style = "display: block; margin-left: auto; margin-right: auto;"),
                              )),
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
                            h3(strong("Food Insecurity")),
                            p("In this research we will use the indicator of current economic vulnerability as a proxy for the food insecurity. The current economic vulnerability represents the percentage of
                              total household expenditures devoted to food over the reference period and is calculated as a ratio of food expenditure to the total expenditure. The identification of food insecure
                              people in rural areas is based on the analysis of five indicators: the duration of available food stocks, food consumption, the number of TLUs (Tropical Livestock Units), the share
                              of food expenditure in total expenditure and coping strategies. These indicators reflect the three pillars of food security, namely availability, accessibility and utilization."),
                            p("Food Insecurity data at the admin 2 level comes from the EVIAM surveys, a joint survey on vulnerability to household food insecurity in Niger for the years 2015 and 2017. In total,
                              the 2015 survey covered a sample of 21,668 households while in 2017, the survey covered a sample of 18,366 households. The approach adopted in these surveys consisted of collecting
                              information at the village level and also at the level of rural households."),
                            h3(strong("Living Standards Measurement Study")),
                            fluidRow(
                              column(4,
                                     p("The Living Standards Measurement Study (LSMS) is a survey program conducted by the World Bank, with the goal of strengthening household survey systems and improving the quality
                              of microdata. LSMS data allows a higher degree of accuracy in research and policy development, collecting measures of household and individual wellbeing. LSMS data from Niger has
                              been utilized in this research to study expenditure, by category: food expenditure, non-food expenditure, and total expenditure. 3859 households were surveyed in 2011, 3627 households were surveyed in 2014 and 
                                       6024 households were surveyed in 2018.When we aggregated at median level, we got 50 observations in 2011 as well as 2014 and 61 observations in 2018 at Department level (admin 2) while 96 observations in 2011 and 2014 and
                                       232 observations in 2018 at Commune level (admin 3)."),
                              ),
                              column(8,
                                     img(src = "lsms.png", class = "topimage", width = "45%", style = "display: block; margin-left: auto; margin-right: auto;")),
                            ),
                            h1(strong("Methodology")),
                            h3(strong('Z-Score')),
                            p("The DSPG team used z-scores to translate historical weather to “anomalies” from normal (xI). Z-scores (Zit) quantify how anomalous a given annual precipitation
                              amount or NDVI value is by comparing that value (xI) to the mean (x̄) of those values in a prior period (here we used 1981-2010), and dividing by the standard deviation
                              (si) across that same baseline period."),
                            h3(strong('Correlation')),
                            p("Analysis then moved to determining the relationship between annual weather anomalies and aggregate welfare using the Person R correlation
                              coefficient, which measures the strength of the linear association between the variables.")),
                 ),
                 ## Tab Drought Index ---------------------------------------------------------------
                 tabPanel("Drought Index",
                          fluidPage(
                            h3(strong("Drought Indices")),
                            withMathJax()),
                          tabsetPanel(
                 tabPanel("Precipitation",
                          fluidRow(
                            column(
                              4,
                              p(h4(strong("Description"))),
                              p("The total annual precipitation by region line charts
                                shows that there are some regions that receive more
                                precipitation than others. We see a couple peaks in
                                precipitation along different years; specifically in
                                1994, 1998, and 2020. These trends stay similar when
                                looking at data aggregated using mean and aggregated
                                using median. "), 
                            ),
                            column(
                              7,
                              p(h4(strong("Total Annual Precipitation by Region"))),
                              plotlyOutput("plot1"),
                              plotlyOutput("plot2")
                            ),  
                          ), 
                          br(),
                          
                          fluidRow(
                            column(
                              4,
                              p(h4(strong("Description"))),
                              p("The department level shows differences in the z-score between the mean
                                versus median aggregated data. In addition, there are high z-scores in
                                the northern regions of Niger, which are mostly desert. These scores could
                                be due to data skewing from flood year outliers as the baseline data had
                                little rain."), 
                              p("The commune level shows differences in the z-score score between the data
                                aggregated using mean and aggregated using median. While there are high z-scores
                                in the northern regions when using mean, using median shows that there is a region
                                in the middle of Niger that receives more rainfall compared to the baseline data. "),
                            ),
                            column(
                              7,
                              p(h4(strong("Annual Z-Score Precipitation Mean and Median"))),
                              radioButtons("precipitation", "Select Administrative levels:", width="100%", choices = c(
                                "Département (Admin 2)"="Admin2","Commune (Admin 3)"="Admin3")),
                              plotOutput("precipitation_out")
                            ),  
                          ),
                          br(),
                          
                          fluidRow(
                            column(
                              4,
                              p(h4(strong("Description"))),
                              p("We see slight differences in the seasonal precipitation data when looking at
                                mean versus median aggregated data. There are high z-scores in the northern
                                region, especially in the years 2015 and 2018. These results align with records
                                which indicate flooding in the year of 2018."), 
                              p("The commune level shows slight differences in the z-score when we look at mean
                              versus median aggregated data. While there are high z-scores in the northern regions
                              when using mean, using median we can see that there is the largest difference when
                              looking at 2015."),
                            ),
                            column(
                              7,
                              p(h4(strong("Seasonal Z-Score Precipitation Mean and Median"))),
                              radioButtons("seasonalPrecip", "Select Administrative levels:", width="100%", choices = c(
                                "Département (Admin 2)"="Admin2seasonal","Commune (Admin 3)"="Admin3seasonal")),
                              plotOutput("seasonalPrecip_out")
                            ),  
                          )
                          
                          
                 ),
                 
                 tabPanel("NDVI", 
                            
                            fluidRow(
                              column(4,
                                     p(h4(strong("Description"))),
                                     p("The annual peak NDVI by region line charts show that there are significant peaks
                                         during 1994 and 2004. The stark peak during 1994 is reflected in precipitation data,
                                         however the peak in 2004 is unsupported by precipitation. The trends across time are
                                         similar when aggregated using mean and median."),
                              ),
                              column(7,
                                     p(h4(strong("Peak Annual NDVI by Region"))),
                                     plotlyOutput("plot3"),
                                     plotlyOutput("plot4")
                              ),
                            ),
                          br(),
                          
                            fluidRow(
                              column(
                                4,
                                p(h4(strong("Description"))),
                                p("Annual Z-Score NDVI data is mapped to compare mean and median aggregation. 
                                As NDVI data is a comparison to historical measures, there is little change in
                                vegetation over time in the northern desert regions as shown in the maps. As such,
                                the variation is concentrated in the southern regions."),
                                p("In both administrative levels the median data shows more variation in regions
                                as it accounts for any outliers in the data, also reflected by the narrower scale
                                of z-score."),
                              ),
                              column(
                                7,
                                p(h4(strong("Annual Z-Score NDVI Mean and Median"))),
                                radioButtons("NDVI", "Select Administrative levels:", width="100%", choices = c(
                                  "Département (Admin 2)"="Admin2","Commune (Admin 3)"="Admin3")),
                                plotOutput("NDVI_out")
                              ),  
                            ),
                          br(),
                          br(),
                          
                            fluidRow(
                              column(
                                4,
                                p(h4(strong("Description"))),
                                p("Similar to annual data, variation is generally concentrated in the southern regions
                                  regardless of mean or median aggregation. There are higher z-scores in the southern
                                  region on the 2018 map, which aligns with records reporting flooding during 2018."), 
                                p("The commune level maps aggregated by mean include outliers in a few communes that
                                  skew the scale of z-score, washing out the maps. Median data, accounting for
                                  outliers, is more visually representative when mapped. Interestingly, z-scores
                                  were slightly lower in 2015 despite flooding recorded during that year."),
                              ),
                              column(
                                7,
                                p(h4(strong("Seasonal Z-Score NDVI Mean and Median"))),
                                radioButtons("seasonalNDVI", "Select Administrative levels:", width="100%", choices = c(
                                  "Département (Admin 2)"="Admin2seasonal","Commune (Admin 3)"="Admin3seasonal")),
                                plotOutput("seasonalNDVI_out")
                              ),  
                            )
                            
                            
                            
                 ))
                 ),
                 ## Tab Welfare Index --------------------------------------------------------------
                  tabPanel("Consumption",
                          fluidPage(
                            h3(strong("Consumption")),
                            withMathJax()),
                          tabsetPanel(
                            tabPanel("LSMS",
                                     column(4,
                                            h4(strong("Description")),
                                            p("The Living Standards Measurement Study (LSMS), the World Bank's premier household survey program, 
                                              aims to improve the quality of microdata and strengthen household survey systems in client countries
                                              in order to better inform development policies. It's main objective is to promote the creation and 
                                              adoption of new standards and methods for gathering household data in order to support evidence-based
                                              policymaking."),
                                            p("These maps show the food expenditure and total expenditure data at the Department level and Commune level
                                              which comes from the LSMS surveys for the years 2011, 2014, and 2018. We aggregated it by median because the
                                              median is less affected by outliers and skewed data than the mean, and is usually the preferred measure of central
                                              tendency when the distribution is not symmetrical. When we aggregated at median level, we got 50 observations in 2011 
                                              as well as 2014 and 61 observations in 2018 at Department level (admin 2) while 96 observations in 2011 and 2014 and 232 
                                              observations in 2018 at Commune level (admin 3)."),
                                            p("We analyzed that per capita food expenditures have been lowest in the southern region. It is lower in 2018 as
                                              compared to 2011 and 2014 at both admin levels. We also observed that total expenditures are lowest in southern
                                              regions and highest in northwest regions at both admin levels. "),
                                            align = "justify"),
                                     column(8,
                                            h4(strong("Food Expenditure")),
                                            radioButtons("food_expenditure", "Select Administrative levels:", width="100%", choices = c(
                                              "Département (Admin 2)"="Admin2","Commune (Admin 3)"="Admin3")),
                                            plotOutput("food_expenditure_out"),
                                            
                                            h4(strong("Total Expenditure")),
                                            radioButtons("total_expenditure", "Select Administrative levels:", width="100%", choices = c(
                                              "Département (Admin 2)"="total_Admin2","Commune (Admin 3)"="total_Admin3")),
                                            plotOutput("total_expenditure_out")
        
                                            
                                     )),
                                   
                            
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
                                            p("If we focus on the maps , 
                                                it is observed that food insecurity is concentrated in southwest regions 
                                                in both 2015 and 2017.If we look at the differences between 2105 and 2017,more share of population 
                                                being food insecure
                                                in 2017 as compared to 2015. Hence, over the period from 2015 to 2017, there is an
                                                upward trend in the percentages of populations with severe,
                                                moderate and at risk food insecurity."),
                                            p("To view the share of the food insecure population at different departments ,
                                                hover over the respective department in the map."), 
                                            align = "justify"),
                                     column(8,
                                            h4(strong("Maps"),align="center"),
                                            plotlyOutput("food_insecurity_out1",height="500px"),
                                            plotlyOutput("food_insecurity_out2",height="500px"),
                                            plotlyOutput("food_insecurity_out3",height="500px"),
                                     ))
                          )),

             
                 ## Tab Analysis -----------------------------------------------------------
                 tabPanel("Analysis",
                          fluidPage(
                            # h3(strong("Analysis")),
                            withMathJax()),
                          fluidRow(
                            column(12,
                              #align="justify",
                              h2(strong("Correlation Analysis at Department level")),
                              align="center")
                          ),
                          br(),
                
                          fluidRow( 
                          column(4,
                                 h4(strong("1. Precipitation & NDVI")), align="center",
                                 img(src = "corr_preip_ndvi_adm2_61.png", class = "topimage", width = "110%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                 ),
                          
                          column(4, 
                                 h4(strong("2. Precipitation & Share of Food Expenditure")), align="center",
                                 img(src = "corr_precipZ_fdecp_adm2.png", class = "topimage", width = "82%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                ),
                          column(4,
                                 h4(strong("3. NDVI & Share of Food Expenditure")), align="center",
                                 img(src = "corr_ndviZ_fdecp_adm2.png", class = "topimage", width = "90%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                )),
                       br(),
                       fluidRow(
                         column(12,
                               h4(strong("Description")),
                               p("In this section we are performing correlation analysis using Pearson Correlation Coefficient method. At the top of each plot, we can find the statistical significance (p) value besides Pearson correlation coefficient (r) and number of observations (n).
                                 In these plots we can also observe the histograms (distribution) of each axis/variable on top and right side."), align="justify",
                               p("The first correlation plot is between annual cumulative precipitation z-score and annual NDVI z-score at Department level (admin 2) for select years 2011, 2014, 2015, 2017, and 2018. To have coherence and better understand correlation, 
                                 we have kept observations only from departments that are covered in 2018 LSMS household survey 61 out of 67 total departments in Niger. We observe that correlation precipitation and NDVI z-scores are positively correlated except in 2017. 
                                 The higher correlation is in 2011 (r = 0.52), whereas 2015 shows nearly zero correlation (r = 0.02) with low significance. There is a year-to-year change in this relationship in strength and the direction."), align="justify",
                               p("Figures 2 and 3 shows correlation graphs between share of per capita food expenditure with annual precipitation z-score and NDVI z-score respectively. The share of per capita food expenditure is obtained from LSMS data after aggregating at department level
                                 taking median values of per capita food expenditure and per capita total expenditure in a household. In both figures we observe little or no correlation between drought indices and share of per capita food expenditure. The statistical significance also varies in each year.
                                 Such low or no correlation could be a result of aggregation at a higher administrative unit due to which we might be losing the relationship that is measured at household level. "), align="justify",
                                )),
                      br(),
                       fluidRow(
                         column(12,
                          #align="justify",
                          h2(strong("Correlation Analysis at Commune level")),
                          align="center")
                               ),
                      br(),
                 
                       fluidRow( 
                          column(4,
                          h4(strong("4. Precipitation & NDVI")), align="center",
                          img(src = "corr_preip_ndvi_adm3_248.png", class = "topimage", width = "107%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                 ),
                   
                          column(4, 
                          h4(strong("5. Precipitation & Share of Food Expenditure")), align="center",
                          img(src = "corr_precipZ_fdecp_adm3.png", class = "topimage", width = "82%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                 ),
                          column(4,
                          h4(strong("6. NDVI & Share of Food Expenditure")), align="center",
                          img(src = "corr_ndviZ_fdecp_adm3.png", class = "topimage", width = "85%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                )
                          ),
                     br(),
                     
                      fluidRow(
                        column(12,
                              h4(strong("Description")),
                              p("In order to better understand the correlation between drought indices and share of food expenditure we have now aggregated the values at commune level (admin 3) in figures 4, 5, and 6. 
                                In figure 4 we have excluded same departments which are not covered in 2018 LSMS data."), align="justify",
                              p("Figure 4 shows that correlation precipitation and NDVI z-scores are positively correlated except in 2017. The higher correlation is in 2011 (r = 0.62) with statistical significance. 
                                Whereas 2015 and 2017 shows very zero correlation but in different direction with significance. In year 2018 it appears to be affected by extreme values of NDVI z-score resulting in low correlation. "), align="justify",
                              p("The correlations of share of food expenditure with both precipitation and NDVI z-scores in figures 5 and 6 are showing very low correlation with high statistical significance. 
                                This again directs us towards the aggregation effect which was done at commune level (admin 3). Hence it would be interesting to observe similar relationship at the disaggregated level, 
                                enumeration area or at household level for better understanding. These relationships could improve potentially when we include other factors such as soil moisture and temperature to analyze drought conditions in Niger."), align="justify",
                 )),
),
                 ## Tab Takeaways ---------------------------------------------------------------
                 tabPanel("Takeaways",
                          column(3),
                          column(6,
                                 h1(strong("Takeaways"), align = "center"),
                                 p("Comparing data that is aggregated at mean vs median level shows differences 
                                 in the Z-Score maps the most and not much differences in annual totals. 
                                 Looking at data that is using admin 2 vs admin 3 its easy
                                 to see where the data is most significant at a more refined locations. 
                                 It is easier to spot variation when comparing seasonal rainfall maps versus 
                                 annual rainfall maps."), 
                                 p("Comparing data that NDVI..."),
                                 p("Comparing per capita food expenditures and total expenditures aggregated at median level, we analyzed that per capita food expenditures have been lowest in the southern region and lower in 2018 as compared to 2011 and 2014 at both admin levels. 
                                   We also observed that total expenditures are lowest in southern regions and highest in northwest regions at both admin levels"),
                                 p("Although NDVI and Precipitation appear to positively correlate with one another, especially in 2011 and 2014 when droughts were reported, we observe very low correlation between drought indices (precipitation and NDVI) and 
                                 share of food expenditure. This limited relationship may be a result from the high degree of aggregation across spatial units that causes us to lose some of the variation in underlying conditions."),
                                 p("Therefore, to further examine, our next proposed are to include alternative indicators for drought e.g., water resource stress index and soil moisture may also exhibit greater correspondence with agricultural drought conditions, to 
                                 disaggregate the food insecurity data to focus on smaller administrative units that would illustrate greater variation over space, and to evaluate alternative approaches to measure correspondence that may account for nonlinear relationships
")
                          )),
                 
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
                                     tags$br(), p(a(href = 'https://www.linkedin.com/in/poonam-tajanpure-72a64523b/', 'Poonam Tajanpure', target = '_blank'), "(Virginia Tech, Biological Systems Engineering)"),
                                     h4(strong("Graduate Research Assistant")), tags$br(),
                                     tags$br(), img(src = "fellow-armine.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width="37%", height="37%"),
                                     tags$br(), p(a(href = 'https://www.linkedin.com/in/poghosyan-armine/', 'Armine Poghosyan', target = '_blank'), "(Virginia Tech, Department of Agricultural and Applied Economics)")
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
                            ),
                              
                            br(),
                              fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                       h4(strong("Project Stakeholders"), align = "center"),
                                       p("The World Bank"),
                                       p(em("Disclaimer: "),("This project is an academic exercise conducted by VT-Data Science for the Public Good. The findings, interpretations, and conclusions expressed here do not necessarily reflect the views of the World Bank."))
                                       
                              ),
                            ))
                 
)


#####-------------------------------------- Define server  -------------------------------------#####

server <- function(input, output) {
  # Run JavaScript Code
  runjs(jscode)
  output$food_insecurity_out1<-renderPlotly(ggplotly({map_at_risk},tooltip="text"))
  output$food_insecurity_out2<-renderPlotly(ggplotly({map_at_moderate_risk},tooltip = "text"))
  output$food_insecurity_out3<-renderPlotly(ggplotly({map_at_severe_risk},tooltip = "text"))
  
  food_expenditure<-reactive({
    input$food_expenditure
  })
  output$food_expenditure_out<-renderImage({
    if(food_expenditure()=="Admin2"){
      list(src='www/deflated_foodexp_admin2.png', align = "center",width=725,height=425)
    }
    else if (food_expenditure()=="Admin3"){
      list(src='www/deflated_foodexp_admin3.png', align = "center",width=725,height=425)
    }
  })
  
  total_expenditure<-reactive({
    input$total_expenditure
  })
  output$total_expenditure_out<-renderImage({
    if(total_expenditure()=="total_Admin2"){
      list(src='www/deflated_totalexp_admin2.png', align = "center",width=725,height=425)
    }
    else if (total_expenditure()=="total_Admin3"){
      list(src='www/deflated_totalexp_admin3.png', align = "center",width=725,height=425)
    }
  })
  

    output$plot1 <- renderPlotly({
    
      annualPrecip %>%
        ggplot(aes(x = Year, y = Precipitation, color = Region)) +
        geom_line()+ 
        scale_color_viridis_d(option = "H") +
        labs(title = "Mean", 
             color =  "Region", x = "Year", 
             y = "Total Precipitation (mm)") + 
        theme_classic() +
        plotly()
    })
    output$plot2 <- renderPlotly({
      
      annualPrecip_md %>%
        ggplot(aes(x = Year, y = Precipitation, color = Region)) +
        geom_line()+ 
        scale_color_viridis_d(option = "H") +
        labs(title = "Median",
             color =  "Region", x = "Year", 
             y = "Total Precipitation (mm)") + 
        theme_classic() +
        plotly()
    })
    
    precipitation <- reactive({
      input$precipitation
    })
    output$precipitation_out<-renderImage({
      if(precipitation()=="Admin2"){
        list(src='www/annualRainfallZScoreAdmin2Comparisons.png', align = "center",width=800,height=500)
      }
      else if (precipitation()=="Admin3"){
        list(src='www/annualRainfallZScoreAdmin3Comparisons.png', align = "center",width=800,height=500)
      }
    })
    
    seasonalPrecip <- reactive({
      input$seasonalPrecip
    })
    output$seasonalPrecip_out<-renderImage({
      if(seasonalPrecip()=="Admin2seasonal"){
        list(src='www/seasonalRainfallZScoreAdmin2Comparisons.png', align = "center",width=800,height=500)
      }
      else if (seasonalPrecip()=="Admin3seasonal"){
        list(src='www/seasonalRainfallZScoreAdmin3Comparisons.png', align = "center",width=800,height=500)
      }
    })
    
    
    output$plot3 <- renderPlotly({
      annualndvi %>%
        ggplot(aes(x = Year, y = NDVI, color = Region)) +
        geom_line()+ 
        scale_color_viridis_d(option = "H") +
        labs(title = "Mean", 
             color =  "Region", x = "Year", 
             y = "Peak NDVI") + 
        theme_classic() +
        plotly()
    })
    
    output$plot4 <- renderPlotly({
      annualndvi_md %>%
        ggplot(aes(x = Year, y = NDVI, color = Region)) +
        geom_line()+ 
        scale_color_viridis_d(option = "H") +
        labs(title = "Median",
             color =  "Region", x = "Year", 
             y = "Peak NDVI") + 
        theme_classic() +
        plotly()
    }) 
    NDVI <- reactive({
      input$NDVI
    })
    output$NDVI_out<-renderImage({
      if(NDVI()=="Admin2"){
        list(src='www/annualNDVIZScoreAdmin2Comparisons.png', align = "center",width=800,height=500)
      }
      else if (NDVI()=="Admin3"){
        list(src='www/annualNDVIZScoreAdmin3Comparisons.png', align = "center",width=800,height=500)
      }
    })
    seasonalNDVI <- reactive({
      input$seasonalNDVI
    })
    output$seasonalNDVI_out<-renderImage({
      if(seasonalNDVI()=="Admin2seasonal"){
        list(src='www/seasonalNDVIZScoreAdmin2Comparisons.png', align = "center",width=800,height=500)
      }
      else if (seasonalNDVI()=="Admin3seasonal"){
        list(src='www/seasonalNDVIZScoreAdmin3Comparisons.png', align = "center",width=800,height=500)
      }
    })
 
}

#####---------------------------------- Run the application ----------------------------------#####
shinyApp(ui = ui, server = server)