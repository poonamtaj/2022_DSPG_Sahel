
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Milind Gupta 
# Project name: Sahel 
# Date last updated: 4 Aug 2022
# Purpose: Generate Maps of Food Insecurity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Libraries

library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(tidyr)



# Standard ggplot has distracting features - adjust those

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
#setwd("C:/Users/Milind Gupta/Desktop/2022_DSPG_SAHEL/2022_DSPG_Sahel")
#mydata <-read_excel("Données EVIAM 15 17 insécurite alimentaire.xlsx")
#niger_level2 <- st_read("C:/Users/Milind Gupta/Desktop/dspg-sahel/Level 2_new/niger_admin2.shp")

mydata <-read_excel("./data/raw_data/Données EVIAM 15 17 insécurite alimentaire.xlsx")
niger_level2 <- st_read("./data/shapefiles/wb_niger_admin2_shapefile/niger_admin2.shp")


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
make_maps <- function(var1,var2,val,title) {
  #Converting data from wide to long to use facet wrap
  
  data_sub <- mydata %>% 
    select(departement_name,var1,var2) %>% 
    rename("2015" = var1,"2017" = var2)
  
  data_sub_long <- gather(data_sub,key="year", value=val, 2:3)
  
  # joining Admin2 shapefile with LSMS data
  data_merged_shp=merge(x=niger_level2, y=data_sub_long, by.x="admin2Name", by.y="departement_name", all=TRUE)
  # Make Maps side by side
  map_out<-ggplot() + 
    geom_sf(data = data_merged_shp, size = 1, color = "NA", aes(fill=val)) + 
    ggtitle(title) +
    scale_fill_viridis_c() +
    coord_sf() +
    labs(fill = "Percent of Population") +
    theme_classic() +
    remove_chart_clutter +
    facet_wrap(~year)
  return(map_out)  
}

#Atrisk maps
map_at_risk <- make_maps(var1="arisque_part_15",var2="arisque_part_17",val="ATRISK",title="Population at  Risk of Food Insecurity by department (admin2)")
map_at_risk

#Moderate Risk maps 
map_at_moderate_risk <- make_maps(var1="moderate_part_15",var2="moderate_part_17",val="MODERATERISK",title="Population at Moderate Risk of Food Insecurity by department (admin2)")
map_at_moderate_risk

#Severe Risk maps
map_at_severe_risk <- make_maps(var1="severe_part_15",var2="severe_part_17",val="SEVERERISK",title="Population at Severe Risk of Food Insecurity by department (admin2)")
map_at_severe_risk





