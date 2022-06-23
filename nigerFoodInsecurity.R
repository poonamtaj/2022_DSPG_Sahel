
################################################################
# Food Insecurity and Niger Level 2 Shape File
# By Catherine Back 2022
################################################################
#Load in Libraries


#Read in Data
library(readxl)
foodInsecurityData <- read_excel("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/Données EVIAM 15 17 insécurite alimentaire.xlsx")
summary(foodInsecurityData)

#Save Shape files
library(rgdal)
nigerMap <- readOGR("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/2_shapeFile")

#Plot Shape file                 
plot(nigerMap, col = "lightgreen", bg = "lightblue", lwd = 2)


###Merge Shape file and Excel Data
library(sf)
nigerShpMerged = merge(x = nigerMap ,y = foodInsecurityData , by.x="admin2Name", by.y="departement_name", all = TRUE)
ggplot() +
  geom_sf(data = nigerShpMerged, size = 1, color = "black", aes(fill = severe_part_15)) +
  ggtitle("Niger") + scale_fill_viridis_c()+
  coord_sf()

#severe_part_15,severe_part_17
library(ggplot2)
ggplot(data, aes(severe_part_15, severe_pop_15)) +
  geom_point() +
  labs( x = "Severe Part", y = "Severe Population Year: 2015") +
  geom_smooth()

ggplot(data, aes(severe_part_17, severe_pop_17)) +
  geom_point() +
  labs( x = "Severe Part", y = "Severe Population Year: 2017") +
  geom_smooth()


#Map
library(dplyr)
library(tidycensus)
# Remember the path
setwd('C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel')

readRenviron("~/.Renviron") #reading Census API key
Sys.getenv("census_api_key") #confirm you have read in your Census API key


# Set a year of interest
this.year = 2015

# This looks at the 5 year estimates
# You can also do "acs1"
vars <- load_variables(year = 2015,
                       dataset = data,
                       cache = TRUE)

# There are 22711 possible variables 
dim(vars)

