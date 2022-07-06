#(NigershpMergedwithexcel.Rmd file in R)
  
#latest version of Food insecurity 
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Milind Gupta 
# Project name: Sahel 
# Purpose: Generate Maps of Food Insecurity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load Libraries

library(ggplot2)
library(dplyr)
library(readxl)
library(leaflet)
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

setwd("C:/Users/Milind Gupta/Desktop/2022_DSPG_SAHEL/2022_DSPG_Sahel")
mydata <-read_excel("Données EVIAM 15 17 insécurite alimentaire.xlsx")
head(mydata)



# reading admin 2 shape file and plotting it

library(sf)
Niger_level2 <- st_read(
  "C:/Users/Milind Gupta/Desktop/dspg-sahel/Level 2_new/niger_admin2.shp")
ggplot() + 
  geom_sf(data = Niger_level2, size = 1, color = "black", fill = "cyan1") + 
  ggtitle("Niger_level2") + 
  coord_sf()

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
#mydata$departement_name <- sub("Tibiri ?Doutchi?", "Tibiri", mydata$departement_name)
mydata$departement_name <- sub("Tibiri .Doutchi.", "Tibiri", mydata$departement_name)

mydata$departement_name <- sub("Tillaberi", "Tillabéri", mydata$departement_name)


# Moderate food insecurity in 2015
nigerShpMerged=merge(x=Niger_level2,y=mydata,by.x="admin2Name",by.y="departement_name",all=TRUE)
Moderate2015<-ggplot() + 
  geom_sf(data = nigerShpMerged, size = 1, color = "NA", aes(fill=moderate_part_15)) + 
  ggtitle("Population at Moderate Risk of Food Insecurity in Niger - 2015") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  remove_chart_clutter
Moderate2015

# Moderate food insecurity in 2017
nigerShpMerged=merge(x=Niger_level2,y=mydata,by.x="admin2Name",by.y="departement_name",all=TRUE)
Moderate2017<-ggplot() + 
  geom_sf(data = nigerShpMerged, size = 1, color = "NA", aes(fill=moderate_part_17)) + 
  ggtitle("Population at Moderate Risk of Food Insecurity in Niger - 2017") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  remove_chart_clutter
Moderate2017


# At risk food insecurity in 2015
nigerShpMerged=merge(x=Niger_level2,y=mydata,by.x="admin2Name",by.y="departement_name",all=TRUE)
Atrisk2015<-ggplot() + 
  geom_sf(data = nigerShpMerged, size = 1, color = "NA", aes(fill=arisque_part_15)) + 
  ggtitle("Population at Risk of Food Insecurity in Niger - 2015") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  remove_chart_clutter
Atrisk2015

#At risk food insecurity in 2017
nigerShpMerged=merge(x=Niger_level2,y=mydata,by.x="admin2Name",by.y="departement_name",all=TRUE)
Atrisk2017<-ggplot() + 
  geom_sf(data = nigerShpMerged, size = 1, color = "NA", aes(fill=arisque_part_17)) + 
  ggtitle("Population at Risk of Food Insecurity in Niger - 2017") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  remove_chart_clutter
Atrisk2017

#Severe Risk food insecurity 2015

nigerShpMerged=merge(x=Niger_level2,y=mydata,by.x="admin2Name",by.y="departement_name",all=TRUE)
Severe2015<-ggplot() + 
  geom_sf(data = nigerShpMerged, size = 1, color = "NA", aes(fill=severe_part_15)) + 
  ggtitle("Population at Severe Risk of Food Insecurity in Niger - 2015") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  remove_chart_clutter
Severe2015

#Severe food insecurity in 2017

nigerShpMerged=merge(x=Niger_level2,y=mydata,by.x="admin2Name",by.y="departement_name",all=TRUE)
Severe2017<-ggplot() + 
  geom_sf(data = nigerShpMerged, size = 1, color = "NA", aes(fill=severe_part_17)) + 
  ggtitle("Population at Severe Risk of Food Insecurity in Niger - 2017") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  remove_chart_clutter
Severe2017

#Reading Niger admin 2 codes 

admin2listdata<-read_excel("C:/Users/Milind Gupta/Desktop/2022_DSPG_SAHEL/2022_DSPG_Sahel/niger_adm2_listCode.xlsx")


#Joining the food insecurity data with the codes

datawithCode<-left_join(admin2listdata, mydata, by=c("departement_name"="departement_pov"))


#Converting atrisk data from wide to long to use facet wrap

Atrisk<-mydata%>%select(departement_name,arisque_part_15,arisque_part_17)%>% rename("2015" = arisque_part_15,"2017" = arisque_part_17)
AtriskLong<-gather(Atrisk,key="year", value="ATRISK", 2:3)


#Maps for atrisk side by side
AtriskmergedShp=merge(x=Niger_level2,y=AtriskLong,by.x="admin2Name",by.y="departement_name",all=TRUE)
Mapatrisk<-ggplot() + 
  geom_sf(data = AtriskmergedShp, size = 1, color = "NA", aes(fill=ATRISK)) + 
  ggtitle("Population at  Risk of Food Insecurity by department (admin2)") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  facet_wrap(~year)
Mapatrisk

#Converting moderate risk data from wide to long to use facet wrap
Moderaterisk<-mydata%>%select(departement_name,moderate_part_15,moderate_part_17)%>% rename("2015" = moderate_part_15,"2017" = moderate_part_17)
ModerateriskLong<-gather(Moderaterisk,key="year", value="MODERATERISK", 2:3)

#Maps for moderate risk side by side

ModerateriskmergedShp=merge(x=Niger_level2,y=ModerateriskLong,by.x="admin2Name",by.y="departement_name",all=TRUE)
MapModeraterisk<-ggplot() + 
  geom_sf(data = ModerateriskmergedShp, size = 1, color = "NA", aes(fill=MODERATERISK)) + 
  ggtitle("Population at Moderate Risk of Food Insecurity by department (admin2)") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  facet_wrap(~year)
MapModeraterisk



#Converting severe risk data from wide to long to use facet wrap
library(dplyr)
Severerisk<-mydata%>%select(departement_name,severe_part_15,severe_part_17)%>% rename("2015" = severe_part_15,"2017" = severe_part_17)
SevereriskLong<-gather(Severerisk,key="year", value="SEVERERISK", 2:3)


#maps for severe risk side by side

SevereriskmergedShp=merge(x=Niger_level2,y=SevereriskLong,by.x="admin2Name",by.y="departement_name",all=TRUE)
MapSevererisk<-ggplot() + 
  geom_sf(data = SevereriskmergedShp, size = 1, color = "NA", aes(fill=SEVERERISK)) + 
  ggtitle("Population at Severe Risk of Food Insecurity by department (admin2)") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  facet_wrap(~year)
MapSevererisk


#Legend at bottom for the presentation

AtriskmergedShp=merge(x=Niger_level2,y=AtriskLong,by.x="admin2Name",by.y="departement_name",all=TRUE)
Mapatrisk<-ggplot() + 
  geom_sf(data = AtriskmergedShp, size = 1, color = "NA", aes(fill=ATRISK)) + 
  ggtitle("Population at  Risk of Food Insecurity by department (admin2)") + scale_fill_viridis_c()+
  coord_sf()+
  labs(fill = "Percent of Population")+theme_classic()+
  facet_wrap(~year)+theme(legend.position="bottom")

Mapatrisk


