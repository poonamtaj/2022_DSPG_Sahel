##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Milind Gupta and Elinor Benami
# Project name: Sahel 
# Date Last Updated: # Mon Aug 3 2022
# Purpose: Import LSMS Data and Generate Maps of Food Expenditure and Total Expenditure for admin 3 in Niger
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 0. Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(haven)
library(tidylog)
library(janitor)
library(sf)
library(viridis)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading dataset
# TODO: coauthors -- change the file path/directory according to your path/directory
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
welfare_2011 <-read_dta("ECVMA_2011_welfare.dta")
welfare_2014 <-read_dta("ECVMA_2014_welfare.dta")
welfare_2018 <-read_dta("EHCVM_2018_welfare.dta")


#slimming down raw 2011 lsms dataset 
ecvma_slim_2011 <- 
  welfare_2011 %>% 
  select(year, hid, 
         cluster, 
         fdtexp, #Purchased and auto-consumption food expenditure, nominal (annual)
         nfdtexp, #Purchased & auto-consumption non-food expenditure, nominal (annual)
         hhtexp, #Household food and non-food consumption expenditure, nominal (annual)
         pc_fd, #Per capita food consumption expenditure, nominal (annual)
         pc_hh, #Per capita food and non-food consumption, nominal (annual)
         padq_fd, #Per adult equivalent food consumption expenditure, nominal (annual)
         padq_hh, #Per adult equivalent food and non-food consumption, nominal (annual)
         pc_fddr, #Per capita food consumption expenditure, deflated (annual)
         pc_hhdr, #Per capita food and non-food consumption expenditure, deflated (annual)
         hhsize, 
         int_year,
         int_month,
         latitude,
         longitude
  )

#slimming down raw 2014 lsms dataset 
ecvma_slim_2014 <- 
  welfare_2014 %>% 
  select(year, hid, 
         cluster, 
         fdtexp, #Purchased and auto-consumption food expenditure, nominal (annual)
         nfdtexp, #Purchased & auto-consumption non-food expenditure, nominal (annual)
         hhtexp, #Household food and non-food consumption expenditure, nominal (annual)
         pc_fd, #Per capita food consumption expenditure, nominal (annual)
         pc_hh, #Per capita food and non-food consumption, nominal (annual)
         padq_fd, #Per adult equivalent food consumption expenditure, nominal (annual)
         padq_hh, #Per adult equivalent food and non-food consumption, nominal (annual)
         pc_fddr, #Per capita food consumption expenditure, deflated (annual)
         pc_hhdr, #Per capita food and non-food consumption expenditure, deflated (annual)
         hhsize, 
         int_year,
         int_month,
         latitude,
         longitude
  )

# merge 2011 and 2014 datasets together
lsms_slim_2011_2014 <- 
  ecvma_slim_2014 %>% 
  rbind(ecvma_slim_2011)

#slimming down raw 2018 lsms dataset.
ecvma_slim_2018 <- 
  welfare_2018 %>% 
  select(#year, 
    hhid, 
    cluster, 
    pcexp_food, #bien-être (seulement alimentation)
    pcexp_nonfood, #bien-être (seulement non-alimentation)
    GPS__Latitude, 
    GPS__Longitude, 
    sh_food,
    sh_nonfood,
    cons_pc_real
  ) %>% 
  mutate(year = 2018) %>% #Adding year column
  rename(latitude = GPS__Latitude, #renaming latitude and longitude columns
         longitude = GPS__Longitude)

#Converting 2018 food expenditure and total expenditure into deflated values

#Converting from daily to annual by multiplying by 365(Number of days in a year)
ecvma_slim_2018$cons_pc_real_annual<-ecvma_slim_2018$cons_pc_real*365

#adding deflated annual food expenditure column
ecvma_slim_2018$food_deflated_annual<-ecvma_slim_2018$cons_pc_real_annual*ecvma_slim_2018$sh_food

#adding deflated annual non-food expenditure column
ecvma_slim_2018$nonfood_deflated_annual<-ecvma_slim_2018$cons_pc_real_annual*ecvma_slim_2018$sh_nonfood

#adding deflated annual total expenditure column
ecvma_slim_2018$total_expend_deflated<-ecvma_slim_2018$food_deflated_annual+ecvma_slim_2018$nonfood_deflated_annual

#renaming columns to match with 2011-2014 dataset
ecvma_slim_2018 <- 
  ecvma_slim_2018%>%
  rename(hid=hhid,pc_fddr=food_deflated_annual,pc_hhdr=total_expend_deflated)

ecvma_slim_2018$hid=as.character(ecvma_slim_2018$hid)

#merge 2018 dataset with 2011 and 2014 dataset
mergednewdata<-bind_rows(lsms_slim_2011_2014,ecvma_slim_2018)


#To extract the merged files
#write.csv(lsms_slim_2011_2014, 'lsms_slim_2011_2014.csv')
#write.csv(ecvma_slim_2018, 'deflated2018.csv')



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



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Load Merged Data for Analysis (from Prior Scripts) ------ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

LSMSdata <- mergednewdata

#Loading shape file of Niger(admin 3)
# TODO: coauthors -- change the file here for your path
niger_level3 <- st_read(
  "C:/Users/Milind Gupta/Desktop/dspg-sahel/Level 3_new/NER_adm03_feb2018.shp")

# Don't show scientific notation

options(scipen = 999)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Convert LSMS data to spatial dataset, generate median over admin unit ------ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# A. Convert LSMS into Spatial Dataset for simple features ---- 

lsms_sf <- st_as_sf(LSMSdata, 
                    coords=c("longitude","latitude"), 
                    crs="WGS84", #"EPSG:2931",
                    na.fail=FALSE)

# B. Join the spatial data and welfare information (what departments are each observation in?)
# If we use left=TRUE here, our result will retain all the points in the dataset rather 
# than just the the spatial overlaps (where points fall inside polygons).


lsms_admin3 <- 
  st_join(niger_level3, lsms_sf, left = TRUE) 

# C. Generate median values by geometry ---- 
# these lines may take a couple of minutes to run 

start_time <- Sys.time()
lsms_median <-
  lsms_admin3 %>%
  # clusters refer to one consistent enumeration area
  # there many be many enumeration areas per administrative region, so use the admin region instead
  group_by(adm_03, year) %>%
  # this should be no more than 266 observations per year, as we're working at admin3
  mutate(median_foodexpend = median(pc_fddr),
          median_totalexpend=median(pc_hhdr)      
  ) %>%
  ungroup() %>% 
  distinct(adm_03, year, median_foodexpend, median_totalexpend, 
           geometry)

# Develop check on number of units

nrow(lsms_median) < 266*3




end_time <- Sys.time()
end_time - start_time

# Time difference of 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Generate Maps ------ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. Median Annual PC Food Expenditure ---- 

niger_level3 %>%
  ggplot() +
  geom_sf(color = "NA") +
  geom_sf(data = na.omit(lsms_median), #na.omit gets rid of the NA facet
          size = 1, 
          color = "NA",
          aes(fill = median_foodexpend/100000), alpha = 1) + #alpha indicates transparency
  facet_wrap(~year, nrow = 1) +
  labs(title = "Median Annual Per Capita Food Expenditure by Commune (Admin 3)", 
       subtitle = "Units in 100,000 West African Francs, with 400-600 Francs to 1 USD",
       caption = paste0("Deflated to 2011 value (in real 2011 currency)\n",
                        "Gray indicates no observations. ",
                        "Data Source: LSMS")) + 
  coord_sf() +
  scale_fill_viridis(name = "Annual Median West African CFA Francs (100k)") +
 theme_classic() +
  theme(legend.position="bottom")+
  remove_chart_clutter+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())
  


# B. Median Annual PC Total Expenditure ---- 
niger_level3 %>%
  ggplot() +
  geom_sf(color = "NA") +
  geom_sf(data = na.omit(lsms_median), #na.omit gets rid of the NA facet
          size = 1, 
          color = "NA",
          aes(fill = median_totalexpend/100000), alpha = 1) + #alpha indicates transparency
  facet_wrap(~year, nrow = 1) +
  labs(title = "Median Annual Per Capita Total Expenditure by Commune (Admin 3)", 
       subtitle = "Units in 100,000 West African Francs, with 400-600 Francs to 1 USD",
       caption = paste0("Deflated to 2011 value (in real 2011 currency)\n",
                        "Gray indicates no observations. ",
                        "Data Source: LSMS")) + 
  coord_sf() +
  scale_fill_viridis(name = "Annual Median West African CFA Francs (100k)") +
  theme_classic() +
  theme(legend.position="bottom")+
  remove_chart_clutter+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())



