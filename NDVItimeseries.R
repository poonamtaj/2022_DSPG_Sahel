#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Riley Rudd
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Fri Jul 1 2022 ------------------------------
# Date Last Updated: Tues Jul 5 2022
# R version: 4.1.3
# Purpose: Load and Clean NDVI Data Files
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(haven) # for read_dta
library(tidylog) # for output on operations/changes with select, drop, merges, etc.
library(rio)
library(janitor)
library(sf) # for st_read

library(readr)
library(lubridate) # for dates

library(reshape2)
library(ggthemes)
library(openxlsx)
library(sf)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clear Environment and Set Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls(all = TRUE))

remove_chart_clutter <- 
  theme(    
    panel.grid.major = element_blank(),      # Remove panel grid lines
    panel.grid.minor = element_blank(),      # Remove panel grid lines
    panel.background = element_blank(),      # Remove panel background
    axis.line = element_line(colour = "grey"),       # Add axis line
    axis.title.y = element_text(angle = 0, vjust = 0.5),      # Rotate y axis so don't have to crank head
    legend.position="bottom"
  ) 

# Don't show scientific notation
options(scipen = 999)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#admin 2

NDVI1admin2 <-read.csv("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/NDVIMean_ts_niger (1981-2000).csv")

NDVI2admin2<-read.csv("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/NDVIMean_ts_niger (2001-2022).csv")

#admin 3

NDVI1admin3 <-read.csv("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/l3NDVIMean_ts_niger (1981-2000).csv")

NDVI2admin3 <- read.csv("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/l3NDVIMean_ts_niger (2001-2022).csv")

#merge datasets
NDVIadmin2 <- dplyr::left_join(NDVI1admin2, NDVI2admin2)
NDVIadmin3 <- dplyr::left_join(NDVI1admin3, NDVI2admin3)

# geospatial
nigerMapAdmin2 <- 
  st_read("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/Level 2 (1)/wb_niger_admin2_shapefile/niger_admin2.shp") 


nigerMapAdmin3 <- 
  st_read("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/Level 3 (1)/NER_adm03_feb2018.shp")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean Column Names
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for ( col in 1:ncol(NDVIadmin2)){
  colnames(NDVIadmin2)[col] <- sub("_NDVI", "", colnames(NDVIadmin2)[col])
}
for ( col in 1:ncol(NDVIadmin2)){
  colnames(NDVIadmin2)[col] <- sub("X", "", colnames(NDVIadmin2)[col])
}

for ( col in 1:ncol(NDVIadmin3)){
  colnames(NDVIadmin3)[col] <- sub("_NDVI", "", colnames(NDVIadmin3)[col])
}
for ( col in 1:ncol(NDVIadmin3)){
  colnames(NDVIadmin3)[col] <- sub("X", "", colnames(NDVIadmin3)[col])
}
head(NDVIadmin3)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Convert data from Wide to Long ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



ndviData2 <- 
  NDVIadmin2 %>%
  select(admin1Name, admin1Pcod, 
         admin2Name, admin2Pcod, "19810701": "20191231", -admin0Name, -admin0Pcod, -system.index) 

ndviDataLong2 <- 
  pivot_longer(ndviData2, 
         names_to = "Date", 
         values_to = "ndvi", 
         cols = "19810701":"20191231") %>% 
  mutate(Date = ymd(Date)) #convert from char to dates
# "19810701":"20191231") 


ndviData3 <-
  NDVIadmin3 %>%
  select(adm_03, rowcacode3,  
         "19810701":"20191231", -adm_01, -ISO3, -adm_02, -rowcacode1, -rowcacode2, -ISO2, -NOM_COM, -system.index)

ndviDataLong3 <-
  pivot_longer(ndviData3,
         names_to = "Date",
         values_to = "ndvi",
         cols = "19810701":"20191231") %>% 
  mutate(Date = ymd(Date)) # Create new col with dates instead of chars

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Adjust Scale by 0.0001 ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ndviDataLong2 <- ndviDataLong2  %>%
  mutate(newndvi =  as.numeric(ndvi) *0.0001)

# Review the data as a check -- reasonable ranges, expected number of observations, etc?
ndviDataLong2 %>% glimpse()
ndviDataLong2 %>% summary() 

ndviDataLong3 <- ndviDataLong3  %>%
  mutate(newndvi =  as.numeric(ndvi) *0.0001)

# Review the data as a check -- reasonable ranges, expected number of observations, etc?
ndviDataLong3 %>% glimpse()
ndviDataLong3 %>% summary() 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Convert Column Names to Dates and create new columns ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_admin2 <- 
  ndviDataLong2 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()

data_admin3 <- 
  ndviDataLong3 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total NDVI Graphics By Admin 0 (country) and Admin 1 (building off admin 2) ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 0 (one line for the entire region)
# Calculate total admin 2 and 3 ndvi for maps (if every obs is on admin 2 basis, then show)
yearData_admin2 <- 
  data_admin2 %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  mutate(peak_ndvi = max(newndvi, na.rm = TRUE)) %>%
  distinct(admin1Name, admin1Pcod,admin2Name, admin2Pcod, year, 
           peak_ndvi) %>%
  ungroup()

yearData_admin3 <- 
  data_admin3 %>% 
  group_by(adm_03, rowcacode3, year) %>%
  mutate(peak_ndvi = max(newndvi, na.rm = TRUE)) %>%
  distinct(adm_03, rowcacode3, year, 
           peak_ndvi) %>%
  ungroup()

peakndvi <- 
  yearData_admin2 %>%
  group_by(year) %>%
  mutate(meanAnnualndvi = mean(peak_ndvi)) %>%
  ungroup() %>%
  distinct(year, meanAnnualndvi)

peakndvi %>% 
  ggplot(aes(x=year, y=meanAnnualndvi)) +
  geom_line() + 
  labs(title="Peak NDVI Across Niger by Year (Admin 0)", x="Year", y ="Peak NDVI") +
  theme_classic() 


# Take the mean over the spatial area for a given year ----
yearData_admin1 <-
  yearData_admin2 %>% 
  group_by(admin1Name, year) %>% 
  mutate(peak_ndvi_annual_admin1 = mean(peak_ndvi)) %>% 
  ungroup() %>% 
  distinct(admin1Name, peak_ndvi_annual_admin1, year)

### Generate Overall ndvi  Graphic ---- 
yearData_admin1  %>% 
  filter(year < 2022) %>% 
  ggplot(aes(x = year, 
             y = peak_ndvi_annual_admin1, 
             color = admin1Name, linetype = admin1Name)) +
  geom_line() +
  labs(title = "Peak NDVI By Administrative Region (Admin 1)", 
       caption = "Data Source: NOAA CDR AVHRR NDVI",
       y = "", 
       x = "",
       color = "Region",
       linetype = "Region") +
  remove_chart_clutter +
  theme(legend.position = "right")

# What's the avg peak ndvi per year in Niger?
mean(yearData_admin1$peak_ndvi_annual_admin1, na.rm = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Peak NDVI Maps (Admin 2) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerShpMerged_admin2 = full_join(nigerMapAdmin2, 
                                  yearData_admin2,
                                  by=c("admin2Pcod" = "admin2Pcod"))

# 3. Generate map --- 
nigerShpMerged_admin2 %>% 
  filter(year %in% seq(2011, 2021, 1)) %>% 
  ggplot() +
  geom_sf(aes(fill = peak_ndvi), color = "NA") + 
  scale_fill_viridis_c(direction = -1) +
  labs(title="Annual Peak NDVI by Department (Admin 2)", 
       caption = "Data Source: NOAA CDR AVHRR NDVI dataset", 
       fill = "NDVI" ) +
  facet_wrap(~year) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total NDVI Maps (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2. Merge NDVI and  Spatial Information (geometry) by common code
nigerShpMerged_admin3= full_join(nigerMapAdmin3, 
                                  yearData_admin3,
                                  by=c("rowcacode3" = "rowcacode3"))


nigerShpMerged_admin3 %>% 
  filter(year %in% seq(2011, 2021, 1)) %>% 
  ggplot() +
  geom_sf(aes(fill = peak_ndvi), color = "NA") + 
  scale_fill_viridis_c(direction = -1) +
  labs(title="Annual Peak NDVI by Commune (Admin 3)", 
       caption = "Data Source: NOAA CDR AVHRR NDVI dataset", 
       fill = "NDVI" ) +
   facet_wrap(~year) +
   theme(
     axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     axis.ticks = element_blank(),
     rect = element_blank()
   ) 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Scores (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate baseline dataset

 admin2ndvi <- 
   yearData_admin2 %>% 
    group_by(admin2Pcod) %>% 
   group_by(admin2Name) %>%
    filter(year >= "1981" & year <= "2010") %>% 
   mutate(baselineMean_ndvi = mean(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE))
    summarise(baselineMean_ndvi = mean(peak_ndvi, na.rm = TRUE),
              baselineSD_ndvi = sd(peak_ndvi, na.rm = TRUE)) %>%
   ungroup()

admin2ndvi <- 
  yearData_admin2 %>% 
  select(admin1Name,admin1Pcod, admin2Name, admin2Pcod, year, peak_ndvi) %>% 
  group_by(admin2Name) %>%  group_by(admin2Pcod) %>%
  mutate(baselineMean_ndvi = mean(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_ndvi = sd(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Name, year) %>% 
  mutate(zscore_ndvi = (peak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)) %>% 
  ungroup()

admin3ndvi <- 
  yearData_admin3 %>% 
  group_by(adm_03) %>% 
  group_by(rowcacode3) %>%
  filter(year >= "1981" & year <= "2010") %>% 
  mutate(baselineMean_ndvi = mean(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE))
summarise(baselineMean_ndvi = mean(peak_ndvi, na.rm = TRUE),
          baselineSD_ndvi = sd(peak_ndvi, na.rm = TRUE)) %>%
  ungroup()

admin3ndvi <- 
  yearData_admin3 %>% 
  select(adm_03, rowcacode3, year, peak_ndvi) %>% 
  group_by(adm_03) %>%  group_by(rowcacode3) %>%
  mutate(baselineMean_ndvi = mean(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_ndvi = sd(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(adm_03, year) %>% 
  mutate(zscore_ndvi = (peak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)) %>% 
  ungroup()

# Review the data as a check -- reasonable ranges, expected number of observations, etc?
admin2ndvi %>% glimpse()
admin2ndvi %>% summary() 


 baselineData_admin2 <- 
   yearData_admin2 %>% 
    group_by(admin2Pcod) %>% 
   group_by(admin2Pcod) %>%
   filter(year >= "1981" & year <= "2010") %>% 
   summarise(baselineMean_ndvi = mean(peak_ndvi, na.rm = TRUE),
             baselineSD_ndvi = sd(peak_ndvi, na.rm = TRUE)) %>%
   ungroup()

# # Focus on the "post" time period
 periodData_admin2 <-
   yearData_admin2 %>% 
   group_by(admin2Name,admin2Pcod, year) %>%
   filter(year >= "2011" & year <= "2021") %>% 
   ungroup()
 
# # Merge the two together
 datajoin_admin2 <- 
   baselineData_admin2 %>% 
   left_join(periodData_admin2, by = "admin2Pcod") %>%
   mutate(zscore_ndvi = (peak_ndvi - baselineMean_ndvi)/
                                    (baselineSD_ndvi))
 
# # Check the distribution of z-scores
 datajoin_admin2  %>% tabyl(zscore_ndvi)

 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # Merge Z-scores with Shapefiles to generate maps-----
 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 # First merge in ndvi scores
 
 nigerShpMerged_admin2 =
   nigerMapAdmin2 %>% 
   full_join(admin2ndvi, by=c("admin2Pcod" = "admin2Pcod")) 
 
 # also merge in admin3
 nigerShpMerged_admin3 =
   nigerMapAdmin3 %>% 
   full_join(admin3ndvi, by=c("adm_03" = "adm_03"))
 # Generate Z-Score Maps (Admin 2 and 3) -----
 nigerShpMerged_admin2 %>% 
   filter(year %in% c(2011, 2014, 2015, 2017, 2018)) %>% 
   ggplot() + 
   geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
   scale_fill_viridis_c(direction = -1) +
   facet_wrap(~year, nrow = 1) +
   labs(title="Annual Z-Score NDVI by Department (Admin 2)", fill = "z-score" ) + 
   theme_classic() + 
   theme(axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         rect = element_blank())
 
  nigerShpMerged_admin3 %>% 
    filter(year %in% c(2011, 2014, 2015, 2017, 2018)) %>% 
             ggplot() + 
             geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
             scale_fill_viridis_c(direction = -1) +
             facet_wrap(~year, nrow = 1) +
             labs(title="Annual Z-Score NDVI by  Commune (Admin 3)", 
                  fill = "z-score" ) + 
             theme_classic() + 
             theme(axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   rect = element_blank())


 
 # All Years 3 Regions
 
 Desert = (yearData_admin2$admin1Name == "Agadez"|yearData_admin2$admin1Name == "Diffa"|yearData_admin2$admin1Name == "Zinder")
 agroPastoral = (yearData_admin2$admin1Name == "Tahoua"|yearData_admin2$admin1Name == "Maradi")
 rainfed = (yearData_admin2$admin1Name == "Tillabery" | yearData_admin2$admin1Name == "Dosso"|| yearData_admin2$admin1Name == "Niamey")
 
 # Filtered 1981 to 2010 Baseline Data ----
 
  baselineData_admin2 <- yearData_admin2 %>% 
    group_by(admin2Name, admin2Pcod) %>%
    filter(year >= "1981" & year <= "2010") %>% 
    summarise(baselineTotal_ndvi = sum(peak_ndvi, na.rm = TRUE),
              baselineMean_ndvi = mean(peak_ndvi, na.rm = TRUE),
              baselineSD_ndvi = sd(peak_ndvi, na.rm = TRUE)) %>%
    ungroup()
  
  baselineData_admin3 <- yearData_admin3 %>% 
    group_by(adm_03, rowcacode3) %>%
    filter(year >= "1981" & year <= "2010") %>% 
    summarise(baselineTotal_ndvi = sum(peak_ndvi, na.rm = TRUE),
              baselineMean_ndvi = mean(peak_ndvi, na.rm = TRUE),
              baselineSD_ndvi = sd(peak_ndvi, na.rm = TRUE)) %>%
    ungroup()
  
  
 # # 2011 to 2020 Filtered with baseline z-score
  periodData_admin2 <- yearData_admin2 %>% 
    group_by(admin2Name,admin2Pcod, year) %>%
    filter(year>="2011" & year<="2021") %>% 
    ungroup()
 
  
 
  datajoin_admin2 = full_join(x = baselineData_admin2, y = periodData_admin2, by = "admin2Pcod") %>%
    mutate(zscore_precipitation = ((peak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)))

  
  periodData_admin3 <- yearData_admin3 %>% 
    group_by(adm_03,rowcacode3, year) %>%
    filter(year >= "2011" & year <= "2021") %>% 
   ungroup()
  
  
  
  datajoin_admin3 = merge(x = baselineData_admin3, y = periodData_admin3, by = "rowcacode3") %>%
    mutate(zscore_ndvi = ((peak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)))

  
  
  
 # Total NDVI by Month Summary
  monthlyData_admin2 <- 
    data_admin2 %>% 
    group_by(admin2Name,admin2Pcod, month) %>%
    filter(year == "2020") %>% 
    mutate(monthlyTotal_ndvi = sum(newNDVI, na.rm = TRUE),
           monthlyMean_ndvi = mean(newNDVI, na.rm = TRUE)) %>%
    ungroup()

   
  monthlyData_admin3 <- data_admin3 %>% 
    group_by(adm_03,rowcacode3, month) %>%
    filter(year == "2020") %>% 
    mutate(monthlyTotal_ndvi = sum(newNDVI, na.rm = TRUE),
           monthlyMean_ndvi = mean(newNDVI, na.rm = TRUE)) %>%
    ungroup()

 
 
 # Seasonal NDVI Summary
 
 monthlyData_admin2 <- data_admin2 %>% 
   group_by(admin2Name,admin2Pcod, month) %>%
   filter(month >= 5 & month <= 10) %>% 
   mutate(monthlyTotal_ndvi = sum(newNDVI, na.rm = TRUE),
          monthlyMean_precipitation = mean(newNDVI, na.rm = TRUE)) %>%
   ungroup()
 
 
 
  
  monthlyData_admin3 <- data_admin3 %>% 
    group_by(adm_03,rowcacode3, month) %>%
    filter(month >= 5 & month <= 10) %>% 
    mutate(monthlyTotal_ndvi = sum(newNDVI, na.rm = TRUE),
           monthlyMean_ndvi = mean(newNDVI, na.rm = TRUE)) %>%
    ungroup()
 
 #Visuals
 
 # Total ndvi Admin 2 and Admin 3 Maps + Bar Graph
 nigerShpMerged_admin2 %>% 
   ggplot() + 
   geom_sf(aes(fill = peak_ndvi),
           color = NA,
           alpha = 0.8) +
   scale_fill_viridis_c(direction = -1) +
   labs(title = "Annual Peak NDVI by Department (Admin 2)",
        caption = "Data Source: NOAA CDR AVHRR NDVI dataset",
        fill = "NDVI") +
   facet_wrap(~year) +
   theme_classic() + theme(
     axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     axis.ticks = element_blank(),
     rect = element_blank()) 
 
 ggplot(nigerShpMerged_admin3) + theme_classic() + theme(
   axis.text.x = element_blank(),
   axis.text.y = element_blank(),
   axis.ticks = element_blank(),
   rect = element_blank()
 ) +
   geom_sf(aes(fill = peak_ndvi),
           color = NA,
           alpha = 0.8) +
   scale_fill_viridis_c(direction = -1) +
   labs(title = "Annual Peak NDVI by Commune (Admin 3)",
        caption = "Data Source: NOAA CDR AVHRR NDVI dataset",
        fill = "NDVI") +
   facet_wrap( ~ year)
 
 # Generate Overall Precipitation  Graphic ----
 yearData_admin1  %>%
   filter(year < 2019) %>%
   ggplot(aes(x = year,
              y = peak_ndvi_annual_admin1,
              color = admin1Name, linetype = admin1Name)) +
   geom_line() +
   labs(title = "Peak NDVI By Administrative Region (Admin 1)",
        caption = "Data Source: NOAA CDR AVHRR NDVI dataset",
        y = "",
        x = "",
        color = "Region",
        linetype = "Region") +
   remove_chart_clutter +
   theme(legend.position = "right")
 
 # At National Level -- Admin 0
 annualndvi <- 
   yearData_admin2 %>%
   group_by(year) %>%
   mutate(meanAnnualndvi = mean(peak_ndvi)) %>%
   ungroup() %>%
   distinct(year, meanAnnualndvi)
 
 annualndvi %>% 
   ggplot(aes(x=year, y=meanAnnualndvi, na.rm=TRUE)) +
   geom_line() +
   labs(title="Average NDVI by Year", x="Year", y="Average NDVI") +
   theme_classic() 
 
 write.csv(nigerShpMerged_admin2, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/2022_DSPG_Sahel/admin2NDVI.csv", row.names = FALSE)
 write.csv(nigerShpMerged_admin3, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/2022_DSPG_Sahel/admin3NDVI.csv", row.names = FALSE)
 