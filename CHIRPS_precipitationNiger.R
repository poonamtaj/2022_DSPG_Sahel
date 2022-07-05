#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Catherine Back and Elinor Benami
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Thu Jun 30 08:26:20 2022 ------------------------------
# Date Last Updated:
# R version: 4.1.3
# Purpose: Load and Clean Precip Data Files
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clear Environment and Set Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls(all = TRUE))

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

# Don't show scientific notation
options(scipen = 999)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(haven) # for read_dta
library(tidylog) # for output on operations/changes with select, drop, merges, etc.
library(rio)

library(openxlsx)
library(readxl)
library(moderndive) #necessary?
library(readr)
library(tidyr)
library(lubridate) # for dates

library(reshape2)
library(ggplot2) # included in tidyverse
library(ggthemes)
library(stringr)
library(sf)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
csvData2 <-read.csv("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/rainfallMean_ts_niger_adm2.csv")
csvData3 <-read.csv("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/rainfallMean_ts_niger_adm3.csv")

#Geospatial Data
nigerMap2 <- st_read("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/niger_admin2/niger_admin2.shp")
nigerMap3 <- st_read("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/niger_admin3/NER_adm03_feb2018.shp")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean Column Names -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for ( col in 1:ncol(csvData2)){
  colnames(csvData2)[col] <- sub("_precipitation", "", colnames(csvData2)[col])
}
for ( col in 1:ncol(csvData2)){
  colnames(csvData2)[col] <- sub("X", "", colnames(csvData2)[col])
}


for ( col in 1:ncol(csvData3)){
  colnames(csvData3)[col] <- sub("_precipitation", "", colnames(csvData3)[col])
}
for ( col in 1:ncol(csvData3)){
  colnames(csvData3)[col] <- sub("X", "", colnames(csvData3)[col])
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Convert from Wide to Long Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#admin 1 and 2
precipData2 <- 
  csvData2 %>%
  select(admin1Name, admin1Pcod, 
         admin2Name, admin2Pcod, "19810101": "20220426")

precipDataLong2 <- 
  gather(precipData2, 
         key = Date, 
         value = Precipitation, 
         "19810101":"20220121") %>% 
  mutate(Date = ymd(Date))


#admin 3
precipData3 <- 
  csvData3 %>%
  select(adm_03, rowcacode3, "19810101": "20220426")

precipDataLong3 <- 
  gather(precipData3, 
         key = Date, 
         value = Precipitation, 
         "19810101":"20220121") %>% 
  mutate(Date = ymd(Date))



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Convert Column Names to Dates and create new columns -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataAdmin2 <- 
  precipDataLong2 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()

dataAdmin3 <- 
  precipDataLong3 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total Precip Graphics By Admin 0 (country) and Admin 1 (building off admin 2) ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 0 (one line for the entire region)
annualPrecip <- 
  dataAdmin2 %>%
  group_by(year) %>%
  mutate(meanAnnualPrecip = mean(Precipitation)) %>%
  ungroup() %>%
  distinct(year, meanAnnualPrecip)

annualPrecip %>% 
  ggplot(aes(x=year, y=meanAnnualPrecip)) +
  geom_line() + 
  labs(title="Average Rainfall by Year", x="Year", y ="Average Precipitation (mm)") +
  theme_classic() 

# Calculate total admin 2 precipitation for maps (if every obs is on admin 2 basis, then show)
yearData2 <- 
  dataAdmin2 %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  mutate(total_precip = sum(Precipitation, na.rm = TRUE),
         mean_precip = mean(Precipitation, na.rm = TRUE),
         sd_precip = sd(Precipitation, na.rm = TRUE)) %>%
  distinct(admin1Name, admin1Pcod,admin2Name, admin2Pcod, year, 
           total_precip, mean_precip, sd_precip) %>%
  ungroup()

# Take the mean over the spatial area ----
yearData1 <-
  yearData2 %>% 
  group_by(admin1Name, year) %>% 
  mutate(total_precip_annual_admin1 = mean(total_precip)) %>% 
  ungroup() %>% 
  distinct(admin1Name, total_precip_annual_admin1, year)

### Generate Overall Precipitation  Graphic ---- 
yearData1  %>% 
  filter(year < 2022) %>% 
  ggplot(aes(x = year, 
             y = total_precip_annual_admin1, 
             color = admin1Name, linetype = admin1Name)) +
  geom_line() +
  labs(title = "Total Precipitation (in millimeters) By Administrative Region (Admin 1)", 
       caption = "Data Source: CHIRPS",
       y = "", 
       x = "",
       color = "Region",
       linetype = "Region") +
  remove_chart_clutter +
  theme(legend.position = "right")

mean(yearData1$total_precip_annual_admin1, na.rm = TRUE)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total Precipitation Maps (Admin 2) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. check to see names for merge (in future can run "clean_names" to avoid case sensitivity)
names(yearData2)
names(nigerMap2)

# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerYearMerged2 = full_join(nigerMap2, 
                         yearData2,
                         by = "admin2Pcod")

# 3. Generate map --- 
nigerYearMerged2 %>% 
  filter(year %in% seq(2011, 2021, 1)) %>% 
  ggplot() +
  geom_sf(aes(fill = total_precip), color = "NA") +
  scale_fill_viridis_c(direction = -1) +
  labs(title="Annual Cumulative Rainfall by Department (Admin 2)", 
       caption = "Data Source: CHIRPS Pentad Precipitation dataset", 
       fill = "Precipitation (mm)" ) +
  facet_wrap(~year) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total Precipitation Maps (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yearData3 <- 
  dataAdmin3 %>% 
  group_by(adm_03, rowcacode3, year) %>%
  mutate(total_precip = sum(Precipitation, na.rm = TRUE),
         mean_precip = mean(Precipitation, na.rm = TRUE),
         sd_precip = sd(Precipitation, na.rm = TRUE)) %>%
  distinct(adm_03, rowcacode3, year, 
           total_precip, mean_precip, sd_precip) %>%
  ungroup()


# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerYearMerged3 = full_join(nigerMap3, 
                            yearData3,
                            by = "adm_03")

# 3. Generate map --- 
nigerYearMerged3 %>% 
  filter(year %in% seq(2011, 2021, 1)) %>% 
  ggplot() +
  geom_sf(aes(fill = total_precip), color = "NA") +
  scale_fill_viridis_c(direction = -1) +
  labs(title="Annual Cumulative Rainfall by Department (Admin 3)", 
       caption = "Data Source: CHIRPS Pentad Precipitation dataset", 
       fill = "Precipitation (mm)" ) +
  facet_wrap(~year) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank()
  ) 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Scores (Admin 2) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate baseline dataset
baselineData2 <- 
  yearData2 %>% 
  group_by(admin2Pcod) %>%
  filter(year >= "1981" & year <= "2010") %>% 
  summarise(baselineMean_precip = mean(total_precip, na.rm = TRUE),
            baselineSD_precip = sd(total_precip, na.rm = TRUE)) %>%
  ungroup()

# Focus on the "post" time period
postData2 <-
  yearData2 %>% 
  group_by(admin2Name,admin2Pcod, year) %>%
  filter(year >= "2011" & year <= "2021") %>% 
  ungroup()

# Merge the two together
datajoin2 <- 
  baselineData2 %>% 
  left_join(postData2, by = "admin2Pcod") %>%
  mutate(zscore_precip = (total_precip - baselineMean_precip)/
           (baselineSD_precip))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Score Maps (Admin 2) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerZScoreMerged2 = full_join(nigerMap2, 
                               datajoin2,
                               by = "admin2Pcod")

nigerZScoreMerged2 %>% 
  filter(year == 2011 | year == 2014 | year == 2015| year == 2017 | year == 2018) %>% 
           ggplot() + 
           geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
           scale_fill_viridis_c(direction = -1) +
           facet_wrap(~year, nrow = 1) +
           labs(title="Annual Z-Score Rainfall by Department (Admin 2)", fill = "z-score" ) + 
           theme_classic() + 
           theme(axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Scores (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate baseline dataset
baselineData3 <- 
  yearData3 %>% 
  group_by(rowcacode3) %>%
  filter(year >= "1981" & year <= "2010") %>% 
  summarise(baselineMean_precip = mean(total_precip, na.rm = TRUE),
            baselineSD_precip = sd(total_precip, na.rm = TRUE)) %>%
  ungroup()

# Focus on the "post" time period
postData3 <-
  yearData3 %>% 
  group_by(adm_03,rowcacode3, year) %>%
  filter(year >= "2011" & year <= "2021") %>% 
  ungroup()

# Merge the two together
datajoin3 <- 
  baselineData3 %>% 
  left_join(postData3, by = "rowcacode3") %>%
  mutate(zscore_precip = (total_precip - baselineMean_precip)/
           (baselineSD_precip))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Score Maps (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerZScoreMerged3 = full_join(nigerMap3, 
                               datajoin3,
                               by = "rowcacode3")

nigerZScoreMerged3 %>% 
  filter(year == 2011 | year == 2014 | year == 2015| year == 2017 | year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual Z-Score Rainfall by Department (Admin 2)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalData2 <- 
  dataAdmin2 %>% 
  group_by(admin2Name, admin2Pcod, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonaltotal_precip = sum(Precipitation, na.rm = TRUE),
         seasonalmean_precip = mean(Precipitation, na.rm = TRUE)) %>%
    ungroup() %>%
  distinct(admin2Name, admin2Pcod, year, month, seasonaltotal_precip,
           seasonalmean_precip)

seasonalData3 <- 
  dataAdmin3 %>% 
  group_by(adm_03,rowcacode3, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonaltotal_precip = sum(Precipitation, na.rm = TRUE),
         seasonalmean_precip = mean(Precipitation, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(adm_03, rowcacode3, year, month, seasonaltotal_precip,
           seasonalmean_precip)
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal Data Graphics-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Merge Precip and  Spatial Information (geometry) by common code
seasonalMerged2 = full_join(nigerMap2, 
                            seasonalData2,
                            by = "admin2Pcod")

seasonalMerged2 %>% 
  filter(year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = seasonaltotal_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~month, nrow = 1) +
  labs(title="Seasonal Rainfall by Department", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#Admin 3
seasonalMerged3 = full_join(nigerMap3, 
                            seasonalData3,
                            by = "adm_03")

seasonalMerged3 %>% 
  filter(year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = seasonaltotal_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~month, nrow = 1) +
  labs(title="Seasonal Rainfall by Commune", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
