#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Catherine Back and Elinor Benami
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Thu Jun 30 08:26:20 2022 ------------------------------
# Date Last Updated:
# R version: 4.1.3
# Purpose: Load and Clean Precip Data Files
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(tidylog)

library(openxlsx)
library(readxl)

library(readr)
library(tidyr)
library(lubridate)

library(ggplot2)
library(ggthemes)
library(stringr)
library(sf)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clear Environment and Set Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
csvData2 <-read.csv("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/rainfallMean_ts_niger_adm2.csv")
csvData3 <-read.csv("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/rainfallMean_ts_niger_adm3.csv")

geospatialData2 <- st_read("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/niger_admin2/niger_admin2.shp")
geospatialData3 <- st_read("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/niger_admin3/NER_adm03_feb2018.shp")

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
precipData2 <- 
  csvData2 %>%
  select(admin1Name, admin1Pcod, 
         admin2Name, admin2Pcod, "19810101": "20220426")

precipDataLong2 <- 
  pivot_longer(precipData2, 
         cols = "19810101": "20220426", 
         names_to = "Date", 
         values_to = "Precipitation") %>% 
  mutate(Date = ymd(Date))


precipData3 <- 
  csvData3 %>%
  select(adm_03, rowcacode3, "19810101": "20220426")

precipDataLong3 <- 
  pivot_longer(precipData3, 
               cols = "19810101": "20220426", 
               names_to = "Date", 
               values_to = "Precipitation") %>% 
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

#Average rainfall per year in Niger
mean(yearData1$total_precip_annual_admin1, na.rm = TRUE)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total Precipitation Maps (Admin 2) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerYearMerged2 = full_join(geospatialData2, 
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


nigerYearMerged3 = full_join(geospatialData3, 
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
# Generate Z-Scores (Admin 2 & Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
admin2precip <- 
  yearData2 %>% 
  select(admin2Name, admin2Pcod, year, total_precip) %>%
  group_by(admin2Pcod) %>% 
  mutate(baselineMean_precip = mean(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_precip = (total_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()


admin3precip <- 
  yearData3 %>% 
  select(adm_03, rowcacode3 , year, total_precip) %>%
  group_by(adm_03) %>% 
  mutate(baselineMean_precip = mean(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(adm_03, year) %>% 
  mutate(zscore_precip = (total_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Score Maps (Admin 2) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nigerZScoreMerged2 = full_join(geospatialData2, 
                               admin2precip,
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
# Generate Z-Score Maps (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nigerZScoreMerged3 = full_join(geospatialData3, 
                               admin3precip,
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
  group_by(admin2Name, admin2Pcod,year, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonaltotal_precip = sum(Precipitation, na.rm = TRUE),
         seasonalmean_precip = mean(Precipitation, na.rm = TRUE)) %>%
    ungroup() %>%
  distinct(admin2Name, admin2Pcod, year, month, seasonaltotal_precip,
           seasonalmean_precip)

seasonalData3 <- 
  dataAdmin3 %>% 
  group_by(adm_03,rowcacode3, year,month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonaltotal_precip = sum(Precipitation, na.rm = TRUE),
         seasonalmean_precip = mean(Precipitation, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(adm_03, rowcacode3, year, month, seasonaltotal_precip,
           seasonalmean_precip)
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal Data Graphics-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalMerged2 = full_join(geospatialData2, 
                            seasonalData2,
                            by = "admin2Pcod")

seasonalMerged2 %>% 
  filter(year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = seasonaltotal_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~month, nrow = 1) +
  labs(title="Seasonal Rainfall by Department Year 2018", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#Admin 3
seasonalMerged3 = full_join(geospatialData3, 
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

