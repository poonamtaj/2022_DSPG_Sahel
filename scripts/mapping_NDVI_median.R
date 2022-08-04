#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Riley Rudd & Catherine Back
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Fri July 29 2022 ------------------------------
# Date Last Updated:
# R version: 4.1.3
# Purpose: Median NDVI Z-Score Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

knitr::opts_chunk$set(
  echo = FALSE,
  cache = TRUE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(sf)
library(lubridate)

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
csvData2 <-read.csv("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/ndviDataLong2_md.csv")
csvData3 <-read.csv("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/ndviDataLong3_md.csv")

geospatialData2 <- st_read("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/wb_niger_admin2_shapefile")
geospatialData3 <- st_read("C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/ner_adm03_feb2018/NER_adm03_feb2018.shp")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 2 and Admin 3 Data Frames ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_admin2 <- 
  csvData2 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()

data_admin3 <- 
  csvData3 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 2 and Admin 3 Data Frames ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yearAdmin2_md <-
  data_admin2 %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  mutate(peak_ndvi = max(newNDVI, na.rm = TRUE),
         median_ndvi = median(newNDVI, na.rm = TRUE),
         sd_ndvi = sd(newNDVI, na.rm = TRUE)) %>%
  distinct(admin1Name, admin1Pcod, admin2Name, admin2Pcod, year, 
           peak_ndvi, median_ndvi, sd_ndvi) %>%
  ungroup()

yearAdmin3_md <-
  data_admin3 %>% 
  group_by(adm_03, rowcacode3, year) %>%
  mutate(peak_ndvi = max(newNDVI, na.rm = TRUE),
         median_ndvi = median(newNDVI, na.rm = TRUE),
         sd_ndvi = sd(newNDVI, na.rm = TRUE)) %>%
  distinct(adm_03, rowcacode3, year, 
           peak_ndvi, median_ndvi, sd_ndvi) %>%
  ungroup()

# Take the median over the spatial area ----
yearAdmin1_md <-
  yearAdmin2_md %>% 
  filter(year %in% seq(1981, 2021, 1)) %>%
  group_by(admin1Name, year) %>% 
  mutate(median_ndvi = round(median(peak_ndvi),digits = 3)) %>% 
  ungroup() %>% 
  distinct(admin1Name, median_ndvi, year)

colnames(yearAdmin1_md) <- c('Region','Year','NDVI')

### to download the dataframe as csv file
write.csv(yearAdmin1_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/yearAdmin1_md.csv", row.names = FALSE)
write.csv(yearAdmin2_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/yearAdmin2_md.csv", row.names = FALSE)
write.csv(yearAdmin3_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/yearAdmin3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Seasonal Data Frames (Admin 2 and 3)---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalAdmin2_md <- 
  data_admin2 %>% 
  group_by(admin2Name, admin2Pcod, year, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonalpeak_ndvi = max(newNDVI, na.rm = TRUE),
         seasonalmedian_ndvi = median(newNDVI, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(admin2Name, admin2Pcod, year, month, seasonalpeak_ndvi,
           seasonalmedian_ndvi)

seasonalAdmin3_md <- 
  data_admin3 %>% 
  group_by(adm_03, rowcacode3, year, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonalpeak_ndvi = max(newNDVI, na.rm = TRUE),
         seasonalmedian_ndvi = median(newNDVI, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(adm_03, rowcacode3, year, month, seasonalpeak_ndvi,
           seasonalmedian_ndvi)

### to download the dataframe as csv file
write.csv(seasonalAdmin2_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalData2_md.csv", row.names = FALSE)
write.csv(seasonalAdmin3_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalData3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal by Year Data Frames (Admin 2 & Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalAnnualAdmin2_md <- 
  seasonalAdmin2_md %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  mutate(yearpeak_ndvi = max(seasonalpeak_ndvi, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(admin2Name, admin2Pcod, year, yearpeak_ndvi)

seasonalAnnualAdmin3_md <- 
  seasonalAdmin3_md %>% 
  group_by(adm_03, rowcacode3, year) %>%
  mutate(yearpeak_ndvi = max(seasonalpeak_ndvi, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(adm_03, rowcacode3, year, yearpeak_ndvi)

### to download the dataframe as csv file
write.csv(seasonalAnnualAdmin2_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalAnnualAdmin2_md.csv", row.names = FALSE)
write.csv(seasonalAnnualAdmin3_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalAnnualAdmin3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Annual Z-Scores (Admin 2 & Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annualZScoreAdmin2_md <- 
  yearAdmin2_md %>% 
  select(admin2Name, admin2Pcod, year, peak_ndvi) %>%
  group_by(admin2Pcod) %>% 
  mutate(baselineMean_ndvi = mean(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_ndvi = sd(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_ndvi = (peak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)) %>% 
  ungroup()


annualZScoreAdmin3_md <- 
  yearAdmin3_md %>% 
  select(adm_03, rowcacode3 , year, peak_ndvi) %>%
  group_by(adm_03) %>% 
  mutate(baselineMean_ndvi = mean(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_ndvi = sd(peak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(adm_03, year) %>% 
  mutate(zscore_ndvi = (peak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)) %>% 
  ungroup()

### to download the dataframe as csv file
write.csv(annualZScoreAdmin2_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/annualZScoreAdmin2_md.csv", row.names = FALSE)
write.csv(annualZScoreAdmin3_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/annualZScoreAdmin3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal by Year Z-Score Data Frames (Admin 2 and Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalAnnualZscore2_md <- 
  seasonalAnnualAdmin2_md %>% 
  select(admin2Name, admin2Pcod, year, yearpeak_ndvi) %>%
  group_by(admin2Pcod) %>% 
  mutate(baselineMean_ndvi = mean(yearpeak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_ndvi = sd(yearpeak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_ndvi = (yearpeak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)) %>% 
  ungroup()

seasonalAnnualZscore3_md <- 
  seasonalAnnualAdmin3_md %>% 
  select(adm_03, rowcacode3, year, yearpeak_ndvi) %>%
  group_by(rowcacode3) %>% 
  mutate(baselineMean_ndvi = mean(yearpeak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_ndvi = sd(yearpeak_ndvi[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(rowcacode3, year) %>% 
  mutate(zscore_ndvi = (yearpeak_ndvi - baselineMean_ndvi)/(baselineSD_ndvi)) %>% 
  ungroup()

### to download the dataframe as csv file
write.csv(seasonalAnnualZscore2_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalAnnualZscore2_md.csv", row.names = FALSE)
write.csv(seasonalAnnualZscore3_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalAnnualZscore3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Overall ndvi  Graphic ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yearAdmin1_md  %>% 
  ggplot(aes(x = year, 
             y = median_ndvi, 
             color = admin1Name, linetype = admin1Name)) +
  geom_line() +
  labs(title = "Total ndvi (mm) By Administrative Region (Admin 1)", 
       caption = "Data Source: CHIRPS",
       y = "", 
       x = "",
       color = "Region",
       linetype = "Region") +
  remove_chart_clutter +
  theme(legend.position = "right")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Annual Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annualZScoreMerged2_md = full_join(geospatialData2, 
                                   annualZScoreAdmin2_md,
                                   by= "admin2Pcod")
write.csv(annualZScoreMerged2_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/annualZScoreMerged3_md", row.names = FALSE)


annualZScoreMerged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual NDVI Z-Score by Department (Median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#Admin 3
annualZScoreMerged3_md = full_join(geospatialData3, 
                                   annualZScoreAdmin3_md,
                                   by = "adm_03")
write.csv(annualZScoreMerged3_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/annualZScoreMerged3_md", row.names = FALSE)


annualZScoreMerged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual NDVI Z-Score by Commune (Median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal By Year Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalZScore_Merged2_md = full_join(geospatialData2, 
                                      seasonalAnnualZscore2_md,
                                      by = "admin2Pcod")
write.csv(seasonalZScore_Merged2_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalZScore_Merged2_md", row.names = FALSE)

seasonalZScore_Merged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Seasonal NDVI Z-Score by Department (Median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#Admin 3
seasonalZScoreMerged3_md = full_join(geospatialData3, 
                                     seasonalAnnualZscore3_md,
                                     by = "adm_03")

write.csv(seasonalZScoreMerged3_md, "C:/Users/riley/OneDrive/Desktop/My classes/Economics/New folder/DSPG 2022/DSPG_Sahel/Sahel Data/seasonalZScore_Merged3_md", row.names = FALSE)


seasonalZScoreMerged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Seasonal NDVI Z-Score by Commune (Median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())
