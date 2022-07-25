#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Catherine Back
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Wed Jul 20 14:33:13 2022 ------------------------------
# Date Last Updated:
# R version: 4.1.3
# Purpose: Load and Clean Precip Data Files
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries and Options -----
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
csvData2 <-read.csv("./precipDataLong2_md.csv")
csvData3 <-read.csv("./precipDataLong3_md.csv")

geospatialData2 <- st_read("./niger_admin2/niger_admin2.shp")
geospatialData3 <- st_read("./niger_admin3/NER_adm03_feb2018.shp")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Convert Column Names to Dates and create new columns -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dataAdmin2 <- 
  csvData2 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()

dataAdmin3 <- 
  csvData3 %>%
  mutate(year = year(Date), 
         month = month(Date), 
         day = day(Date)) %>%
  ungroup()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 0, Admin 1, Admin 2, and Admin 3 Data Frames ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yearAdmin0_md <- 
  dataAdmin2 %>%
  group_by(year) %>%
  mutate(medianAnnualPrecip = median(Precipitation)) %>%
  ungroup() %>%
  distinct(year, medianAnnualPrecip)

yearAdmin1_md <- 
  dataAdmin2 %>% 
  group_by(admin1Name, admin1Pcod, year) %>%
  mutate(total_precip = sum(Precipitation, na.rm = TRUE),
         median_precip = median(Precipitation, na.rm = TRUE),
         sd_precip = sd(Precipitation, na.rm = TRUE)) %>%
  distinct(admin1Name, admin1Pcod, year, 
           total_precip, median_precip, sd_precip) %>%
  ungroup()

yearAdmin2_md <-
  dataAdmin2 %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  mutate(total_precip = sum(Precipitation, na.rm = TRUE),
         median_precip = median(Precipitation, na.rm = TRUE),
         sd_precip = sd(Precipitation, na.rm = TRUE)) %>%
  distinct(admin2Name, admin2Pcod, year, 
           total_precip, median_precip, sd_precip) %>%
  ungroup()

yearAdmin3_md <-
  dataAdmin3 %>% 
  group_by(adm_03, rowcacode3, year) %>%
  mutate(total_precip = sum(Precipitation, na.rm = TRUE),
         median_precip = median(Precipitation, na.rm = TRUE),
         sd_precip = sd(Precipitation, na.rm = TRUE)) %>%
  distinct(adm_03, rowcacode3, year, 
           total_precip, median_precip, sd_precip) %>%
  ungroup()


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 2 and Admin 3 Seasonal Data Frames ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalAdmin2_md <- 
  dataAdmin2 %>% 
  group_by(admin2Name, admin2Pcod, year, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonaltotal_precip = sum(Precipitation, na.rm = TRUE),
         seasonalmedian_precip = median(Precipitation, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(admin2Name, admin2Pcod, year, month, seasonaltotal_precip,
           seasonalmedian_precip)

seasonalAdmin3_md <- 
  dataAdmin3 %>% 
  group_by(adm_03, rowcacode3, year, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonaltotal_precip = sum(Precipitation, na.rm = TRUE),
         seasonalmedian_precip = median(Precipitation, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(adm_03, rowcacode3, year, month, seasonaltotal_precip,
           seasonalmedian_precip)

### to download the dataframe as csv file
write.csv(seasonalAdmin2_md, "./seasonalData2_md.csv", row.names = FALSE)
write.csv(seasonalAdmin3_md, "./seasonalData3_md.csv", row.names = FALSE)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Scores Seasonal Annual Data Frames (Admin 2 & Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalAnnualAdmin2_md <- 
  seasonalAdmin2_md %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  mutate(yeartotal_precip = sum(seasonaltotal_precip, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(admin2Name, admin2Pcod, year, yeartotal_precip)


seasonalAnnualAdmin3_md <- 
  seasonalAdmin3_md %>% 
  group_by(adm_03, rowcacode3, year) %>%
  mutate(yeartotal_precip = sum(seasonaltotal_precip, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(adm_03, rowcacode3, year, yeartotal_precip)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal Z-Score Data Frames (Admin 2 and Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalZscore2_md <- 
  seasonalAdmin2_md %>% 
  select(admin2Name, admin2Pcod, year, month, seasonaltotal_precip) %>%
  group_by(admin2Pcod) %>% 
  mutate(baselineMean_precip = mean(seasonaltotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(seasonaltotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Pcod, year, month) %>% 
  mutate(zscore_precip = (seasonaltotal_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()

seasonalZscoreAnnual2_md <- 
  seasonalAnnualAdmin2_md %>% 
  select(admin2Name, admin2Pcod, year, yeartotal_precip) %>%
  group_by(admin2Pcod) %>% 
  mutate(baselineMean_precip = mean(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_precip = (yeartotal_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()
 
#Admin 3
seasonalZscore3_md <- 
  seasonalAdmin3_md %>% 
  select(adm_03, rowcacode3, year, month, seasonaltotal_precip) %>%
  group_by(rowcacode3) %>% 
  mutate(baselineMean_precip = mean(seasonaltotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(seasonaltotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(rowcacode3, year, month) %>% 
  mutate(zscore_precip = (seasonaltotal_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()

seasonalZscoreAnnual3_md <- 
  seasonalAnnualAdmin3_md %>% 
  select(adm_03, rowcacode3, year, yeartotal_precip) %>%
  group_by(rowcacode3) %>% 
  mutate(baselineMean_precip = mean(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(rowcacode3, year) %>% 
  mutate(zscore_precip = (yeartotal_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total Precip Graphic By Admin 0 ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yearAdmin1_md  %>% 
  ggplot(aes(x = year, 
             y = median_precip, 
             color = admin1Name, linetype = admin1Name)) +
  geom_line() +
  labs(title = "Total Precipitation (mm) By Administrative Region (Admin 1)", 
       caption = "Data Source: CHIRPS",
       y = "", 
       x = "",
       color = "Region",
       linetype = "Region") +
  remove_chart_clutter +
  theme(legend.position = "right")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Scores Graphics (Admin 2 & Admin 3) ----- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nigerZScoreMerged2 = full_join(geospatialData2, 
                               zScoreAdmin2_md,
                               by = "admin2Pcod")

nigerZScoreMerged2 %>% 
  filter(year == 2011 | year == 2014| year == 2018) %>% 
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


nigerZScoreMerged3 = full_join(geospatialData3, 
                               zScoreAdmin3_md,
                               by = "rowcacode3")

nigerZScoreMerged3 %>% 
  filter(year == 2011 | year == 2014| year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual Z-Score Rainfall by Department (Admin 3)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal Data Graphics-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalMerged2 = full_join(geospatialData2, 
                            seasonalData2,
                            by = "admin2Pcod")

seasonalMerged2 %>% 
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
                            by = "rowcacode3")

seasonalMerged3 %>% 
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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalZScore_Merged2_md = full_join(geospatialData2, 
                                   seasonalZscore2_md,
                                   by = "admin2Pcod")

seasonalZScore_Merged2_md %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~month, nrow = 1) +
  labs(title="Seasonal Rainfall Z-Score by Department Year 2018 (median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

seasonalZScore_Merged3_md = full_join(geospatialData3, 
                                      seasonalZscore3_md,
                                      by = "rowcacode3")

seasonalZScore_Merged3_md %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~month, nrow = 1) +
  labs(title="Seasonal Rainfall Z-Score by Department (median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal Annual Z-Score Data Graphics-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalZScore_Merged2_md = full_join(geospatialData2, 
                                   seasonalZscoreAnnual2_md,
                                   by = "admin2Pcod")

seasonalZScore_Merged2_md %>% 
  filter(year %in% seq(2011, 2021, 1)) %>%
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year) +
  labs(title="Annual Total of Seasonal Rainfall Z-Score by Department", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())





