#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Catherine Back
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Wed Jul 20 14:33:13 2022 ------------------------------
# Date Last Updated:
# R version: 4.1.3
# Purpose: Median Precipitation Z-Score Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(sf)
library(lubridate)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
csvData2 <-read.csv("./dataAdmin2_md.csv")
csvData3 <-read.csv("./dataAdmin3_md.csv")

geospatialData2 <- st_read("./niger_admin2/niger_admin2.shp")
geospatialData3 <- st_read("./niger_admin3/NER_adm03_feb2018.shp")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 2 and Admin 3 Data Frames ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
yearAdmin2_md <-
  csvData2 %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  mutate(total_precip = sum(Precipitation, na.rm = TRUE),
         median_precip = median(Precipitation, na.rm = TRUE),
         sd_precip = sd(Precipitation, na.rm = TRUE)) %>%
  distinct(admin2Name, admin2Pcod, year, 
           total_precip, median_precip, sd_precip) %>%
  ungroup()

yearAdmin3_md <-
  csvData3 %>% 
  group_by(adm_03, rowcacode3, year) %>%
  mutate(total_precip = sum(Precipitation, na.rm = TRUE),
         median_precip = median(Precipitation, na.rm = TRUE),
         sd_precip = sd(Precipitation, na.rm = TRUE)) %>%
  distinct(adm_03, rowcacode3, year, 
           total_precip, median_precip, sd_precip) %>%
  ungroup()

### to download the dataframe as csv file
write.csv(yearAdmin2_md, "./yearAdmin2_md.csv", row.names = FALSE)
write.csv(yearAdmin3_md, "./yearAdmin3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Seasonal Data Frames (Admin 2 and 3)---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalAdmin2_md <- 
  csvData2 %>% 
  group_by(admin2Name, admin2Pcod, year, month) %>%
  filter(month >= 6 & month <= 9) %>%
  mutate(seasonaltotal_precip = sum(Precipitation, na.rm = TRUE),
         seasonalmedian_precip = median(Precipitation, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(admin2Name, admin2Pcod, year, month, seasonaltotal_precip,
           seasonalmedian_precip)

seasonalAdmin3_md <- 
  csvData3 %>% 
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
# Generate Seasonal by Year Data Frames (Admin 2 & Admin 3) -----
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

### to download the dataframe as csv file
write.csv(seasonalAnnualAdmin2_md, "./seasonalAnnualAdmin2_md.csv", row.names = FALSE)
write.csv(seasonalAnnualAdmin3_md, "./seasonalAnnualAdmin3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Annual Z-Scores (Admin 2 & Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annualZScoreAdmin2_md <- 
  yearAdmin2_md %>% 
  select(admin2Name, admin2Pcod, year, total_precip) %>%
  group_by(admin2Pcod) %>% 
  mutate(baselineMean_precip = mean(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_precip = (total_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()


annualZScoreAdmin3_md <- 
  yearAdmin3_md %>% 
  select(adm_03, rowcacode3 , year, total_precip) %>%
  group_by(adm_03) %>% 
  mutate(baselineMean_precip = mean(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(total_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(adm_03, year) %>% 
  mutate(zscore_precip = (total_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()

### to download the dataframe as csv file
write.csv(annualZScoreAdmin2_md, "./annualZScoreAdmin2_md.csv", row.names = FALSE)
write.csv(annualZScoreAdmin3_md, "./annualZScoreAdmin3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Seasonal by Year Z-Score Data Frames (Admin 2 and Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalAnnualZscore2_md <- 
  seasonalAnnualAdmin2_md %>% 
  select(admin2Name, admin2Pcod, year, yeartotal_precip) %>%
  group_by(admin2Pcod) %>% 
  mutate(baselineMean_precip = mean(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(admin2Pcod, year) %>% 
  mutate(zscore_precip = (yeartotal_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()
 
seasonalAnnualZscore3_md <- 
  seasonalAnnualAdmin3_md %>% 
  select(adm_03, rowcacode3, year, yeartotal_precip) %>%
  group_by(rowcacode3) %>% 
  mutate(baselineMean_precip = mean(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE), 
         baselineSD_precip = sd(yeartotal_precip[year >= "1981" & year <= "2010"], na.rm = TRUE)) %>% 
  group_by(rowcacode3, year) %>% 
  mutate(zscore_precip = (yeartotal_precip - baselineMean_precip)/(baselineSD_precip)) %>% 
  ungroup()

### to download the dataframe as csv file
write.csv(seasonalAnnualZscore3_md, "./seasonalAnnualZscore2_md.csv", row.names = FALSE)
write.csv(seasonalAnnualZscore3_md, "./seasonalAnnualZscore3_md.csv", row.names = FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Annual Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annualZScoreMerged2_md = full_join(geospatialData2, 
                                   annualZScoreAdmin2_md,
                                   by= "admin2Pcod")

annualZScoreMerged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual Rainfall Z-Score by Department (Median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#Admin 3
annualZScoreMerged3_md = full_join(geospatialData3, 
                                   annualZScoreAdmin3_md,
                                   by = "adm_03")

annualZScoreMerged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual Rainfall Z-Score by Commune (Median)", fill = "z-score" ) + 
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

seasonalZScore_Merged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Seasonal Rainfall Z-Score by Department (Median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

#Admin 3
seasonalZScoreMerged3_md = full_join(geospatialData3, 
                                     seasonalAnnualZscore3_md,
                                      by = "adm_03")

seasonalZScoreMerged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Seasonal Rainfall Z-Score by Commune (Median)", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())





