#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Catherine Back
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Wed Jul 20 14:33:13 2022 ------------------------------
# Date Last Updated:
# R version: 4.1.3
# Purpose: Combine Median and Mean Precipitation Z-Score Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(sf)
library(lubridate)
library(cowplot)
library(ggplot2)
library(ggpubr)
library(readxl)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Precipitation Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Annual Precipitation Data
annualPrecipMean_admin2 <-read.csv("./admin2precip.csv")
annualPrecipMean_admin3 <-read.csv("./admin3precip.csv")

annualPrecipMedian_admin2 <-read.csv("./annualZScoreAdmin2_md.csv")
annualPrecipMedian_admin3 <-read.csv("./annualZScoreAdmin3_md.csv")

#Seasonal Precipitation Data
seasonalPrecipMean_admin2 <-read.csv("./seasonalZscoreAnnual2.csv")
seasonalPrecipMean_admin3 <-read.csv("./seasonalZscoreAnnual3.csv")

seasonalPrecipMedian_admin2 <-read.csv("./seasonalAnnualZscore2_md.csv")
seasonalPrecipMedian_admin3 <-read.csv("./seasonalAnnualZscore3_md.csv")

#Geospatial Data
geospatialData2 <- st_read("./niger_admin2/niger_admin2.shp")
geospatialData3 <- st_read("./niger_admin3/NER_adm03_feb2018.shp")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Precipitation Annual Mean Z-Score Maps (Admin 2 & Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nigerZScoreMerged2 = full_join(geospatialData2, 
                               annualPrecipMean_admin2,
                               by = "admin2Pcod")

annualPrecipMeanDepartment <-
  nigerZScoreMerged2 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
nigerZScoreMerged3 = full_join(geospatialData3, 
                               annualPrecipMean_admin3,
                               by = "rowcacode3")

annualPrecipMeanCommune <-
  nigerZScoreMerged3 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Precipitation Seasonal Mean Z-Score Data Graphics (Admin 2 & Admin 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Admin 2
seasonalZScore_Merged2 = full_join(geospatialData2, 
                                   seasonalPrecipMean_admin2,
                                   by = "admin2Pcod")

seasonalPrecipMeanDepartment <-
  seasonalZScore_Merged2 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
seasonalZScore_Merged3 = full_join(geospatialData3, 
                                   seasonalPrecipMean_admin3,
                                   by = "rowcacode3")

seasonalPrecipMeanCommune <-
  seasonalZScore_Merged3 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Precipitation Annual Median Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annualZScoreMerged2_md = full_join(geospatialData2, 
                                   annualPrecipMedian_admin2,
                                   by= "admin2Pcod")

annualPrecipMedianDepartment <-
  annualZScoreMerged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
annualZScoreMerged3_md = full_join(geospatialData3, 
                                   annualPrecipMedian_admin3,
                                   by = "adm_03")


annualPrecipMedianCommune <-
  annualZScoreMerged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Precipitation Seasonal Median Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalZScore_Merged2_md = full_join(geospatialData2, 
                                      seasonalPrecipMedian_admin2,
                                      by = "admin2Name")


seasonalPrecipMedianDepartment <-
  seasonalZScore_Merged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
seasonalZScore_Merged3_md = full_join(geospatialData3, 
                                     seasonalPrecipMedian_admin3,
                                     by = "adm_03")


seasonalPrecipMedianCommune <-
  seasonalZScore_Merged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_precip),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Precipitation Graphics Admin2/Admin3 and Seasonal/Annual-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Annual Admin 2
ggarrange(annualPrecipMeanDepartment, annualPrecipMedianDepartment, ncol=1, common.legend = TRUE, legend="right")

#Annual Admin 3
ggarrange(annualPrecipMeanCommune, annualPrecipMedianCommune, ncol=1, common.legend = TRUE, legend="right")

#Seasonal Admin 2
ggarrange(seasonalPrecipMeanDepartment, seasonalPrecipMedianDepartment, ncol=1, common.legend = TRUE, legend="right")

#Seasonal Admin 3
ggarrange(seasonalPrecipMeanCommune, seasonalPrecipMedianCommune, ncol=1, common.legend = TRUE, legend="right")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load NDVI Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Annual Precipitation Data
annualNDVIMean_admin2 <-read_excel("./annualndvimean2.xlsx")
annualNDVIMean_admin3 <-read_excel("./annualndvimean3.xlsx")

annualNDVIMedian_admin2 <-read_excel("./annualNDVIZScoreAdmin2_md.xlsx")
annualNDVIMedian_admin3 <-read_excel("./annualNDVIZScoreAdmin3_md.xlsx")

#Seasonal Precipitation Data
seasonalNDVIMean_admin2 <-read_excel("./seasonalndvimean2.xlsx")
seasonalNDVIMean_admin3 <-read_excel("./seasonalndvimean3.xlsx")

seasonalNDVIMedian_admin2 <-read_excel("./seasonalNDVIAdmin2_md.xlsx")
seasonalNDVIMedian_admin3 <-read_excel("./seasonalNDVIZscore3_md.xlsx")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate NDVI Annual Mean Z-Score Maps (Admin 2 & Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nigerNDVIZScoreMerged2 = full_join(geospatialData2,
                                   annualNDVIMean_admin2,
                                   by = "admin2Pcod")

annualNDVIMeanDepartment <-
  nigerNDVIZScoreMerged2 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
nigerNDVIZScoreMerged3 = full_join(geospatialData3,
                                   annualNDVIMean_admin3,
                                   by = "adm_03")

annualNDVIMeanCommune <-
  nigerNDVIZScoreMerged3 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate NDVI Seasonal Mean Z-Score Data Graphics (Admin 2 & Admin 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Admin 2
seasonalNDVIZScore_Merged2 = full_join(geospatialData2,
                                       seasonalNDVIMean_admin2,
                                       by = "admin2Pcod")

seasonalNDVIMeanDepartment <-
  seasonalNDVIZScore_Merged2 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
seasonalNDVIZScore_Merged3 = full_join(geospatialData3,
                                       seasonalNDVIMean_admin3,
                                       by = "adm_03")

seasonalNDVIMeanCommune <-
  seasonalNDVIZScore_Merged3 %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Mean", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate NDVI Annual Median Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annualNDVIZScoreMerged2_md = full_join(geospatialData2,
                                       annualNDVIMedian_admin2,
                                       by= "admin2Pcod")

annualNDVIMedianDepartment <-
  annualNDVIZScoreMerged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
annualNDVIZScoreMerged3_md = full_join(geospatialData3,
                                       annualNDVIMedian_admin3,
                                       by = "adm_03")


annualNDVIMedianCommune <-
  annualNDVIZScoreMerged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate NDVI Seasonal Median Z-Score Data Graphics (Admin 2 and 3)-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
seasonalNDVIZScore_Merged2_md = full_join(geospatialData2,
                                          seasonalNDVIMedian_admin2,
                                          by = "admin2Name")


seasonalNDVIMedianDepartment <-
  seasonalNDVIZScore_Merged2_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())

#Admin 3
seasonalNDVIZScoreMerged3_md = full_join(geospatialData3,
                                         seasonalNDVIMedian_admin3,
                                         by = "adm_03")


seasonalNDVIMedianCommune <-
  seasonalNDVIZScoreMerged3_md %>% 
  filter(year == 2011|year == 2014|year == 2015|year == 2017|year == 2018) %>% 
  ggplot() + 
  geom_sf(aes(fill = zscore_ndvi),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Median", fill = "z-score" ) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        rect = element_blank())


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate NDVI Graphics Admin2/Admin3 and Seasonal/Annual-----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Annual Admin 2
ggarrange(annualNDVIMeanDepartment, annualNDVIMedianDepartment, ncol=1, common.legend = TRUE, legend="right")

#Annual Admin 3
ggarrange(annualNDVIMeanCommune, annualNDVIMedianCommune, ncol=1, common.legend = TRUE, legend="right")

#Seasonal Admin 2
ggarrange(seasonalNDVIMeanDepartment, seasonalNDVIMedianDepartment, ncol=1, common.legend = TRUE, legend="right")

#Seasonal Admin 3
ggarrange(seasonalNDVIMeanCommune, seasonalNDVIMedianCommune, ncol=1, common.legend = TRUE, legend="right")

