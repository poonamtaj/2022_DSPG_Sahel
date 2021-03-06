##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Milind Gupta and Elinor Benami
# Project name: Sahel 
# Date Last Updated: # Wed Jun 29 22:50:28 2022 ------------------------------
# R version: 4.1.3
# Purpose: Import LSMS Data and Generate Maps of Food Insecurity
# TODO: reconsider aggregation (medians by hh first?)
# TODO: double check aggregation of households (waves already aggregated?)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clear Environment -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls(all = TRUE))

pathdata <- if(Sys.info()["user"] == "elinor"){
  "/Users/elinor/Dropbox/research/wb_climateshocks/data/"
} else{ #TODO: coauthors -- change the file here for your path
  "~/Dropbox/shared_wb_climateshocks/data/"
}
datapath <- function(x){paste0(pathdata, x)}


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
# 0. Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# On first run will need to uncomment and run if packages are not installed 
# install.packages("tidyverse")
# install.packages("janitor")
# install.packages("sf")
# install.packages("viridis")
# install.packages("ggthemes")
# install.packages("tidylog")

library(tidyverse)
library(janitor)
library(sf)
library(viridis)
library(tidylog)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Load Merged Data for Analysis (from Prior Scripts) ------ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
LSMSdata <- read_csv(datapath("processed_lsms/nigerlsms_11_14_18_slim.csv"))
niger_level2 <- st_read(datapath("geospatial/wb_niger_admin2_shapefile/niger_admin2.shp"))

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

lsms_admin2 <- 
  st_join(niger_level2, lsms_sf, left = TRUE) %>% 
  select(-c(OBJECTID, admin0Name, admin0Pcod, hhid, hhnum_percluster, X, int_year, int_month, passage))

# C. Generate median values by geometry ---- 
# these lines may take a couple of minutes to run 
start_time <- Sys.time()
lsms_median <-
  lsms_admin2 %>%
  # clusters refer to one consistent enumeration area
  # there many be many enumeration areas per administrative region, so use the admin region instead
  group_by(admin2Name, year) %>%
  # this should be no more than 67 observations per year, as we're working at admin2 (it's 56 per year)
  mutate(median_foodexpend = median(pcexp_food),
         log_median_pc_food = log(median_foodexpend),
         median_totalexpend = median(pcexp),
         median_nonfoodexp = median(pcexp_nonfood)
       ) %>%
  ungroup() %>% 
  distinct(admin2Name, year, median_foodexpend, log_median_pc_food, 
           median_totalexpend, median_nonfoodexp, 
           geometry)

# Develop check on number of units
nrow(lsms_median) < 67*3

end_time <- Sys.time()
end_time - start_time
# Time difference of 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Generate Maps ------ 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. Median Annual PC Food Expenditure ---- 
niger_level2 %>%
  ggplot() +
  geom_sf(color = "NA") +
  geom_sf(data = na.omit(lsms_median), #na.omit gets rid of the NA facet
          size = 1, 
          color = "NA",
          aes(fill = median_foodexpend/100000), alpha = 1) + #alpha indicates transparency
  facet_wrap(~year, nrow = 1) +
  labs(title = "Median Annual Per Capita Food Expenditure by Département (Admin 2)", 
       subtitle = "Units in 100,000 West African Francs, with 400-600 Francs to 1 USD",
       caption = paste0("Gray indicates no observations\n",
                        "Data Source: LSMS")) + 
  coord_sf() +
  scale_fill_viridis(name = "Annual Median West African CFA Francs (100k)") +
  theme_classic() +
  theme(legend.position="bottom")


# B. Median Annual PC Nonfood Expenditure ---- 
niger_level2 %>%
  ggplot() +
  geom_sf(color = "NA") +
  geom_sf(data = na.omit(lsms_median), #na.omit gets rid of the NA facet
          size = 1, 
          color = "NA",
          aes(fill = median_nonfoodexp/100000), alpha = 1) + #alpha indicates transparency
  facet_wrap(~year, nrow = 1) +
  labs(title = "Median Annual Per Capita Nonfood Expenditure by Département (Admin 2)", 
       subtitle = "Units in 100,000 West African Francs, with 400-600 Francs to 1 USD",
       caption = paste0("Gray indicates no observations\n",
                        "Data Source: LSMS")) + 
  coord_sf() +
  scale_fill_viridis(name = "Annual Median West African CFA Francs (100k)") +
  theme_classic() +
  theme(legend.position="bottom")


# C. Share of Food Expenditure Relative to the Whole ---- 
# TODO: verify this mathematically is correct 
# (may need to reconsider first doing household level shares then taking the median of those)
niger_level2 %>%
  ggplot() +
  geom_sf(color = "NA") +
  geom_sf(data = na.omit(lsms_median), #na.omit gets rid of the NA facet
          size = 1, 
          color = "NA",
          aes(fill = median_foodexpend/(median_nonfoodexp+median_foodexpend)*100), 
              alpha = 1) + #alpha indicates transparency
  facet_wrap(~year, nrow = 1) +
  labs(title = "Share of Food Expenditure Relative to Total Expenditure by Département (Admin 2)", 
       subtitle = "",
       caption = paste0("Gray indicates no observations\n",
                        "Data Source: LSMS")) + 
  coord_sf() +
  scale_fill_viridis(name = "Share Per Capita Food Expenditure") +
  theme_classic() +
  theme(legend.position="bottom")
## TODO: Double Check aggregation and why some values are so extreme --- 

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. For Discussion -- Why Some Values are Extreme, and Where Aggregation Happened
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Indicate the distribution of values over time
LSMSdata %>% 
  ggplot(aes(x = pcexp_food/pcexp)) +
  geom_histogram() +
  facet_wrap(~year) +
  labs(title = "Food Expenditure Relative to Total Expenditure by Observation") +
  remove_chart_clutter +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))


# first aggregate by household and survey waves to ensure have totals
LSMSdata <- 
  LSMSdata %>% 
  group_by(hhid, year) %>% 
  mutate(total_pc_exp = sum(pcexp, na.rm = TRUE), 
         food_pc_exp = sum(pcexp_food, na.rm = TRUE),
         nonfood_pc_exp = sum(pcexp_nonfood, na.rm = TRUE)) %>% 
  ungroup()

LSMSdata %>% 
  ggplot(aes(x = food_pc_exp/total_pc_exp)) +
  geom_histogram() +
  facet_wrap(~year) +
  labs(title = "Food Expenditure Relative to Total Expenditure by HHID") +
  remove_chart_clutter +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# set to origin
