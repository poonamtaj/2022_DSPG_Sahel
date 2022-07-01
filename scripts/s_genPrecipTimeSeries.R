#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Catherine Back and Elinor Benami
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Thu Jun 30 08:26:20 2022 ------------------------------
# Date Last Updated:
# R version: 4.1.3
# Purpose: Load and Clean Precip Data Files
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Overall notes to consider  -----
# develop tests to having "sanity" check of data (should rainfall be above X? if not, go back and review)
# Add in all datasets  & libraries up front
# Add headers with the 4 dashes at end to be able to have a seciton appear in the  lines so as to be 
# Consider plain vanilla R scripts (Rmd Can be really nice for explortatory notebooks, but they can also be wonky, take time to compile, and eat space with chunk headers)
# plain vanilla .R scripts are often easier to export from (it'll take parameters from your plot window, for example). 
# you can also batch .R scripts together.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clear Environment and Set Options -----
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

# Reconsider if all these are necessary 
# library(dplyr)
# library(moderndive)
# library(readr)
# library(tidyr)
# library(tidyverse)
# library(lubridate)
# library(reshape2)
# library(ggplot2)
# library(sf)
# library(sp)
# library(rgdal)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Replace datapath filepath with appropriate 
pentadDataadmin2 <- read_csv(datapath("chirps/rainfallMean_ts_niger_adm2.csv")) 
pentadDataadmin3 <- read_csv(datapath("chirps/rainfallMean_ts_niger_adm3.csv"))

# geospatial
nigerMapAdmin2 <- 
  st_read(datapath("geospatial/wb_niger_admin2_shapefile/niger_admin2.shp")) %>% 
  mutate(dept_clean = make_clean_names(admin2Name)) %>%
  clean_names()  %>% 
  # may not be necessary -- double check
  mutate(dept_clean = case_when(dept_clean == "ingall" ~ "in_gall", 
                                dept_clean == "n_gourti" ~ "ngourti", 
                                dept_clean == "guidan_roumdji" ~ "guidan_roumji", 
                                dept_clean == "ville_de_niamey" ~ "niamey" , 
                                dept_clean == "takeita" ~ "takieta", 
                                TRUE ~ dept_clean))

# nigerMapAdmin3 <- 
#   st_read(datapath("geospatial/wb_niger_admin2_shapefile/niger_admin3.shp")) %>% 
#   mutate(dept_clean = make_clean_names(admin2Name)) %>%
#   clean_names()  

# Don't show scientific notation
options(scipen = 999)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean titles and data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Remove the xfrom before the colname and and precip after the date from the colnames --- 
# TODO: revisit if necessary (R doesn't like having columns that start with numbers 
# Can also reshape/pivot_longer first and then do a str_extact for the dates up front. 
# Points for creativiity! Where possible I do tend to avoid for-loops and instead opt for vector operations
# see more on that here: https://bookdown.org/rdpeng/rprogdatascience/vectorized-operations.html
for ( col in 1:ncol(pentadDataadmin2)){
  colnames(pentadDataadmin2)[col] <- sub("_precipitation", "", colnames(pentadDataadmin2)[col])
}
for ( col in 1:ncol(pentadDataadmin2)){
  colnames(pentadDataadmin2)[col] <- sub("X", "", colnames(pentadDataadmin2)[col])
}

for ( col in 1:ncol(pentadDataadmin3)){
  colnames(pentadDataadmin3)[col] <- sub("_precipitation", "", colnames(pentadDataadmin3)[col])
}
for ( col in 1:ncol(pentadDataadmin3)){
  colnames(pentadDataadmin3)[col] <- sub("X", "", colnames(pentadDataadmin3)[col])
}
head(pentadDataadmin3)

# Check how many we have and view to check ---- 
dim(pentadDataadmin2)
show <- pentadDataadmin2 %>% select(2970:2987)
glimpse(show)

dim(pentadDataadmin3)
pentadDataadmin2 %>% select(2990-10: 2990)

# Convert data from Wide to Long ---- 
precipitationData2 <- 
  pentadDataadmin2 %>%
  # select(admin2Name, admin2Pcod, "19810101": "20220121")
  select(admin1Name, admin1Pcod, 
         admin2Name, admin2Pcod, "19810101": "20220426") #if had x, could do starts_with("x_"). # also checked to end of dataset

precipitationDataLong2 <- 
  gather(precipitationData2, 
         key = Date, 
         value = Precipitation, 
         "19810101":"20220121") %>% 
  mutate(Date = ymd(Date))
         # "19810101":"20220121") 

precipitationData3 <-
  pentadDataadmin3 %>%
  select(adm_03, rowcacode3,  ##EB did not run admin3, need to check -- could make parallel
         "19810101":"20220121")

precipitationDataLong3 <-
  gather(precipitationData3,
         Date,
         Precipitation,
         "19810101":"20220121") %>% 
  mutate(Date = ymd(Date))

# # Convert Characters to Dates, then create columns 
# precipitationDataLong2$Date <- ymd(precipitationDataLong2$Date)
# precipitationDataLong3$Date <- ymd(precipitationDataLong3$Date)

# if you're using dplyr, don't need to repeat the df name
# data_admin2 <- 
#   precipitationDataLong2 %>%
#   dplyr::mutate(year = lubridate::year(precipitationDataLong2$Date), 
#                 month = lubridate::month(precipitationDataLong2$Date), 
#                 day = lubridate::day(precipitationDataLong2$Date)) %>%
#   ungroup()

data_admin2 <- 
  precipitationDataLong2 %>%
  mutate(year = year(Date), 
                month = month(Date), 
                day = day(Date)) %>%
  ungroup()

# data_admin3 <- precipitationDataLong3 %>%
#   dplyr::mutate(year = lubridate::year(precipitationDataLong3$Date), 
#                 month = lubridate::month(precipitationDataLong3$Date), 
#                 day = lubridate::day(precipitationDataLong3$Date)) %>%
#   ungroup()


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total Precip Graphics By Admin 0 (country) and Admin 1 (building off admin 2) ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Admin 0 (one line for the entire region)
annualPrecip <- 
  yearData_admin2 %>%
  group_by(year) %>%
  mutate(meanAnnualPrecip = mean(total_precipitation)) %>%
  ungroup() %>%
  distinct(year, meanAnnualPrecip)

annualPrecip %>% 
  ggplot(aes(x=year, y=meanAnnualPrecip)) +
  geom_line() + 
  labs(title="Average Rainfall by Year", x="Year", y ="Average Precipitation (mm)") +
  theme_classic() 

# Calculate total admin 2 precipitation for maps (if every obs is on admin 2 basis, then show)
yearData_admin2 <- 
  data_admin2 %>% 
  group_by(admin2Name, admin2Pcod, year) %>%
  # filter(year >= "1981" & year <= "2021") %>% # not necesary (data is already constrained here)
  mutate(total_precipitation = sum(Precipitation, na.rm = TRUE),
         mean_precipitation = mean(Precipitation, na.rm = TRUE),
         standardDeviation_precipitation = sd(Precipitation, na.rm = TRUE)) %>%
  # distinct(total_precipitation, .keep_all = TRUE) %>% # keep all is likely more info than what we need (defeats point of distinct)
  distinct(admin1Name, admin1Pcod,admin2Name, admin2Pcod, year, 
           total_precipitation, mean_precipitation, standardDeviation_precipitation) %>%
  ungroup()

# Take the mean over the spatial area ----
yearData_admin1 <-
  yearData_admin2 %>% 
  group_by(admin1Name, year) %>% 
  mutate(total_precip_annual_admin1 = mean(total_precipitation)) %>% 
  ungroup() %>% 
  distinct(admin1Name, total_precip_annual_admin1, year)

### Generate Overall Precipitation  Graphic ---- 
yearData_admin1  %>% 
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

mean(yearData_admin1$total_precip_annual_admin1, na.rm = TRUE)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Total Precipitation Maps (Admin 2) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. check to see names for merge (in future can run "clean_names" to avoid case sensitivity)
names(yearData_admin2)
names(nigerMapAdmin2)

# 2. Merge Precip and  Spatial Information (geometry) by common code
nigerShpMerged_admin2 = full_join(nigerMapAdmin2, 
                                  yearData_admin2,
                                  by=c("admin2pcod" = "admin2Pcod"))

# 3. Generate map --- 
nigerShpMerged_admin2 %>% 
  filter(year %in% seq(2011, 2021, 1)) %>% 
  ggplot() +
  geom_sf(aes(fill = total_precipitation), color = "NA") + # important that the color = "NA" comes in this part of the call
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

# 2. Merge Precip and  Spatial Information (geometry) by common code
# nigerShpMerged_admin3= full_join(nigerMapAdmin3, 
#                                   yearData_admin3,
#                                   by=c("admin2pcod" = "admin2Pcod"))


# nigerShpMerged_admin3 %>% 
#   filter(year %in% seq(2011, 2021, 1)) %>% 
#   ggplot() +
#   geom_sf(aes(fill = total_precipitation), color = "NA") + # important that the color = "NA" comes in this part of the call
#   scale_fill_viridis_c(direction = -1) +
#   labs(title="Annual Cumulative Rainfall by Commune (Admin 3)", 
#        caption = "Data Source: CHIRPS Pentad Precipitation dataset", 
#        fill = "Precipitation (mm)" ) +
#   facet_wrap(~year) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     rect = element_blank()
#   ) 


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Scores (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate baseline dataset
baselineData_admin2 <- 
  yearData_admin2 %>% 
  # group_by(admin2Name, admin2Pcod) %>% # one of these two should be sufficient
  group_by(admin2Name, admin2Pcod) %>%
  filter(year >= "1981" & year <= "2010") %>% 
  summarise(baselineMean_precipitation = mean(total_precipitation, na.rm = TRUE),
            baselineSD_precipitation = sd(total_precipitation, na.rm = TRUE)) %>%
  ungroup()

# Focus on the "post" time period
periodData_admin2 <-
  yearData_admin2 %>% 
  group_by(admin2Name,admin2Pcod, year) %>%
  filter(year >= "2011" & year <= "2021") %>% 
  ungroup()

# Merge the two together
datajoin_admin2 <- 
  baselineData_admin2 %>% 
  left_join(periodData_admin2, by = "admin2Pcod") %>%
  mutate(zscore_precipitation = (total_precipitation - baselineMean_precipitation)/
                                   (baselineSD_precipitation))

# Check the distribution of z-scores (seem reasonable?)
datajoin_admin2  %>% tabyl(zscore_precipitation)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate Z-Score Maps (Admin 3) -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nigerShpMerged_admin2 %>% 
  filter(year %in% c(2011, 2014, 2015, 2017, 2018) %>% 
           # this is more parsimious, though definitely the other version works, too!
  # filter(year == 2011 | year == 2014 | year == 2015| year == 2017 | year == 2018) %>%
  ggplot() + 
  geom_sf(aes(fill = zscore_precipitation),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual Z-Score Rainfall by Department (Admin 2)", fill = "z-score" ) + 
    theme_classic() + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank()))


nigerShpMerged_admin3 %>% 
  filter(year %in% c(2011, 2014, 2015, 2017, 2018) %>% 
           # this is more parsimious, though definitely the other version works, too!
           # filter(year == 2011 | year == 2014 | year == 2015| year == 2017 | year == 2018) %>%
           ggplot() + 
           geom_sf(aes(fill = zscore_precipitation),color = NA, alpha = 0.8) +
           scale_fill_viridis_c(direction = -1) +
           facet_wrap(~year, nrow = 1) +
           labs(title="Annual Z-Score Rainfall by  Commune (Admin 3)", 
                fill = "z-score" ) + 
           theme_classic() + 
           theme(axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 rect = element_blank()))







# yearData_admin3 <- data_admin3 %>% 
#   group_by(adm_03, rowcacode3, year) %>%
#   filter(year >= "1981" & year <= "2021") %>%
#   mutate(total_precipitation = sum(Precipitation, na.rm = TRUE),
#          mean_precipitation = mean(Precipitation, na.rm = TRUE),
#          standardDeviation_precipitation = sd(Precipitation, na.rm = TRUE)) %>%
#   distinct(total_precipitation, .keep_all = TRUE) %>%
#   ungroup()

# All Years 3 Regions

#Desert = (yearData_admin2$admin1Name == "Agadez"|yearData_admin2$admin1Name == "Diffa"|yearData_admin2$admin1Name == "Zinder")
#agroPastoral = (yearData_admin2$admin1Name == "Tahoua"|yearData_admin2$admin1Name == "Maradi")
#rainfed = (yearData_admin2$admin1Name == "Tillabery" | yearData_admin2$admin1Name == "Dosso"|| yearData_admin2$admin1Name == "Niamey")

# Filtered 1981 to 2010 Baseline Data ----

baselineData_admin2 <- yearData_admin2 %>% 
  group_by(admin2Name, admin2Pcod) %>%
  filter(year >= "1981" & year <= "2010") %>% 
  summarise(baselineTotal_precipitation = sum(total_precipitation, na.rm = TRUE),
            baselineMean_precipitation = mean(total_precipitation, na.rm = TRUE),
            baselineSD_precipitation = sd(total_precipitation, na.rm = TRUE)) %>%
  ungroup()

baselineData_admin3 <- yearData_admin3 %>% 
  group_by(adm_03, rowcacode3) %>%
  filter(year >= "1981" & year <= "2010") %>% 
  summarise(baselineTotal_precipitation = sum(total_precipitation, na.rm = TRUE),
            baselineMean_precipitation = mean(total_precipitation, na.rm = TRUE),
            baselineSD_precipitation = sd(total_precipitation, na.rm = TRUE)) %>%
  ungroup()


# 2011 to 2020 Filtered with baseline z-score
```{r}
periodData_admin2 <- yearData_admin2 %>% 
  group_by(admin2Name,admin2Pcod, year) %>%
  filter(year >= "2011" & year <= "2021") %>% 
  ungroup()



datajoin_admin2 = merge(x = baselineData_admin2, y = periodData_admin2, by = "admin2Pcod") %>%
  mutate(zscore_precipitation = ((total_precipitation - baselineMean_precipitation)/(baselineSD_precipitation)))




periodData_admin3 <- yearData_admin3 %>% 
  group_by(adm_03,rowcacode3, year) %>%
  filter(year >= "2011" & year <= "2021") %>% 
  ungroup()



datajoin_admin3 = merge(x = baselineData_admin3, y = periodData_admin3, by = "rowcacode3") %>%
  mutate(zscore_precipitation = ((total_precipitation - baselineMean_precipitation)/(baselineSD_precipitation)))
```



Total Precipitation by Month Summary
```{r}
monthlyData_admin2 <- data_admin2 %>% 
  group_by(admin2Name,admin2Pcod, month) %>%
  filter(year == "2020") %>% 
  mutate(monthlyTotal_precipitation = sum(Precipitation, na.rm = TRUE),
         monthlyMean_precipitation = mean(Precipitation, na.rm = TRUE)) %>%
  ungroup()




monthlyData_admin3 <- data_admin3 %>% 
  group_by(adm_03,rowcacode3, month) %>%
  filter(year == "2020") %>% 
  mutate(monthlyTotal_precipitation = sum(Precipitation, na.rm = TRUE),
         monthlyMean_precipitation = mean(Precipitation, na.rm = TRUE)) %>%
  ungroup()
```



Seasonal Precipitation Summary
```{r}
monthlyData_admin2 <- data_admin2 %>% 
  group_by(admin2Name,admin2Pcod, month) %>%
  filter(month >= 5 & month <= 10) %>% 
  mutate(monthlyTotal_precipitation = sum(Precipitation, na.rm = TRUE),
         monthlyMean_precipitation = mean(Precipitation, na.rm = TRUE)) %>%
  ungroup()




monthlyData_admin3 <- data_admin3 %>% 
  group_by(adm_03,rowcacode3, month) %>%
  filter(month >= 5 & month <= 10) %>% 
  mutate(monthlyTotal_precipitation = sum(Precipitation, na.rm = TRUE),
         monthlyMean_precipitation = mean(Precipitation, na.rm = TRUE)) %>%
  ungroup()
```





#Visuals




#Bring in Shape Files # bring up front 
```{r}
nigerMapAdmin2 <- st_read("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/niger_admin2/niger_admin2.shp")
nigerMapAdmin3 <- st_read("C:/Users/Catherine/OneDrive/Documents/2022_DSPG_Sahel/niger_admin3/NER_adm03_feb2018.shp")
```



Merge Data with Shape Files
```{r}
nigerShpMerged_admin2 = full_join(nigerMapAdmin2, datajoin_admin2 ,by="admin2Pcod")



nigerShpMerged_admin3 = full_join(nigerMapAdmin3, datajoin_admin3 ,by="rowcacode3")
```



Total Precipitation Admin 2 and Admin 3 Maps + Bar Graph
```{r}
ggplot(nigerShpMerged_admin2) + theme_classic() + theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank()) +
  geom_sf(aes(fill = total_precipitation), color = NA, alpha = 0.8) +
  scale_fill_viridis_c( direction = -1) +
  labs(title="Annual Cumulative Rainfall by Department (Admin 2)", caption = "Data Source: CHIRPS Pentad Precipitation dataset", fill = "Precipitation (mm)" ) +
  facet_wrap(~year)



ggplot(nigerShpMerged_admin3) + theme_classic() + theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank()) +
  geom_sf(aes(fill = total_precipitation),color = NA, alpha = 0.8) +
  scale_fill_viridis_c( direction = -1) +
  labs(title="Annual Cumulative Rainfall by Commune (Admin 3)", caption = "Data Source: CHIRPS Pentad Precipitation dataset", fill = "Precipitation (mm)" ) +
  facet_wrap(~year)

```




z-score Precipitation Admin 2 and Admin 3
```{r}
#Admin 2 Slide 10
nigerShpMerged_admin2 %>% 
  filter(year == 2011 | year == 2014 | year == 2015| year == 2017 | year == 2018) %>%
  ggplot() + theme_classic() + theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank()) +
  geom_sf(aes(fill = zscore_precipitation),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual Z-Score Rainfall by Department (Admin 2)", fill = "z-score" )



#Admin 3 Slide 10
nigerShpMerged_admin3 %>% 
  filter(year == 2011 | year == 2014 | year == 2015| year == 2017 | year == 2018) %>%
  ggplot() + theme_classic() + theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),rect = element_blank()) +
  geom_sf(aes(fill = zscore_precipitation),color = NA, alpha = 0.8) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~year, nrow = 1) +
  labs(title="Annual Z-Score Rainfall by Commune (Admin 3)", fill = "z-score" )
```




```{r}
annualPrecip <- yearData_admin2 %>%
  group_by(year) %>%
  mutate(meanAnnualPrecip = mean(total_precipitation)) %>%
  ungroup() %>%
  distinct(year, meanAnnualPrecip)



#Region Data Slide 7
ggplot(annualPrecip, aes(x=year, y=meanAnnualPrecip, na.rm=TRUE)) +
  geom_line() + theme_classic() +
  labs(title="Average Rainfall by Year", x="Year", y="Average Precipitation (mm)")
```