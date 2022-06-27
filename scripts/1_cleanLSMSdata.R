#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Elinor Benami
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Thu Jun 16 21:12:35 2022 ------------------------------
# Date Last Updated: # Tue Jun 21 11:54:57 2022 ------------------------------ (ts shift tab)
# R version: 4.1.3
# Purpose: Load and Clean Data Files Used for Analysis
# Notes: https://microdata.worldbank.org/index.php/catalog/2676
# https://microdata.worldbank.org/index.php/catalog/2050/study-description 
# https://microdata.worldbank.org/index.php/catalog/2676/related-materials
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(haven) # for read_dta
library(tidylog) # for output on operations/changes with select, drop, merges, etc.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Replace datapath filepath with appropriate 
wb_data <- read_dta(datapath("Niger_11_14_18.dta"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean and slim to the 2018 data to the most relevant columns ---- 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# View the labels where available
wb_dictionary <- labelled::generate_dictionary(wb_data)

#GRAPPE is the cluster in which the household is located and MENAGE is the household number within that cluster
#The GRAPPE and MENAGE identifiers of the households in 2014 are identical with the grappe and menage identifiers in 2011. 

wb_data_slim <- 
  wb_data %>% 
  select(grappe, menage, hhid, year, int_year, int_month, passage, season, admin1,  admin2, 
         hhsize, pcexp, pcexp_food, pcexp_nonfood, latitude, longitude, zae) %>% 
  arrange(hhid, year) %>% a
rename(cluster = grappe, 
       hhnum_percluster = menage) 

# Check outputs
glimpse(wb_data_slim)

# Number of Households per year
# n_hhid <- wb_data_slim %>% distinct(hhid) %>% nrow() #8069
# n_hhid_year <- wb_data_slim %>%  tabyl(hhid, year) %>% arrange(-2018, 2014, 2011)

# What agroecological zones do we have represented per interview year?
# Note interview year differs from the survey ref year (generally reflect planting vs. harvest visit).
wb_data_slim %>% tabyl(int_year, zae)

# Ensure you have a folder with this title "processed_lsms", else change the folder title
write.csv(wb_data_slim, datapath("processed_lsms/nigerlsms_11_14_18_slim.csv")) 

