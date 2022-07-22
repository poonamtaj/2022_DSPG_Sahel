##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Elinor Benami
# Project name: Sahel ASP
# Date Last Updated: # Thu Jul 21 14:21:36 2022 ------------------------------
# R version: 4.1.3
# Purpose: Inspect revised LSMS data
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
# Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(haven) # for read_dta
library(tidylog) # for output on operations/changes with select, drop, merges, etc.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Bulk Upload the Data For Inspection 
files <- list.files(datapath("shared_wb_climateshocks/data/raw/lsms_update_21july2022"), pattern = "*.dta", include.dirs = TRUE)
file.names <- datapath(paste0("shared_wb_climateshocks/data/raw/lsms_update_21july2022/", files))
boring <- datapath("shared_wb_climateshocks/data/raw/lsms_update_21july2022/")

for(i in 1:length(file.names)){
  start.stripped.i <- unlist(strsplit(x = file.names[i], split = boring))[2]
  obj.name.i <- unlist(strsplit(x = start.stripped.i, split = '\\.'))[1] # escape character before . so it's not treated as a wildcard 
  X.i <- read_dta(file.names[i])
  assign(x = obj.name.i, value = X.i)
  rm(list = c('start.stripped.i', 'obj.name.i', 'X.i'))
  gc()
}

# see all the files, remove extraneous
ls()
rm(boring, file.names, i)


# 2. Inspect Files --- 
skim(ECVMA_2011) # household data
glimpse(ECVMA_2011)
ecv_2011_dict <- labelled::generate_dictionary(ECVMA_2011); ecv_2011_dict 

ecvma_slim_2011 <- 
  ECVMA_2011 %>% 
  select(year, hid, 
         cluster, 
         fdtexp, #Purchased and auto-consumption food expenditure, nominal (annual)
         nfdtexp, #Purchased & auto-consumption non-food expenditure, nominal (annual)
         hhtexp, #Household food and non-food consumption expenditure, nominal (annual)
         pc_fd, #Per capita food consumption expenditure, nominal (annual)
         pc_hh, #Per capita food and non-food consumption, nominal (annual)
         padq_fd, #Per adult equivalent food consumption expenditure, nominal (annual)
         padq_hh, #Per adult equivalent food and non-food consumption, nominal (annual)
         pc_fddr, #Per capita food consumption expenditure, deflated (annual)
         pc_hhdr, #Per capita food and non-food consumption expenditure, deflated (annual)
         hhsize, 
         int_year,
         int_month,
         latitude,
         longitude
  )


ecvma_slim_2014 <- 
  ECVMA_2014_welfare %>% 
  select(year, hid, 
         cluster, 
         fdtexp, #Purchased and auto-consumption food expenditure, nominal (annual)
         nfdtexp, #Purchased & auto-consumption non-food expenditure, nominal (annual)
         hhtexp, #Household food and non-food consumption expenditure, nominal (annual)
         pc_fd, #Per capita food consumption expenditure, nominal (annual)
         pc_hh, #Per capita food and non-food consumption, nominal (annual)
         padq_fd, #Per adult equivalent food consumption expenditure, nominal (annual)
         padq_hh, #Per adult equivalent food and non-food consumption, nominal (annual)
         pc_fddr, #Per capita food consumption expenditure, deflated (annual)
         pc_hhdr, #Per capita food and non-food consumption expenditure, deflated (annual)
         hhsize, 
         int_year,
         int_month,
         latitude,
         longitude
  )

# Re-merge both datasets together.
lsms_slim_2011_2014 <- 
  ecvma_slim_2014 %>% 
  rbind(ecvma_slim_2014)

write.csv(lsms_slim_2011_2014, datapath("shared_wb_climateshocks/data/processed/lsms_harmonized_11_14.csv"))

## 2018 renders a bit differently/with different colnames and contents
# Note we didn't have an issue before with 2018, so exporting 2011 and 2014 seems fine
# Determine common variables for each and create new datafile --- 
ecv_2018_dict <- labelled::generate_dictionary(EHCVM_2018_welfare); ecv_2018_dict
names(ECVMA_2014_welfare)

ecvma_slim_2018 <- 
  EHCVM_2018_welfare %>% 
  select(#year, 
         hhid, 
         cluster, 
         pcexp_food, #bien-être (seulement alimentation)
         pcexp_nonfood, #bien-être (seulement non-alimentation)
         GPS__Latitude, 
         GPS__Longitude, 
         sh_food,
         sh_nonfood
  ) %>% 
  mutate(year = 2018) %>% 
  rename(latitude = GPS__Latitude, 
         longitude = GPS__Longitude)


