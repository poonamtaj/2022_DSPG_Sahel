#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Elinor Benami
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Wed Jun 29 08:50:41 2022 ------------------------------
# Date Created: # Wed Jun 29 23:55:12 2022 ------------------------------
# R version: 4.1.3
# Purpose: Visualize Population in Niger
# Sources: https://data.humdata.org/dataset/cod-ps-ner
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
library(tidylog) # for output on operations/changes with select, drop, merges, etc.
library(readxl)
library(janitor)
library(rio) # for import export
library(ggspatial)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Data -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
niger_level2 <- 
  st_read(datapath("geospatial/wb_niger_admin2_shapefile/niger_admin2.shp")) %>% 
  mutate(dept_clean = make_clean_names(admin2Name)) %>%
  clean_names()  %>% 
  mutate(dept_clean = case_when(dept_clean == "ingall" ~ "in_gall", 
                                dept_clean == "n_gourti" ~ "ngourti", 
                                dept_clean == "guidan_roumdji" ~ "guidan_roumji", 
                                dept_clean == "ville_de_niamey" ~ "niamey" , 
                                dept_clean == "takeita" ~ "takieta", 
                                TRUE ~ dept_clean))

pop_niger_admin2 <- 
  import(datapath("niger_statistiques-population-2022_ins_adm1_adm2_30092021.xlsx"), 
             sheet = 3) %>% 
  clean_names() %>% 
  mutate(dept_clean = make_clean_names(departement)) %>% 
  select(region, departement, dept_clean, total_population_2022) %>% 
  filter(region != "#adm1+name") 

## Join datasets
pop_sf <- full_join(niger_level2, pop_niger_admin2, by = "dept_clean") 
total_population_est <- sum(as.numeric(pop_sf$total_population_2022), na.rm = TRUE)

## Generate Map
niger_level2 %>%
  ggplot() +
  geom_sf(color = "NA") +
  geom_sf(data = pop_sf,
          size = 1, 
          color = "NA",
          aes(fill = as.numeric(total_population_2022)/100000), alpha = 0.95) + #alpha adjusts te transparency
  labs(title = "Population by Département in 2022 (in 100,000s) ",
       subtitle = paste0("Total estimated population for 2022 = ", scales::comma(total_population_est)), 
       caption = paste0("Source: Niger Data Grid (https://data.humdata.org/dataset/cod-ps-ner)")) + 
  # coord_sf() +
  scale_fill_viridis(name = "100k People", direction = 1) +
  annotation_scale(location = "br") +
  theme_classic() +
  theme(#legend.position="bottom",
        plot.caption = element_text(hjust = 0))
  
