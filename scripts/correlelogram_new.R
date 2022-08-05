#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Poonam Tajanpure
# Project name: Sahel Adaptive Social Protection Program
# Date Created: # Fri Jul 1 2022 ------------------------------
# Date Last Updated: Tues Jul 5 2022
# R version: 4.1.2
# Purpose: Correlogram 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clear Environment and Set Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls(all = TRUE))

# Don't show scientific notation
options(scipen = 999)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load Libraries and Options -----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(ggstatsplot)


###Set Options -----
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


NDVIadmin2_ts <- read.csv("./data/processed/admin2ndvi.csv")
preciadmin2_ts <- read.csv("./data/processed/admin2precip.csv")

##clean dataframe
preciadmin2_ts_clean <- preciadmin2_ts %>% 
  filter(!year > 2019) %>% 
  select(year, admin2Pcod, total_precip, zscore_precip, baselineMean_precip)


drought_ts <- left_join(NDVIadmin2_ts, preciadmin2_ts_clean) 
#write.csv(drought_ts, "./data/processed/drought_ts.csv", row.names = FALSE)
drt_short <- drought_ts %>% select(admin2Name, admin2Pcod, year, total_precip, peak_ndvi, zscore_precip, zscore_ndvi) 
#write.csv(drt_short, "./data/processed/drt_short.csv", row.names = FALSE)


########## ggstatsplot #############


## adm2 short for period 2011 to 2019
drt_adm2_short <- drt_short %>% filter( year == 2011 | year == 2014  | year == 2015 | year == 2017 | year == 2018) %>% 
  filter(! admin2Name == "Bilma", ! admin2Name == "Birni N'Konni", ! admin2Name == "Boboye", 
        ! admin2Name == "Bosso", ! admin2Name == "Tassara", ! admin2Name == "Tillia" ) ## excluding Bilma & 5 other departments not covered in LSMS 2018.

grouped_ggscatterstats(data = drt_adm2_short, x = total_precip, y = peak_ndvi, 
                       grouping.var = year, plotgrid.args = list(nrow = 3),
                       xlab  = "Total Precipitation (mm)",
                       ylab  = "Peak NDVI",
                       annotation.args  = list(title = "Relationship between Precipitation and NDVI"))


###  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## z-score Corr plots using grouped_ggscatterstats()

 grouped_ggscatterstats(data = drt_adm2_short, x = zscore_precip, y = zscore_ndvi, 
                             grouping.var = year, plotgrid.args = list(nrow = 3),
                             xlab  = "Annual Precipitation z-score",
                             ylab  = "Annual NDVI z-score",
                             annotation.args  = list(title = "Relationship between Precipitation and NDVI"))

#################################################################
###  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## z-score Corr plots using grouped_ggscatterstats()

######### join dorught to lsms data to plot food share exp

lsms_adm2 <- read.csv("./data/processed/lsms_admin2_new.csv") %>% 
  select(admin2Name, admin2Pcod, year, pc_fddr, pc_hhdr)

lsms_drt_join <- left_join(drt_adm2_short, lsms_adm2, by = c("admin2Pcod", "year")) %>% 
  select(admin2Pcod, year, total_precip, peak_ndvi, zscore_precip, zscore_ndvi, pc_fddr, pc_hhdr)
  
lsms_drt_clean <- lsms_drt_join %>% group_by(admin2Pcod, year) %>%
  mutate(median_pcfood = median(pc_fddr),
         median_pctotal = median(pc_hhdr)) %>%
  distinct( year, admin2Pcod, total_precip, peak_ndvi, zscore_precip, zscore_ndvi, median_pcfood, median_pctotal) %>%
  ungroup() %>% na.omit()
  

fd_share <- lsms_drt_clean %>% mutate(share_fdexp = ( median_pcfood/ median_pctotal)*100)

####### vul Vs Precip

grouped_ggscatterstats(data = fd_share, x = vul , y = zscore_ndvi, 
                       grouping.var = year, plotgrid.args = list( ncol = 1),
                       xlab  = "Vulnerability",
                       ylab  = "NDVI z-score",
                       annotation.args  = list(title = "Vulnerability and NDVI z-score"))


grouped_ggscatterstats(data = merge_vul_short, x = vul , y = zscore_precip, 
                       grouping.var = year, plotgrid.args = list( ncol = 1),
                       xlab  = "Vulnerability",
                       ylab  = "Precipitation z-score",
                       annotation.args  = list(title = "Vulnerability and Precipitation z-score"))

############################
## new columns to get aggregated values of pcexp_food and vul variables at admin2 level

vul_short <- merge_vul_short %>% group_by(admin2Pcod, year) %>% 
  mutate(agr_vul = median(vul), median_pcfood = median(pcexp_food),
         median_pctotal = median(pcexp), median_pcnonfood = median(pcexp_nonfood)) %>%
  distinct( year, total_precip, peak_ndvi,zscore_precip, zscore_ndvi, median_pcfood, median_pctotal, median_pcnonfood) %>%
  ungroup()


###################### precip & ndvi z-score with less observations

grouped_ggscatterstats(data = vul_short, x = zscore_precip , y = zscore_ndvi, 
                       grouping.var = year, plotgrid.args = list( ncol = 1),
                       xlab  = "Annual Precipitation z-score",
                       ylab  = "Annual NDVI z-score",
                       annotation.args  = list(title = "Relationship between NDVI z-score and Precipitation "))

####### vul Vs Precip
grouped_ggscatterstats(data = fd_share, x = share_fdexp , y = zscore_ndvi, 
                       grouping.var = year, plotgrid.args = list( ncol = 1),
                       xlab  = "Share of per capita food expenditure",
                       ylab  = "Annual NDVI z-score",
                       annotation.args  = list(title = "Share of food expenditure and NDVI z-score"))

grouped_ggscatterstats(data = fd_share, x = share_fdexp , y = zscore_precip, 
                       grouping.var = year, plotgrid.args = list( ncol = 1),
                       xlab  = "Share of per capita food expenditure",
                       ylab  = "Annual Precipitation z-score",
                       annotation.args  = list(title = "Share of food expenditure and Precipitation z-score"))



#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###### Correlograms for Admin 3 level -------------------
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NDVIadmin3_ts <- read.csv("./data/processed/admin3ndvi.csv")
preciadmin3_ts <- read.csv("./data/processed/admin3precip.csv")

##clean dataframe
preciadmin3_ts_clean <- preciadmin3_ts %>% 
  filter(!year > 2019) %>% 
  select(year, rowcacode3, total_precip, zscore_precip, baselineMean_precip)


drought_ts_adm3 <- left_join(NDVIadmin3_ts, preciadmin3_ts_clean) 
#write.csv(drought_ts, "./data/processed/drought_ts.csv", row.names = FALSE)
drt_short_adm3 <- drought_ts_adm3 %>% select(adm_03, rowcacode3, year, total_precip, peak_ndvi, zscore_precip, zscore_ndvi) 


## adm3 short for period 2011 to 2019
drt_adm3_short <- drt_short_adm3 %>% filter( year == 2011 | year == 2014  | year == 2015 | year == 2017 | year == 2018) %>% 
  filter(! adm_03 == "Birni N'Gaouré", ! adm_03 == "Kankandi", ! adm_03 == "Fabidji", ! adm_03 == "Fakara", ! adm_03 == "Koygolo", ! adm_03 == "Kiota", ! adm_03 == "Harikanassou", ! adm_03 == "N'Gonga", 
     ! adm_03 == "Tillia", ! adm_03 == "Alléla", ! adm_03 == "Birni N'Konni", ! adm_03 == "Bazaga", ! adm_03 == "Tsernaoua", ! adm_03 == "Toumour", ! adm_03 == "Bosso",
     ! adm_03 == "Tassara", ! adm_03 == "Djado", ! adm_03 == "Dirkou", ! adm_03 == "Bilma", ! adm_03 == "Fachi") ## excluding Bilma & 5 other departments not covered in LSMS 2018.   

grouped_ggscatterstats(data = drt_adm3_short, x = zscore_precip, y = zscore_ndvi, 
                       grouping.var = year, plotgrid.args = list(nrow = 3),
                       xlab  = "Annual Precipitation z-score",
                       ylab  = "Annual NDVI z-score",
                       annotation.args  = list(title = "Relationship between Precipitation and NDVI"))


lsms_adm3 <- read.csv("./data/processed/lsms_admin3_new.csv") %>% 
  select(adm_02, adm_03, rowcacode3, year, pc_fddr, pc_hhdr)

lsms3_drt_join <- left_join(drt_adm3_short, lsms_adm3, by = c("rowcacode3", "year")) %>% 
  select(adm_02, rowcacode3, year, total_precip, peak_ndvi, zscore_precip, zscore_ndvi, pc_fddr, pc_hhdr)

lsms3_drt_clean <- lsms3_drt_join %>% group_by(rowcacode3, year) %>%
  mutate(median_pcfood = median(pc_fddr),
         median_pctotal = median(pc_hhdr)) %>%
  distinct(adm_02, year, rowcacode3, total_precip, peak_ndvi, zscore_precip, zscore_ndvi, median_pcfood, median_pctotal) %>%
  ungroup() %>% na.omit()


fd3_share <- lsms3_drt_clean %>% mutate(share_fdexp = ( median_pcfood/ median_pctotal)*100)
fd3_share_filter <- 
#write.csv(fd3_share, "./data/processed/fdexp_join_adm3.csv", row.names = FALSE)

grouped_ggscatterstats(data = fd3_share, x = share_fdexp , y = zscore_ndvi, 
                       grouping.var = year, plotgrid.args = list( ncol = 1),
                       xlab  = "Share of per capita food expenditure",
                       ylab  = "Annual NDVI z-score",
                       annotation.args  = list(title = "Share of food expenditure and NDVI z-score"))

grouped_ggscatterstats(data = fd3_share, x = share_fdexp , y = zscore_precip, 
                       grouping.var = year, plotgrid.args = list( ncol = 1),
                       xlab  = "Share of per capita food expenditure",
                       ylab  = "Annual Precipitation z-score",
                       annotation.args  = list(title = "Share of food expenditure and Precipitation z-score"))

















