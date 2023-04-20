#===============================================================================
# Purpose: Plot specification chart for vulnerability analysis 
# Author: Armine Poghosyan & Elinor Benami
# Last Updated: # Wed Apr 19 18:30:40 2023 ------------------------------
#===============================================================================

#===============================================================================
# Clean work space ----
#===============================================================================
rm(list=ls(all=TRUE))

#===============================================================================
# Set up file paths  & load data ----
#===============================================================================
# 1. Set up relative file paths so sourcing will work appropriately  ----
this.wd <- dirname(rstudioapi::getActiveDocumentContext()$path)  
setwd(this.wd) 
# setwd("G:/My Drive/VT/_Projects/_WB Sahel/_data/output")

# 2. Point to filepath in dropbox ----
pathdata <- if(Sys.info()["user"] == "elinor"){
  "~/Dropbox/research/wb_climateshocks/data/shared_wb_climateshocks/data/"
} else{
  # enter your filepath here
}
datapath <- function(x){paste0(pathdata, x)}

#===============================================================================
# Load libraries ----
#===============================================================================
library(tidyverse)
library(dplyr)
library(janitor)

library(sf)
library(haven)
library(viridis)

# for regressions
library(plm)
library(fixest)

# source spec_chart.R
source("spec_chart.R")
#===============================================================================
# Load and minorly clean data  ----
#===============================================================================
data_sj <- 
  read_csv(datapath("processed/merge_lsms_sj.csv")) %>% 
  filter(year==2011 & rururb==0| 
           year==2014 & rururb==0| 
           year==2018 & rururb==2) %>% 
  mutate(log_tot_cons = log(data_sj$tot_cons))

#===============================================================================
# Step 1: Get the mean of expected consumption without weather vars ---- 
#===============================================================================
model_multi_object <- feols(log_tot_cons ~ hhsize + mvsw(hhagey, hheducat4, roof, wall, floor), data = data_sj)
etable(model_multi_object)

# Function to predict on fixest_multi objects
predict_fixest_multi <- function(fixest_multi_obj, new_data) {
  predictions <- lapply(fixest_multi_obj, function(model) {
    predict(model, new_data = new_data)
  })
  return(predictions)
}

predictions <- predict_fixest_multi(model_multi_object)
modelnames <- names(model_multi_object)

# Convert predictions list to a data frame
predictions_df <- t(data.frame(matrix(unlist(predictions), nrow=length(predictions), byrow=T))) %>% as.data.frame()

# Add model names as a new column
names(predictions_df) <- modelnames

#===============================================================================
# Step 2: Get the draws for residual & weather indicators ---- 
#===============================================================================
model_multi_weather <- feols(log_tot_cons ~ total_precip + hhsize + mvsw(hhagey, hheducat4, roof, wall, floor), data = data_sj)
etable(model_multi_weather)

# Extract coefficients for each model in the fixest_multi object
coefficients_list <- lapply(model_multi_weather, coef)

# Extract residuals for each model in the fixest_multi object
residuals_list <- lapply(model_multi_weather, residuals)

#===============================================================================
# Step 3: Get the mean and sd of residuals ---- 
#===============================================================================
sd_resid <- lapply(residuals_list, sd) %>% unlist() %>% as.data.frame() %>% setNames("sd_resid")
mean_resid <- lapply(residuals_list, mean) %>% unlist() %>% as.data.frame() %>% setNames("mean_resid")
resid_mean_sd <- bind_cols(mean_resid, sd_resid)
rownames(resid_mean_sd) <- modelnames 

#===============================================================================
# Step 4: simulate 1,000 draws from normal dist ---- 
#===============================================================================
# Set seed
set.seed(37)

simulate_values <- function(df) {
  mean_resid <- df[[1]]
  sd_resid <- df[[2]]
  
  # Obtain simulated values, assuming resids follow  Gaussian normal dist

  sim_values <- rnorm(n = 1000, mean = mean_resid, sd = sd_resid)
  
  return(sim_values)
}

# Run 1,000 simulations for each row in resid_mean_sd
simulations_list <- lapply(seq_len(nrow(resid_mean_sd)), function(i) {
  simulate_values(resid_mean_sd[i,])
})


#===============================================================================
# Step 5: Get the mean and sd of weather anomaly & simulate 1,000 draws from hist. dist ---- 
#===============================================================================
# Take 1,000 draws of the weather anomaly from its historic distr. 
sim.precip <-
  rnorm(n = 1000,
        mean(data_sj$z_ndvi_1982_2010),
        sd(data_sj$z_ndvi_1982_2010)) %>%
  as.data.frame()

#===============================================================================
# Step 6: Get var of total per capita consumption ---- 
#===============================================================================
# Create an empty list to store the simulated per capita consumption
sim_cons <- list()


for (i in 1:nrow(model.space)) {
  
  # Get the data frame and coefficients for the i-th model
  model_name <- paste0("mod", i)
  coef_name <- paste0("coef", i)
  
  model_i <- get(model_name) #matrix of independent variables
  coef_i <- get(coef_name) 
  
  for (j in 1:6){
    
    # Check if the current row of coef_i exists
    if (!is.na(coef_i[j,1])) {
      
      # Create the prediction
      sim_cons[[i]] <- coef_i[1,1] + #intercept
        if (ncol(coef_i) > 1) {
          apply(coef_i[j+1, ] * model_i[, j], 1, sum)
        } else {
          coef_i[j+1, ] * model_i[, j]
        } + sim.resid[[i]]
      break
    }
  }
}  



# Calculate st. dev. for each HH using 1,000 obs.
sim.stdev <- list()

for(i in 1:length(sim_cons)){
  
  sim.stdev[[i]] <- rep(sd(sim_cons[[i]]), length(data_sj$log_tot_cons))
}


#===============================================================================
# Step 4: Get the probability of falling below the poverty line
#===============================================================================

poverty_line <- list()
exp_mean <- as.list(predict)

data_vul <- list()
prob_poor <- list()
vul_dummy <- list()

vul_count <- data.frame(count = rep(0, length(sim_cons)))

# Just replicating the log.poverty_line value to match the structure 
# of the remaining components
for(i in 1:length(sim_cons)){
  
  poverty_line[[i]] <- data_sj$log_tot_cons
  
  data_vul[[i]] <- (poverty_line[[i]] - exp_mean[[i]]) / (sim.stdev[[i]])

  prob_poor[[i]] <- pnorm(data_vul[[i]])
  
  vul_dummy[[i]] <- ifelse(prob_poor[[i]] < 0.5, 1, 0)
  
  count <- sum(vul_dummy[[i]] == 1, na.rm = TRUE)
  vul_count[i, "count"] <- count

}

vul_count$percentage <- round((vul_count$count/length(data_sj$log_tot_cons))*100,3)


########################################################################################

#===============================================================================
# Specification chart
#===============================================================================

xs <- c("weather_anomaly","hhsize", "hhagey", "hheducat4", "roof", "wall", "floor")

vars_list <- list()

# Loop through each row of model.space
for (i in 1:nrow(model.space)) {
  
  # Extract the column indices where the value is 1
  col_indices <- which(model.space[i,] == 1)
  
  # Use the column indices to extract the corresponding variable names from xs
  vars <- xs[col_indices]
  
  # Store the variable names in the vars_list
  vars_list[[i]] <- vars
}

# Print the vars_list
#vars_list #correct

flist <- lapply(vars_list, function(x) paste("log.tot_cons ~",paste(x, collapse=" + ")))

#flist #correct

vul.percent <- vul_count #similar to regs1

# add se because chart does not work without se
sd <- sd(vul_count$percentage)
sd <- rep(sd(vul_count$percentage), nrow(model.space))
vul.percent$sd <- sd

vul.indicator <- lapply(vars_list, function(x) xs %in% x) #similar to regs2 from original reproducible script
vul.indicator_df <- data.frame(matrix(unlist(vul.indicator), ncol=length(xs), byrow=TRUE))
colnames(vul.indicator_df) <- xs

data_chart <- cbind(vul.percent, vul.indicator_df)

# Remove unnecessary var
data_chart <- data_chart[,-1]


# Add labels for variables
head(data_chart)

# Enter labels in order they appear in table
labels <- list("Weather Anomaly:" = c("Total annual precipitation"),
               "Household Characteristics:" = c("Household size",
                                                      "Age of hh head", 
                                                      "Education level of hh head",
                                                      "Roof material",
                                                      "Wall material",
                                                      "Floor material"))

#===============================================================================
# Specification Chart
#===============================================================================

setwd("c:/Users/armin/Desktop")

# Load specification chart function
source("spec_chart.R")


# Looks better when there is an outer margins
par(oma=c(1,0,1,1))

schart(data_chart, labels, ylab="Percentage of vulnerable",
       order="increasing", highlight=1, pch.est=16,
       col.est=c("grey80","royalblue"), 
       col.dot=c("grey60","grey95","grey95","royalblue"),
       lwd.symbol=2, lwd.est=2, 
       ylim = c(49, 52))


#===============================================================================
# Specification Chart for All Weather Anomaly Measures
#===============================================================================

# Rerun the above code and rowbind results together for chart
chart_full_data <- rbind(data_chart1, data_chart2, data_chart3, data_chart4)

chart_full_data <- chart_full_data %>% 
  mutate(total_precip = FALSE, 
         peak_ndvi = FALSE, 
         z_precip = FALSE,
         z_ndvi = FALSE)


chart_full_data$total_precip <- c(rep(TRUE, 32), rep(FALSE, nrow(chart_full_data) - 32))
chart_full_data$peak_ndvi <- c(rep(FALSE, 32), rep(TRUE, 32), rep(FALSE, nrow(chart_full_data) - 64))
chart_full_data$z_precip <- c(rep(FALSE, 64), rep(TRUE, 32), rep(FALSE, nrow(chart_full_data) - 96))
chart_full_data$z_ndvi <- c(rep(FALSE, 96), rep(TRUE, 32), rep(FALSE, nrow(chart_full_data) - 128))

chart_full_data <- subset(chart_full_data, select = -c(3))
chart_full_data <- chart_full_data[, c(1:2, 9:12, 3:8)]


# Enter labels in order they appear in table
labels <- list("Weather Anomaly:" = c("Total annual precipitation", 
                                      "Peak NDVI",
                                      "Z-score precip (1982-2010)",
                                      "Z-score NDVI (1982-2010)"),
               "Household Characteristics:" = c("Household size",
                                                "Age of hh head", 
                                                "Education level of hh head",
                                                "Roof material",
                                                "Wall material",
                                                "Floor material"))

setwd("c:/Users/armin/Desktop")

# Load specification chart function
source("spec_chart.R")

# Looks better when there is an outer margins
par(oma=c(1,0,1,1))

schart(data_chart1, labels, ylab="Percentage of vulnerable",
       order="increasing", highlight=1, pch.est=16,
       col.est=c("grey80","royalblue"), 
       col.dot=c("grey60","grey95","grey95","royalblue"),
       lwd.symbol=2, lwd.est=2, 
       ylim = c(49, 52))




### Scrap--- 
# ####
# 
# # Get all combinations of variables
# n <- 6
# l <- rep(list(0:1), n)
# 
# model.space <- 
#   expand.grid(l) %>% 
#   filter(Var1==1) %>% 
#   data.frame() #32 combinations hence we need to estimate 32 models
# 
# # Filter for usable independent vars for regression analysis
# var <- data_sj %>% select('hhsize', 'hhagey', 'hheducat4', 'roof', 'wall', 'floor')
# 
# # Create a list of model data frames
# model_list <- lapply(1:nrow(model.space), function(i) {var[model.space[i] == 1]})
# 
# # Create a list of model data frames
# model_list <- lapply(1:nrow(model.space), function(i) {
#   # Select the columns from variable_matrix based on the values in the ith row of model.space
#   var[, model.space[i,] == 1, drop = FALSE]
# })
# 
# # Example of accessing the first model
# #model_list[[1]] #this is correct
# 
# # Loop over each element in the list and create a new data frame with a unique name
# for (i in seq_along(model_list)) {
#   
#   # Create a unique name for the data frame
#   df_name <- paste0("mod", i)
#   
#   # Extract the current vector from the list and convert it to a data frame
#   df <- as.data.frame(model_list[[i]])
#   
#   # Assign the data frame to a variable with the unique name
#   assign(df_name, df)
# }
# 
# # Create an empty data frame to store the predicted values
# predict <-  data.frame(matrix(nrow=length(data_sj$log_tot_cons), ncol=0))
# 
# # Loop over each data frame name and run a linear regression model
# for (i in 1:nrow(model.space)) {
#   
#   # Extract the data frame
#   data <- get(paste0("mod", i))
#   
#   # Run the linear regression model
#   model <- lm(data_sj$log_tot_cons ~ ., data = data)
#   
#   # Make predictions
#   predict$exp.mean_mod <- model$fitted.values
#   
#   # Create a unique name for the variables
#   colname <- paste0("exp.mean_mod",i)
#   
#   # Assign the variable to its unique name
#   predict[[colname]] <- predict$exp.mean_mod
# }
# 
# # Print the resulting data frame
# #print(predict) #correct
# 
# # Remove unnecessary columns
# predict <- predict[,-1]


# 
# # Get all combinations of variables
# n <- 7 #adding the weather indicator
# l <- rep(list(0:1), n)
# model.space <- expand.grid(l) %>% filter(Var1==1 & Var2==1) %>% 
#   data.frame() #32 combinations hence we need to estimate 32 models
# 
# # Filter for usable independent vars for regression analysis
# var <- data_sj %>% 
#   select('z_ndvi_1982_2010','hhsize', 'hhagey', 'hheducat4', 'roof', 'wall', 'floor')
# 
# # Create empty data frame to store the residuals & list to store coefficients
# residual <-  data.frame(matrix(nrow=length(data_sj$log_tot_cons), ncol=0))
# coef_list <- list()
# 
# # Create a list of model data frames
# model_list <- lapply(1:nrow(model.space), function(i) {
#   
#   # Select the columns from variable_matrix based on the values in the ith row of model.space
#   var[, model.space[i,] == 1, drop = FALSE]
# })
# 
# 
# # Loop over each element in the list and create a new data frame with a unique name
# for (i in seq_along(model_list)) {
#   
#   # Create a unique name for the data frame
#   df_name <- paste0("mod", i)
#   
#   # Extract the current vector from the list and convert it to a data frame
#   df <- as.data.frame(model_list[[i]])
#   
#   # Assign the data frame to a variable with the unique name
#   assign(df_name, df)
# }
# 
# 
# # Loop over each data frame name and run a linear regression model
# for (i in 1:nrow(model.space)) {
#   
#   # Extract the data frame
#   data <- get(paste0("mod", i))
#   
#   # Run the linear regression model
#   formula <- as.formula(lm(data_sj$log_tot_cons ~ ., data = data))
#   
#   # Obtain residuals 
#   residual$resid <- residuals(lm(formula, data = data))
#   
#   # Create a unique name for the variables
#   colname <- paste0("resid_mod",i)
#   
#   # Assign the variable to its unique name
#   residual[[colname]] <-  residual$resid
#   
#   # Moving to obtaining coefficients
#   
#   # Collect coeffs from regression model in the list
#   coef_list[[i]] <- list(coef(lm(formula, data = data)))
#   
#   # Choose names for extracted coeffs
#   df_name <- names(coef_list)[i] <- paste0("coef", i)
#   
#   # Extract the current vector from the list, convert it to a data frame 
#   # and assign it to selected unique column names
#   assign(df_name, setNames(as.data.frame(coef_list[[i]]), df_name))
# }
# 
# # Remove unnecessary columns
# residual <- residual[,-1]


# Create an empty data frame to store the mean residuals
resid_mean_sd <-  data.frame(matrix(nrow=nrow(model.space), ncol=0))

for (i in 1:nrow(model.space)) {
  
  # Extract the data frame
  data <- residual[, i]
  
  # Get the mean and sd
  resid_mean_sd$mean <- mean(data)
  resid_mean_sd$sd   <- sd(data)
  
  # Assign unique column names for mean and sd associated w/regression models
  colname1 <- paste0("mean.resid",i)
  resid_mean_sd[[colname1]] <-  resid_mean_sd$mean
  
  colname2 <- paste0("sd.resid",i)
  resid_mean_sd[[colname2]] <-  resid_mean_sd$sd
}

# Remove unnecessary columns
resid_mean_sd <- resid_mean_sd[,c(-1,-2)]