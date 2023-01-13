#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                              Description
##
## The purpose of this example is to demonstrate how to
##
##    - create a model involving inter-occasion variability (IOV) and reset in RsNLME
##
##    - fit the model with engine arguments defined through ellipsis (additional argument)
##
## The model demonstrated is a one-compartment model with IV bolus.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load necessary packages and set working directory
library(Certara.RsNLME)
library(magrittr)
library(data.table)
setwd("./Example5")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#      Model  ------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##
##  Load the input dataset
##
InputDataSetName <- "OneCpt_IVBolus_IOV_Reset"
dt_InputDataSet <- fread(paste0(InputDataSetName, ".csv"))


##******************************************************************************************************************************************
##
##    Define the model and its associated column mappings ------
##
##******************************************************************************************************************************************

## Model name
ModelName <- paste0(InputDataSetName, "_QRPEM")

##
## Define the basic PK model (a one-compartment model with IV bolus) along with the associated column mapping, and then
##
##  - Add covariate "Occasion" to the structural parameters V and Cl (and it is automatically mapped to its corresponding column,
##    Occasion, in the input dataset) and set the associated inter-occasion covariance matrix to be a diagonal matrix with all its
##    diagonal elements being 0.5.
##
##  - Set the low value and high value to allow a range of values to be used to trigger a reset and map it to the corresponding
##    column in the input dataset
##
model <- pkmodel(data = dt_InputDataSet, ID = "id", Time = "time", A1 = "Dose", CObs = "CObs"
                , modelName = ModelName
                )%>%
  addCovariate(covariate = "Occasion", effect = c("V", "Cl"), type = "Occasion", levels = c(1, 2, 3), values = c(0.5, 0.5)) %>%
  addReset(low = 4, hi = 4, Reset = "Reset")

## View the model
print(model)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                Model Fitting   --------------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##
## Run the model with the default host
##
## Engine arguments are defined through ellipsis (additional argument)
##
##    - Set the engine method to be QPREM with MAP assistance enabled (the period to perform MAP assistance is set to be 1)
##
##    - Use the default value for all the other relevant arguments
##
job <- fitmodel(model, method = "QRPEM", mapAssist = 1)

## View estimation results
print(job$Overall)

