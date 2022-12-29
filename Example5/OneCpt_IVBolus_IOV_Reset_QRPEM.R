#####################################################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to
##
##    - create a model involving inter-occasion variability (IOV) and reset in RsNLME
##    - fit the model
##
## The model demonstrated is a one-compartment model with IV bolus.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
##
#####################################################################################################################################

## Load necessary packages, and set up working directory
library(Certara.RsNLME)
library(magrittr)
setwd("./Example5")


#####################################################################################################################################

###############      Load the input dataset, and define the model and the associated column mappings                  ###############

#####################################################################################################################################


##===================================================================================================================================
## Load the input dataset
##===================================================================================================================================

input_data <- read.csv("OneCpt_IVBolus_IOV_Reset.csv")

##===================================================================================================================================
##                           Define the model and its associated column mappings
##===================================================================================================================================
##----------------------------------------------------------------------------------------------------------------------------------
## Define the basic PK model (a one-compartment model with IV bolus) along with the associated column mapping, and then
##
##  - Add covariate "Occasion" to the structural parameters V and Cl (and it is automatically mapped to its corresponding column,
##    Occasion, in the input dataset) and set the associated inter-occasion covariance matrix to be a diagonal matrix with all its
##    diagonal elements being 0.5.
##
##  - Set the low value and high value to allow a range of values to be used to trigger a reset and map it to the corresponding
##    column in the input dataset
##----------------------------------------------------------------------------------------------------------------------------------
model <-
  pkmodel(
    data = input_data,
    ID = "id",
    Time = "time",
    A1 = "Dose",
    CObs = "CObs",
    modelName = "OneCpt_IVBolus_IOV_Reset_QRPEM"
  ) %>%
  addCovariate(
    covariate = "Occasion",
    effect = c("V", "Cl"),
    type = "Occasion",
    levels = c(1, 2, 3),
    values = c(0.5, 0.5)
  ) %>%
  addReset(low = 4, hi = 4, Reset = "Reset")

## View the model
print(model)

#####################################################################################################################################

###################                    Model Fitting                                                            ####################

#####################################################################################################################################

## Engine argument setup: set the engine method to be QPREM with MAP assistance enabled (the period to perform MAP assistance is set
## to be 1) and use the default value for all the other relevant arguments.
# engineSetup <- engineParams(model, method = "QRPEM", mapAssist = 1)
# localMPIHost <- hostParams(hostName = "Local_MPI", parallelMethod = "LOCAL_MPI", numCores = 4)
# job <- fitmodel(model, params = engineSetup, hostPlatform = localMPIHost)
## Or specify arguments in fitmodel() via the ellipses ('...') as illustrated below:
## Run the model
job <-
  fitmodel(
    model,
    method = "QRPEM",
    mapAssist = 1,
    parallelMethod = "LOCAL_MPI",
    numCores = 4
  )

## View estimation results
print(job[c("Overall", "theta", "omega")])

