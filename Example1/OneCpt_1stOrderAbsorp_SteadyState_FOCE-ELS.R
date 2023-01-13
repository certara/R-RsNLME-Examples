#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                               Description
##
## The purpose of this example is to demonstrate how to create a model involving steady state through RsNlme.
## The model demonstrated is a one-compartment model with first-order absorption having no time lag.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load necessary packages and set working directory
library(Certara.RsNLME)
library(data.table)
library(magrittr)
setwd("./Example1")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#       Model ----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##
##  Load the input dataset
##
InputDataSetName <- "OneCpt_1stOrderAbsorp_SteadyState"
dt_InputDataSet <- fread(paste0(InputDataSetName, ".csv"))


##******************************************************************************************************************************************
##
##                   Create the model and map model variables to their corresponding input data columns ----
##
##******************************************************************************************************************************************

## Model name
ModelName <- paste0(InputDataSetName, "_FOCE-ELS")

##
## Define a basic one-compartment PK model with 1st-order absorption as well as the associated column mapping
##
## Change the initial values of fixed effects, tvV and tvCl, to 10 and 2, respectively.
##
model <- pkmodel(
  numCompartments = 1,
  absorption = "FirstOrder",
  data = dt_InputDataSet,
  ID = "id",
  Time = "time",
  Aa = "Dose",
  CObs = "CObs",
  modelName = ModelName
) %>%
  fixedEffect(effect = c("tvV", "tvCl"), value = c(10, 2))

##
## View the model and its associated column mappings
##
print(model)


##******************************************************************************************************************************************
##
##  Add steady state dose mapping information to the column mapping file ----
##
## Note: the model object must have basic column mappings defined
##
##******************************************************************************************************************************************
model <- addSteadyState(model, SS = "SS", II = "II")


##
## Note: for this problem, an alternative way to implement the steady state is through "addDoseCycle" with code given below
##    model <- addDoseCycle(model, type = "SteadyState", name = "Aa", amount = "Dose", II = "II", colName = "SS")
##


##
## View the updated column mappings
##
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                         Model Fitting  ------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)


## View estimation results
print(job$Overall)




