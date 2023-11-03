#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                                                  Description
##
## The purpose of this example is to demonstrate how to
##
##    - edit a build-in model
##
##    - fit the edited model
##
## The model demonstrated is a  model involving both continuous and event observations
##
##   - PK: a one-compartment model with first-order absorption having no time lag,
##
##   - Event model: hazard function is described by an Imax model given by 1 - C/(C + IC50)
##
##  Here C denotes the drug concentration at the central compartment, and IC50 is the drug concentration necessary to produce
##  half-maximal inhibition.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load necessary packages and set working directory
library(Certara.RsNLME)
library(magrittr)
library(data.table)

if (Sys.getenv("RSTUDIO") == 1) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  setwd("./Example8")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                  Model ----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##
##     Load the input dataset
##
InputDataSetName <- "OneCpt1stOrderAbsorp_EventHazardImax"
dt_InputDataSet <- fread(paste0(InputDataSetName, ".csv"))


##******************************************************************************************************************************************
##
##                               Define the model and associated column mappings -----
##
##******************************************************************************************************************************************
## model name
ModelName <- paste0(InputDataSetName, "_QRPEM")


##
## Define a basic one-compartment model with 1st-order absorption as well as its associated column mappings
##
## Set initial values for fixed effects, tvV and tvCl to be 5
##
## Set the covariance matrix of random effects to be a diagonal matrix with all its diagonal elements being 0.01
##
model <- pkmodel(
  absorption = "FirstOrder",
  data = dt_InputDataSet,
  ID = "ID",
  Time = "Time",
  Aa = "Dose",
  CObs = "CObs",
  modelName = ModelName
) %>%
  fixedEffect(effect = c("tvV", "tvCl"), value = c(5, 5)) %>%
  randomEffect(effect = c("nV", "nCl", "nKa"),
               value = rep(0.01, 3))


## View the model and its associated column mappings
print(model)


##******************************************************************************************************************************************
##
##             Edit the model to include time-to-event observations -----
##
##******************************************************************************************************************************************

## Run the code below to open the text editor to edit the PML codes
model <- editModel(model)


## To include the time-to-event model with hazard function described by an Imax model 1 - C/(C + IC50), add the following statements
## to the codes right after "ranef" statement
##
##    event(EventObs, 1 - C/(C + IC50))
##
##    stparm(IC50 = tvIC50)
##
##    fixef(tvIC50 = c(, 100, ))
##
## Then click the "Save" button to update the model.
##
## Note: the edited model object can be found in the same directory as this file is located, and it is called
##       editedModel_OneCpt1stOrderAbsorp_EventHazardImax.Rds


## View the updated model and its associated column mappings
print(model)

## Manually map the newly added observed variable "EventObs" to its corresponding column in the input dataset
model <- model %>%
  colMapping(c(EventObs = "EventObs"))


## View the updated column mappings
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                                       Model Fitting -------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Engine argument setup: set the engine method to be QPREM and use default values for all the relevant arguments
##
engineSetup <- engineParams(model, method = "QRPEM")


##
## Run the model using the default host with engine arguments defined above
##
## Note: Alternatively one can define engine arguments through ellipsis (additional argument) in fitmodel as what is shown below
##
##          job <- fitmodel(model, method = "QRPEM")
##
job <- fitmodel(model, params = engineSetup)


## View estimation results
print(job$Overall)
