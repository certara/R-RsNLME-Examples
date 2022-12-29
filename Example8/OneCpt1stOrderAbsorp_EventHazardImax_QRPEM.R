#####################################################################################################################################
##                              Description
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
##
#####################################################################################################################################

## Load necessary packages, and set up working directory
library(Certara.RsNLME)
library(magrittr)
setwd("./Example8")


#####################################################################################################################################

############  Load the input dataset, define the model and associated column mapping, and then edit them            ################

#####################################################################################################################################


##===================================================================================================================================
##                            Load the input dataset
##===================================================================================================================================

input_data <- read.csv("OneCpt1stOrderAbsorp_EventHazardImax.csv")

##===================================================================================================================================
##                           Define the model and associated column mappings
##===================================================================================================================================
##-----------------------------------------------------------------------------------------------------------------------------------
## Define a basic one-compartment model with 1st-order absorption as well as its associated column mappings
##
## Set initial values for fixed effects, tvV and tvCl to be 5
##
## Set the covariance matrix of random effects to be a diagonal matrix with all its diagonal elements being 0.01
##-----------------------------------------------------------------------------------------------------------------------------------
model <- pkmodel(
  absorption = "Extravascular",
  data = input_data,
  columnMap = FALSE, # Map columns later using colMapping()
  modelName = "OneCpt1stOrderAbsorp_EventHazardImax_QRPEM"
) %>%
  fixedEffect(effect = c("tvV", "tvCl"), value = c(5, 5)) %>%
  randomEffect(effects = c("nV", "nCl", "nKa"),
               values = rep(0.01, 3))


## View the model
print(model)

## Map Columns: ID, Time, and CObs were automatically mapped, need to map Aa model variables to input columns
colnames(input_data)
model <- model %>%
  colMapping(Aa = Dose) # As of RsNLME v1.2.0, unquoted column names are now supported

print(model) #Required columns from parameterization of built-in model e.g., pkmodel() have been mapped
##===================================================================================================================================
##         Edit the model to include time-to-event observations
##===================================================================================================================================

## Run the code below to open the text editor to edit the PML codes
model <- editModel(model)


## To include the time-to-event model with hazard function described by an Imax model 1 - C/(C + IC50), add the following statements
## to the codes right after "ranef" statement
##
   # event(EventObs, 1 - C/(C + IC50))
   #
   # stparm(IC50 = tvIC50)
   #
   # fixef(tvIC50 = c(, 100, ))
##
## Then click the "Save" button to update the model.

## View the updated model
print(model)

## Manually map the newly added observed variable "EventObs" to its corresponding column in the input dataset
model <- model %>%
  colMapping(EventObs = EventObs)


###########################################################################################################################################

###################                    Model Fitting                                                                  ####################

###########################################################################################################################################

## Run the model, setting estimation method to 'QRPEM'
job <- fitmodel(model, method = "QRPEM")

## View estimation results
print(job[c("Overall", "theta", "omega")])
