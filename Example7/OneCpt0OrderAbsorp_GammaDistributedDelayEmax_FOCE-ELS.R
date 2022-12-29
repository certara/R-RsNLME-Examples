##################################################################################################################################
#                         Description
#
# The purpose of this file is to demonstrate how to fit a model defined by PML codes, where
#
#   - the PK portion of the model is described by a one-compartment model with zero-order absorption,
#
#   - the PD portion of the model is described by a delayed Emax model with delay time assumed to be gamma distributed
#
# See "OneCpt0OrderAbsorp_GammaDistributedDelayEmax.mdl" in the same directory where this file is located for details.
#
#
# Note: To run this file, please set the working directory to the location where this file is located.
#
##################################################################################################################################

## Load necessary packages, and set up working directory
library(Certara.RsNLME)
library(magrittr)
setwd("./Example7")

##================================================================================================================================
##       Load the input data
##================================================================================================================================

input_data <- read.csv("OneCpt0OrderAbsorp_GammaDistributedDelayEmax.csv")
##================================================================================================================================
##       Load the PML codes to create a R model object and define the associated column mappings
##================================================================================================================================

# Load the PML codes and link it to associated input data to create a model object
model <- textualmodel(modelName = "OneCpt0OrderAbsorp_GammaDistributedDelayEmax_FOCE-ELS",
                      mdl = "OneCpt0OrderAbsorp_GammaDistributedDelayEmax.mdl",
                      data = input_data)

# Check the mapping between model variables and input data columns
print(model)
# We can see that A is not mapped. Manually map the rest of model variables to its corresponding input data columns
model <- model %>%
  colMapping(A = "Dose")


##================================================================================================================================
##                          Model fitting
##================================================================================================================================

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)

## View estimation results
print(job[c("Overall", "theta", "omega")])


##================================================================================================================================
##                      Diagnostic plots
##================================================================================================================================
library(Certara.Xpose.NLME)
library(xpose)
## Imports results of an NLME model/job into xpose database to create commonly used diagnostic plots
xp <- xposeNlmeModel(model, job)

## observations against population predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")

## observations against individual predictions
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


## CWRES against population predictions
res_vs_pred(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")

## CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")


## Observations, individual predictions and population predictions plotted against the independent variable for every individual
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")
