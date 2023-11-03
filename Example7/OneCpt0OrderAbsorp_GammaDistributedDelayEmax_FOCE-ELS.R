#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
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
# We also demonstrate how to import estimation results to xpose database to create some commonly used diagnostic plots.
#
# Note: To run this file, please set the working directory to the location where this file is located.
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load necessary packages and set working directory
library(Certara.RsNLME)
library(magrittr)
library(data.table)

if (Sys.getenv("RSTUDIO") == 1) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  setwd("./Example7")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#       Model ----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load input data set
inputDataSetName = "OneCpt0OrderAbsorp_GammaDistributedDelayEmax"
dt_InputDataSet <- fread(paste0(inputDataSetName, ".csv"))

##******************************************************************************************************************************************
##
##       Load the PML codes to create a R model object and define the associated column mappings -----
##
##******************************************************************************************************************************************

# Name of the R model object to be created
modelName <- paste0(inputDataSetName, "_FOCE-ELS")

# Name of PML code file and its location
sourceCode <- paste0(getwd(), "/", inputDataSetName, ".mdl")

# Load the PML codes and link it to associated input data to create a model object
model <- textualmodel(modelName = modelName, mdl = sourceCode, data = dt_InputDataSet)

# View the model and its associated column mappings
print(model)

# Manually map the un-mapped model variables to their corresponding input data columns
model <- model %>%
  colMapping(A = Dose) #RsNLME >= 1.2.0 supports unquoted column names in colMapping()


# View the updated column mappings
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                          Model fitting ----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)

## View estimation results
print(job$Overall)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                      Diagnostic plots ----
#
#
# Here we demonstrate how to import estimation results to xpose database to create some commonly used diagnostic plots (through command-line).
#
# Alternatively, one can view/customize diagnostic plots as well as estimation results through model results shiny app
# (in Certara.ModelResults package), which can also be used to to generate R script and report as well as the associated R markdown.
# This shiny app can be invoked through either the model object created above
#
#           resultsUI(model)
#
# or the xpose data base created below
#
#           resultsUI(xpdb = xp)
#
# For details on this app as well as how to use it, please visit the following link
#
#           https://certara.github.io/R-model-results/index.html.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
library(Certara.Xpose.NLME)
library(xpose)
xp <- xposeNlmeModel(model, job)


##******************************************************************************************************************************************
##
##      Observations against population or individual predictions  ----
##
##******************************************************************************************************************************************

## observations against population predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")

## observations against individual predictions
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


##******************************************************************************************************************************************
##
##    CWRES against population predictions (PRED) or independent variable  ----
##
##******************************************************************************************************************************************

## CWRES against population predictions
res_vs_pred(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")

## CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")


##******************************************************************************************************************************************
##
##          Individual plots  -----
##
##******************************************************************************************************************************************

## Observations, individual predictions and population predictions plotted against the independent variable for every individual
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")

