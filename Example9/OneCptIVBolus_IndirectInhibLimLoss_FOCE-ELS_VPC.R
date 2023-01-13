#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                              Description
##
## This example involves both PK and PD, where PK is described by a one-compartment model with IV bolus, and
## PD is described by an indirect model with the loss of a mediator inhibited.
##
##  - Create the model through build-in model library and define the associated column mappings
#
#         * update structural parameter forms
#         * change initial values of Theta and Omega
#         * edit residual error model
##
##  - Fit the model
##
##  - Import estimation results to xpose database to create some commonly used diagnostic plots
##
##  - Perform VPC for the final model
##
##  - Create VPC plots through open source package "tidyvpc" (command-line usage) and VPC results shiny app (in Certara.VPCResults package)
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
##
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load necessary packages and set working directory
library(Certara.RsNLME)
library(magrittr)
library(data.table)
library(Certara.Xpose.NLME)
library(xpose)
library(tidyvpc)
setwd("./Example9")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#      Model   --------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
##   Load the input dataset
##
InputDataSetName <- "OneCptIVBolus_IndirectInhibLimLoss"
dt_InputDataSet <- fread(paste0(InputDataSetName, ".csv"))


##******************************************************************************************************************************************
##
##                           Define the model and its associated column mappings ----
##
##******************************************************************************************************************************************
# Model name
ModelName <- paste0(InputDataSetName, "_FOCE-ELS")

# Define a basic PK/Indirect model along with the associated column mappings
model <-
  pkindirectmodel(
    indirectType = "LimitedInhibition",
    isBuildup = FALSE,
    data = dt_InputDataSet,
    ID = "ID",
    Time = "Time",
    A1 = "Dose",
    CObs = "CObs",
    EObs = "EObs",
    modelName = ModelName
  )

### .......................................................................................................................................
###
###
### Reset the forms of structural model parameters as well as initial values for Theta and Omega -----
###
###   - Set Imax = ilogit(tvlogitImax) with ilogit used to make sure it is between 0 and 1
###
###   - set IC50 = tvIC50
###
###   - reset initial values for fixed effects "tvCl", "tvKin", "tvKout" (the default value is 1)
###
###   - Reset the covariance matrix of the random effects to be a diagonal matrix with all its diagonal elements being 0.1
###
### Reset the residual error models for EObs to be multiplicative with standard deviation of residual error to be 0.1 ----
###
### .......................................................................................................................................
model <- model %>%
  structuralParameter(paramName = "Imax", style = "LogitNormal", fixedEffName = "tvlogitImax", hasRandomEffect = FALSE) %>%
  structuralParameter(paramName = "IC50", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvCl", "tvKin", "tvKout"), value = c(0.5, 10, 0.5)) %>%
  randomEffect(effect = c("nV", "nCl", "nKin", "nKout"),  value = rep(0.1, 4)) %>%
  residualError(predName = "E", errorType = "Multiplicative", SD = 0.1)


##
## View the model and its associated column mappings
##
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                    Model Fitting ------
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
#                      Diagnostic plots -----
#
# Here we demonstrate how to import estimation results to xpose database to create some commonly used diagnostic plots
# for each continuous observed variable (through command-line).
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
xp <- xposeNlmeModel(model, job)


##******************************************************************************************************************************************
##
##      Observations against population or individual predictions for each observed variable ----
##
##******************************************************************************************************************************************

## observations against population predictions
dv_vs_pred(xp, type = "p", facets = "ObsName", subtitle = "-2LL: @ofv")

# observations against individual predictions
dv_vs_ipred(xp, type = "p", facets = "ObsName", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


##******************************************************************************************************************************************
##
##    CWRES against population predictions or independent variable for each observed variable ----
##
##******************************************************************************************************************************************

## CWRES against population predictions
res_vs_pred(xp, res = "CWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")

## CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")


##******************************************************************************************************************************************
##
##    |IWRES| against IPRED  ----
##
##******************************************************************************************************************************************

xp %>%
  set_var_types(pred = "IPRED") %>%
  absval_res_vs_pred(res = "IWRES", type = "ps", facets = "ObsName"
                     , title = "|IWRES| vs @x | @run"
                     , subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk"
                     ) +
  labs(y = "|IWRES|")


##******************************************************************************************************************************************
##
##    Individual plots  -----
##
##******************************************************************************************************************************************
##
##  Plots are facetted by both ID and observed variable name
##
##  Each page containing 2 x 2 plots with only the first page displayed
##
ind_plots(
  xp,
  facets = c("ID", "ObsName"),
  subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk",
  nrow = 2,
  ncol = 2,
  page = 1
)


##******************************************************************************************************************************************
##
##          Distribution plots of ETA  -----
##
##******************************************************************************************************************************************
eta_distrib(xp)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                     VPC   -----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates for VPC simulation
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0(ModelName, "_VPC"))

## View the model
print(modelVPC)


##******************************************************************************************************************************************
##
##      Run VPC  -----
##
##******************************************************************************************************************************************

## Run VPC using the default host, default values for the relevant NLME engine arguments, and default values for VPC arguments
vpcJob <- vpcmodel(modelVPC)


##******************************************************************************************************************************************
##
##      Observed and simulated data ------
##
##******************************************************************************************************************************************
##
## predcheck0 contains observed data for all continuous observed variables
##
dt_ObsData <- vpcJob$predcheck0

##
## predout contains simulated data for all observed variables
##
dt_SimData <- vpcJob$predout


##******************************************************************************************************************************************
##
##       Create VPC plots through "tidyvpc" package  -----
##
##  The "tidyvpc" package provides support for both continuous and categorical VPC using both binning and binless methods.
##  For details on this package, please visit the following link:
##
##              https://certara.github.io/tidyvpc/index.html
##
##  Next, we will demonstrate how to create VPC plots for this example through this package. Note that this example contains 2 observed variables.
##  Hence, to use this package to create VPC plots for each of them, we have to do some data preprocessing on both simulated
##  and observed data.
##
##******************************************************************************************************************************************
##
## Extract observed data for each observed variable
##
dt_ObsData_CObs <- dt_ObsData[ObsName == "CObs"]
dt_ObsData_EObs <- dt_ObsData[ObsName == "EObs"]


##
## Extract simulated data for each observed variable
##
dt_SimData_CObs <- dt_SimData[OBSNAME == "CObs"]
dt_SimData_EObs <- dt_SimData[OBSNAME == "EObs"]


### .......................................................................................................................................
###
###  binless VPC plots  ----
###
### .......................................................................................................................................
###
### Create a binless VPC plot for CObs
###
binlessVPC_CObs <- observed(dt_ObsData_CObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_CObs, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot_binlessVPC_CObs <- plot(binlessVPC_CObs)


###
### Create a binless VPC plot for EObs
###
binlessVPC_EObs <- observed(dt_ObsData_EObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_EObs, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot_binlessVPC_EObs <- plot(binlessVPC_EObs, legend.position = "none")


###
### Put these two plots in one page
###
egg::ggarrange(
  plot_binlessVPC_CObs,
  plot_binlessVPC_EObs, labels = c("CObs", "EObs"))


### .......................................................................................................................................
###
###  binning VPC plots  ----
###
### .......................................................................................................................................
###
### Create a binning VPC plot for CObs: binning on x-variable itself
###
binningVPC_CObs <- observed(dt_ObsData_CObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_CObs, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats()
plot_binningVPC_CObs <- plot(binningVPC_CObs)


###
### Create a binning VPC plot for EObs: binning on x-variable itself
###
binningVPC_EObs <- observed(dt_ObsData_EObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_EObs, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats()
plot_binningVPC_EObs <- plot(binningVPC_EObs, legend.positon = "none")


###
### Put these two plots in one page
###
egg::ggarrange(plot_binningVPC_CObs, plot_binningVPC_EObs, labels = c("CObs", "EObs"))


##******************************************************************************************************************************************
##
##    Create VPC plots through VPC results shiny app (in Certara.VPCResults package) ----
##
##
##     Alternatively, one can create/customize VPC plots through VPC results shiny app, which can also be used to
##
##          - generate corresponding tidyvpc code to reproduce the VPC ouput from R command line
##
##          - generate report as well as the associated R markdown.
##
##     Here we only demonstrate how to invoke this shiny app (Note: The shiny app will automatically preprocess the data as what we did
##     above for tidyvpc package).
##
##******************************************************************************************************************************************

library(Certara.VPCResults)
## Invoke VPC results shiny app to create VPC plots for CObs
##
taggedVPC_CObs <- vpcResultsUI(observed = dt_ObsData, simulated = dt_SimData, ObsName = "CObs")

##
## Invoke VPC results shiny app to create VPC plots for EObs
##
taggedVPC_EObs <- vpcResultsUI(observed = dt_ObsData, simulated = dt_SimData, ObsName = "EObs")
