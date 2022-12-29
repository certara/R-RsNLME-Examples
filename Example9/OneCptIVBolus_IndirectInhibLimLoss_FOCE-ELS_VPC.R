###############################################################################################################################
##                              Description
##
## This example involves both PK and PD, where PK is described by a one-compartment model with IV bolus, and
## PD is described by an indirect model with the loss of a mediator inhibited.
##
##  - Create the model through build-in model library and define the associated column mappings
##
##  - Fit the model
##
##  - Import estimation results to xpose database to create commonly used diagnostic plots
##
##  - Perform VPC for the final model
##
##  - Create VPC plots through open source package "vpc" or "tidyvpc"
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
##
################################################################################################################################

## Load necessary packages, and set up working directory
library(Certara.RsNLME)
library(magrittr)
setwd("./Example9")

################################################################################################################################

###############      Load the input dataset, and define the model and the associated column mapping      ######################

#################################################################################################################################


## ================================================================================================================================
##                             Load the input dataset
## ================================================================================================================================

input_data <- read.csv("OneCptIVBolus_IndirectInhibLimLoss.csv")

# =================================================================================================================================
#                           Define the model and its associated column mappings
# =================================================================================================================================
# Define a basic PK/Indirect model along with the associated column mappings
model <-
  pkindirectmodel(
    indirectType = "LimitedInhibition",
    isBuildup = FALSE,
    data = input_data,
    columnMap = FALSE,
    modelName = "OneCptIVBolus_IndirectInhibLimLoss_FOCE-ELS"
  )

# Most variables were auto-mapped, map remaining A1 model variables to Dose column
model <- model %>%
  colMapping(A1 = Dose)

print(model)
## ----------------------------------------------------------------------------------------------------------------------------------
## Reset the forms of structural model parameters as well as initial values for Theta and Omega
##
##   - Set Imax = ilogit(tvlogitImax) with ilogit used to make sure it is between 0 and 1
##
##   - set IC50 = tvIC50
##
##   - reset initial values for fixed effects "tvCl", "tvKin", "tvKout" (the default value is 1)
##
##   - Reset the covariance matrix of the random effects to be a diagonal matrix with all its diagonal elements being 0.1
##
## Reset the residual error models for EObs to be multiplicative with standard deviation of residual error to be 0.1
# ----------------------------------------------------------------------------------------------------------------------------------
model <- model %>%
  structuralParameter(paramName = "Imax", style = "LogitNormal", fixedEffName = "tvlogitImax", hasRandomEffect = FALSE) %>%
  structuralParameter(paramName = "IC50", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvCl", "tvKin", "tvKout"), value = c(0.5, 10, 0.5)) %>%
  randomEffect(effect = c("nV", "nCl", "nKin", "nKout"),  value = rep(0.1, 4)) %>%
  residualError(predName = "E", errorType = "Multiplicative", SD = 0.1)


## View the model
print(model)


###################################################################################################################################

###################                    Model Fitting                                              ################################

###################################################################################################################################

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)

## View estimation results
print(job[c("Overall", "theta", "omega")])

###################################################################################################################################

#####################                      Diagnostic plots                            ##########################################

###################################################################################################################################

## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
library(Certara.Xpose.NLME)
library(xpose)
# Using xposeNlme() function, which uses run directory in file system to import results
xp <- xposeNlme(dir = model@modelInfo@workingDir, modelName = "OneCptIVBolus_IndirectInhibLimLoss_FOCE-ELS")

# Using xposeNlmeModel() function, which uses model and job objects from R environment to import results
xp <- xposeNlmeModel(model, job)


##================================================================================================================================
##      Observations against population or individual predictions for each observed variable
##================================================================================================================================

## observations against population predictions
dv_vs_pred(xp, type = "p", facets = "ObsName", subtitle = "-2LL: @ofv")

# observations against individual predictions
dv_vs_ipred(xp, type = "p", facets = "ObsName", subtitle = "-2LL: @ofv")

##================================================================================================================================
##    Residuals against population predictions or independent variable for each observed variable
##================================================================================================================================

## CWRES against population predictions
res_vs_pred(xp, res = "CWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")

## CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")


##================================================================================================================================
##          Distribution plots of ETA
##================================================================================================================================
eta_distrib(xp)


##################################################################################################################################

###################                     VPC                                                                ######################

#################################################################################################################################

## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates for VPC simulation
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0("OneCptIVBolus_IndirectInhibLimLoss", "_VPC"))

## View the model
print(modelVPC)


##================================================================================================================================
##                                   Run VPC for the model
##================================================================================================================================

## Run VPC using the default host, default values for the relevant NLME engine arguments, and default values for VPC arguments
vpcJob <- vpcmodel(modelVPC)

##================================================================================================================================
##                             Using vpc or tidyvpc library to do the VPC plots
##================================================================================================================================

## Simulation input dataset, predcheck0.csv, has all the observations for all the observed variables in one column
obs_data <- vpcJob$predcheck0

## Clean the simulation input dataset to make it ready to create VPC plots through "vpc" or "tidyvpc" package
library(dplyr)
obs_data <- obs_data %>%
  rename(TIME = IVAR,
         ID = ID5)

obs_data_CObs <- obs_data %>%
  filter(ObsName == "CObs")

obs_data_EObs <- obs_data %>%
  filter(ObsName == "EObs")


## Simulation output dataset, predout.csv, has all the observations for all the observed variables in one column
sim_data <- vpcJob$predout

## Clean the simulation output dataset to make it ready to create VPC plots through the "tidyvpc" package
sim_data <- sim_data %>%
  rename(TIME = IVAR,
         ID = ID5)

sim_data_CObs <- obs_data %>%
  filter(ObsName == "CObs")

sim_data_EObs <- obs_data %>%
  filter(ObsName == "EObs")

## -----------------------------------------------------------------------------------------------------------------------
## Create binless VPC plots through "tidyvpc" package
## -----------------------------------------------------------------------------------------------------------------------

library(tidyvpc)
binlessVPC_CObs <- observed(obs_data_CObs, x = TIME, yobs = DV) %>%
  simulated(sim_data_CObs, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot_binlessVPC_CObs <- plot(binlessVPC_CObs)

binlessVPC_EObs <- observed(obs_data_EObs, x = TIME, yobs = DV) %>%
  simulated(sim_data_EObs, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot_binlessVPC_EObs <- plot(binlessVPC_EObs)

egg::ggarrange(plot_binlessVPC_CObs, plot_binlessVPC_EObs)
