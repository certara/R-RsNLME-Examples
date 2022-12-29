#################################################################################################################################
#                         Description
#
# The purpose of this file is to demonstrate how to
#
#   - fit a model defined by PML codes (see "OneCpt_GammaDistributedAbsorptionDelay.mdl" in the same directory
#     where this file is located) to the input data defined in the csv file in this directory
#
#   - create commonly used diagnostic plots
#
#   - perform VPC and create VPC plots through "tidyvpc" package
#
# Note: To run this file, please set the working directory to the location where this file is located.
#
##################################################################################################################################

## Set up environment variables, load necessary packages
library(Certara.RsNLME)
library(magrittr)
setwd("./Example3")


##================================================================================================================================
##       Load the input data
##================================================================================================================================

input_data <- read.csv("OneCpt_GammaDistributedAbsorptionDelay.csv")

##================================================================================================================================
##       Use textualmodel function with path to .mdl file
##================================================================================================================================

## Load the PML codes and link it to associated input data to create a model object
model <- textualmodel(modelName = "OneCpt_GammaDistributedAbsorptionDelay_QRPEM",
                      mdl = "OneCpt_GammaDistributedAbsorptionDelay.mdl",
                      data = input_data)

## Check the mapping between model variables and input data columns
print(model)
## Manually map the rest of model variables to its corresponding input data columns

model <- model %>%
  colMapping(A1 = Dose) # RsNLME v1.2.0 now supports unquoted column names in colMapping()

print(model)
##================================================================================================================================
##                          Model fitting
##================================================================================================================================


## Define local mpi host
local_mpi_host <- hostParams(hostName = "Local_MPI", parallelMethod = "LOCAL_MPI", numCores = 4)

## Run the model, passing the `method` argument from engineParams() using the `...`
job <- fitmodel(model, method = "QRPEM", hostPlatform = local_mpi_host)

## View estimation results
print(job[c("Overall", "theta", "omega")])

##================================================================================================================================
##                      Diagnostic plots
##================================================================================================================================

## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
xp <- xposeNlme(dir = model@modelInfo@workingDir, modelName = modelName)


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


# Distribution plots of ETA
eta_distrib(xp)


##################################################################################################################################

###################                      VPC                                                                #####################

##################################################################################################################################

## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates for VPC simulation
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0(modelName, "_VPC"))

## View model
print(modelVPC)


##===================================================================================================================================
##                                   Run VPC for the model
##===================================================================================================================================

## Run VPC using the default host, default values for the relevant NLME engine arguments, and default values for VPC arguments
vpcJob <- vpcmodel(modelVPC)

##================================================================================================================================
##                             Using "tidyvpc" package to do the VPC plots
##================================================================================================================================
## Simulation input data
dt_ObsData <- vpcJob$predcheck0.csv

## Simulation output dataset, predout.csv
dt_SimData <- vpcJob$predout.csv


## Load tidyvpc package
library(tidyvpc)

## Create a binless VPC plot through "tidyvpc" package
plot_binless_vpc <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot(plot_binless_vpc)




