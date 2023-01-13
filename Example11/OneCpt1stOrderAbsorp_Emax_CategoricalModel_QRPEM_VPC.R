#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                         Description
#
# The purpose of this file is to demonstrate how to
#
#   - fit a model defined by PML codes, where
#
#        * the PK portion of the model is described by a one-compartment model with first-order absorption,
#
#        * the PD portion of the model is described by an Emax model and a categorical model with three categories
#
#     See "OneCpt1stOrderAbsorp_Emax_CategoricalModel.mdl" in the same directory where this file is located for details.
#
#   - Import estimation results to xpose database to create some commonly used diagnostic plots for each continuous observed variable
#
#   - Perform VPC for the model
#
#   - Create VPC plots through open source package "tidyvpc" (command-line usage) and VPC results shiny app (in Certara.VPCResults package)
#
#
# Note: To run this file, please set the working directory to the location where this file is located.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load libraries and set working directory
library(Certara.RsNLME)
library(magrittr)
setwd("./Example11")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#    Model ----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Load the input data
input_data <- read.csv("OneCpt1stOrderAbsorp_Emax_CategoricalModel.csv")

##******************************************************************************************************************************************
##
##       Load the PML codes to create a R model object and define the associated column mappings -----
##
##******************************************************************************************************************************************

# Name of PML code file and its location
sourceCode <- file.path(getwd(), "OneCpt1stOrderAbsorp_Emax_CategoricalModel.mdl")

# Load the PML codes and link it to associated input data to create a model object
model <- textualmodel(modelName = "OneCpt1stOrderAbsorp_Emax_CategoricalModel_QRPEM",
                      mdl = sourceCode,
                      data = input_data)

# View the model and its associated column mappings
print(model)

# Manually map the un-mapped model variables to their corresponding input data columns
model <- model %>%
  colMapping(id = SubID, Aa = dose_Aa) # In RsNLME >= 1.2.0, unquoted column names are supported


# View the model and its associated column mappings
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                           Model fitting -----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Engine argument setup
##
##  - Set the engine method to be QPREM
##
##  - Output residuals PCWRES with 1000 replicates used to generate PCWRES (Note: PCWRES is not outputted by default)
##
##  - Use default values for all the relevant arguments
##
engineSetup <- engineParams(model, method = "QRPEM", numRepPCWRES = 1000)


##
## Run the model using the default host with engine arguments defined above
##
## Note: Alternatively one can define engine arguments through ellipsis (additional argument) in fitmodel as what is shown below
##
##          job <- fitmodel(model, method = "QRPEM", numRepPCWRES = 1000)
##
job <- fitmodel(model, params = engineSetup)


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
library(Certara.Xpose.NLME)
library(xpose)

xp <- xposeNlmeModel(model, job)

##
## Filter out CategoricalObs
##
xp <- xp %>%
  filter(ObsName != "CategoricalObs")

##******************************************************************************************************************************************
##
##      Observations against population or individual predictions for each continuous observed variable ----
##
##******************************************************************************************************************************************

## observations against population predictions
dv_vs_pred(xp, type = "p", facets = "ObsName", subtitle = "-2LL: @ofv")

# observations against individual predictions
dv_vs_ipred(xp, type = "p", facets = "ObsName", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")

##******************************************************************************************************************************************
##
##    CWRES against population predictions (PRED) or independent variable for each continuous observed variable ----
##
##******************************************************************************************************************************************

## CWRES against population predictions
res_vs_pred(xp, res = "CWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")

## CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")


##******************************************************************************************************************************************
##
##    PCWRES against population predictions (PRED) or independent variable for each continuous observed variable ----
##
##******************************************************************************************************************************************
## PCWRES against population predictions
res_vs_pred(xp, res = "PCWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")

## PCWRES against the independent variable
res_vs_idv(xp, res = "PCWRES", type = "ps", facets = "ObsName", subtitle = "-2LL: @ofv")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                       VPC  ------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates for VPC simulation
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = "OneCpt1stOrderAbsorp_Emax_CategoricalModel_VPC")

## View model
print(modelVPC)


##******************************************************************************************************************************************
##
##      Run VPC ------
##
##******************************************************************************************************************************************

##
## Run VPC with PRED outputted using the default host and default values for the relevant NLME engine arguments
##
## Note: Here VPC argument, outputPRED, is defined through ellipsis (additional argument).
##       Alternatively, one can define VPC arguments through vpcParams argument (type ?vpcmodel for more information on this)
##
vpcJob <- vpcmodel(modelVPC, outputPRED = TRUE)


##******************************************************************************************************************************************
##
##      Observed and simulated data ------
##
##******************************************************************************************************************************************
## predcheck0 contains observed data for all continuous observed variables
dt_ObsData_ContinuousObs <- vpcJob$predcheck0

## predcheck0_cat contains observed data for Categorical/count observed variables
dt_ObsData_CategoricalObs <- vpcJob$predcheck0_cat

##
## predout contains simulated data for all observed variables
##
##  Note: If PRED is requested to be outputted (i.e., outputPRED = TRUE),
##  then it also contains PRED column with population predictions for continuous observed variables
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
##  Next, we will demonstrate how to create these VPC plots through this package. Note that this example contains 3 observed variables with
##  PRED outputted. Hence, to use this package, we have to do some data preprocessing on both simulated and observed data to meet the
##  requirements set by the tidyvpc package.
##
##******************************************************************************************************************************************
##
## Clean the simulated data to pass to the "simulated" function in tidyvpc package
##
##      Extract simulated data for observed variable "CObs"
dt_SimData_tidyvpc_CObs <- dt_SimData[OBSNAME == "CObs"]
##      Extract simulated data for observed variable "EObs"
dt_SimData_tidyvpc_EObs <- dt_SimData[OBSNAME == "EObs"]
##      Extract simulated data for observed variable "CategoricalObs"
dt_SimData_tidyvpc_CategoricalObs <- dt_SimData[OBSNAME == "CategoricalObs"]


##
## Clean the observed data to pass to the "observed" function in tidyvpc package
##
##      Extract observed data for observed variable "CObs"
dt_ObsData_ContinuousObs_tidyvpc_CObs <- dt_ObsData_ContinuousObs[ObsName == "CObs"]
##      Extract PRED column from sim data
dt_ObsData_ContinuousObs_tidyvpc_CObs$PRED <- as.numeric(dt_SimData_tidyvpc_CObs[REPLICATE == 0]$PRED)

##      Extract observed data for observed variable "EObs"
dt_ObsData_ContinuousObs_tidyvpc_EObs <- dt_ObsData_ContinuousObs[ObsName == "EObs"]
##      Extract PRED column from sim data
dt_ObsData_ContinuousObs_tidyvpc_EObs$PRED <- as.numeric(dt_SimData_tidyvpc_EObs[REPLICATE == 0]$PRED)
##      Extract observed data for observed variable "CategoricalObs"
dt_ObsData_CategoricalObs_tidyvpc <- dt_ObsData_CategoricalObs[ObsName == "CategoricalObs"]

library(tidyvpc)
### .......................................................................................................................................
###
### binless VPC plots ----
###
### .......................................................................................................................................
###
### Create a binless VPC plot for CObs
###
binless_VPC_CObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_CObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_CObs, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot(binless_VPC_CObs)

###
### Create a binless pcVPC plot for CObs
###
binless_pcVPC_CObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_CObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_CObs, ysim = DV) %>%
  predcorrect(pred = PRED) %>%
  binless(loess.ypc = TRUE) %>%
  vpcstats()
plot(binless_pcVPC_CObs)


###
### Create a binless VPC plot for EObs
###
binless_VPC_EObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_EObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_EObs, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot(binless_VPC_EObs)


###
### Create a binless pcVPC plot for EObs
###
binless_pcVPC_EObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_EObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_EObs, ysim = DV) %>%
  predcorrect(pred = PRED) %>%
  binless(loess.ypc = TRUE) %>%
  vpcstats()
plot(binless_pcVPC_EObs)


###
### Create a binless VPC plot for CatgoricalObs
###
binless_VPC_CategoricalObs <- observed(dt_ObsData_CategoricalObs_tidyvpc, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_CategoricalObs, ysim = DV) %>%
  binless() %>%
  vpcstats(vpc.type = "categorical")

plot(binless_VPC_CategoricalObs
     , facet = TRUE
     , facet.scales = "fixed"
     , legend.position = "bottom"
     )

### .......................................................................................................................................
###
### binning VPC plots ----
###
### .......................................................................................................................................
###
### Create a binning VPC plot for CObs: binning on x-variable itself
###
binning_VPC_CObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_CObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_CObs, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats()
plot(binning_VPC_CObs)

###
### Create a binning pcVPC plot for CObs: binning on x-variable itself
###
binning_pcVPC_CObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_CObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_CObs, ysim = DV) %>%
  binning(bin = IVAR) %>%
  predcorrect(pred = PRED) %>%
  vpcstats()
plot(binning_pcVPC_CObs)


###
### Create a binning VPC plot for EObs: binning on x-variable itself
###
binning_VPC_EObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_EObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_EObs, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats()
plot(binning_VPC_EObs)


###
### Create a binning pcVPC plot for EObs: binning on x-variable itself
###
binning_pcVPC_EObs <- observed(dt_ObsData_ContinuousObs_tidyvpc_EObs, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_EObs, ysim = DV) %>%
  binning(bin = IVAR) %>%
  predcorrect(pred = PRED) %>%
  vpcstats()
plot(binning_pcVPC_EObs)


###
### Create a binning VPC plot for CatgoricalObs: binning on x-variable itself
###
binning_VPC_CategoricalObs <- observed(dt_ObsData_CategoricalObs_tidyvpc, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc_CategoricalObs, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats(vpc.type = "categorical")

plot(binning_VPC_CategoricalObs
     , facet = TRUE
     , facet.scales = "fixed"
     , legend.position = "bottom"
     )

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
##
## Invoke VPC results shiny app to create VPC plots for CObs
##
library(Certara.VPCResults)
taggedVPC_CObs <- vpcResultsUI(observed = dt_ObsData_ContinuousObs, simulated = dt_SimData, ObsName = "CObs")

##
## Invoke VPC results shiny app to create VPC plots for EObs
##
taggedVPC_EObs <- vpcResultsUI(observed = dt_ObsData_ContinuousObs, simulated = dt_SimData, ObsName = "EObs")

##
## Invoke VPC results shiny app to create  VPC plot for categoricalObs
##
taggedVPC_CategoricalObs <- vpcResultsUI(observed = dt_ObsData_CategoricalObs
                                         , simulated = dt_SimData
                                         , ObsName = "CategoricalObs"
                                         , vpc.type = "categorical"
                                         )
