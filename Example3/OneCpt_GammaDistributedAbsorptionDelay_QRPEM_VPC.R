#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                         Description
#
# The purpose of this file is to demonstrate how to
#
#   - create a gamma absorption delay model through built-in model library
#
#         * update structural parameter forms
#         * change initial values of fixed effects
#         * add secondary parameters
#
#   - fit the model
#
#   - create commonly used diagnostic plots through both command-line and mode results shiny app (in Certara.ModelResults package)
#
#   - perform VPC and create VPC plots through "tidyvpc" package (command-line) or VPC results shiny app (in Certara.VPCResults package)
#
# The model demonstrated is a one-compartment model with 1st-order clearance and the delay time between
# the administration time of the drug and the time when the drug molecules reach the central compartment
# is assumed to be gamma distributed. In other words, the model is described as follows:
#
#      dA1(t)/dt = sum_{i=1}^{m}D_{i}gammaPDF(t - t_{Di}; rateParameter, shapeParameter) - Cl/V * A1(t)
#
# Here
#
#    - A1 denotes the drug amount at central compartment with V and Cl respectively being the central volume
#      distribution and central clearance rate
#
#    - m denotes the number of bolus dosing events
#
#    - D_{i} is the amount of dose administered at time t_{Di} for the ith dosing events
#
#    - gammaPDF denotes the probability density function of a gamma distribution with with mean = MeanDelayTime
#		  and shape parameter being (ShapeParamMinusOne + 1).
#
# For more information on distribute delays, please see
# 	https://onlinehelp.certara.com/phoenix/8.3/index.html#t=topics%2FDiscrete_and_distributed_delays.htm%23XREF_92535_Discrete_and
#
#
# Note: To run this file, please set the working directory to the location where this file is located.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load libraries and set working directory
library(Certara.RsNLME)
library(magrittr)
library(Certara.Xpose.NLME)
library(xpose)

if (Sys.getenv("RSTUDIO") == 1) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  setwd("./Example3")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#     Model  -------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Load the input dataset
##
input_data <- read.csv("OneCpt_GammaDistributedAbsorptionDelay.csv")

##******************************************************************************************************************************************
##
##                            Create the model and define its associated column mappings ----
##
##******************************************************************************************************************************************
##
## Define a one-compartment model with gamma absorption delay and 1st-order clearance
##
##   - Set V = exp(tvlogV + nlogV)
##
##   - Set Cl = exp(tvlogCl + nlogCl)
##
##   - Set MeanDelayTime = exp(tvlogMeanDelayTime + nlogMeanDelayTime)
##
##   - Set ShapeParamMinusOne = exp(tvlogShapeParamMinusOne)
##
##   - Change initial values for fixed effects " tvlogV " and "tvlogShapeParamMinusOne" to be 5 and 2, respectivel (the default value is 1)
##
##   - Add secondary parameter "tvV", which is set to tvV = exp(tvlogV)
##
##   - Add secondary parameter "tvCl", which is set to tvCl = exp(tvlogCl)
##
##   - Add secondary parameter "tvMeanDelayTime", which is set to tvMeanDelayTime = exp(tvlogMeanDelayTime)
##
##   - Add secondary parameter "tvShapeParam", which is set to tvShapeParam = exp(tvlogShapeParamMinusOne) + 1
##
##
model <- pkmodel(absorption = "Gamma",
                 data = input_data,
                 columnMap = FALSE,
                 modelName = "OneCpt_GammaDistributedAbsorptionDelay_QRPEM"
                 ) %>%
  structuralParameter(paramName = "V", style = "LogNormal2", fixedEffName = "tvlogV", randomEffName = "nlogV") %>%
  structuralParameter(paramName = "Cl", style = "LogNormal2", fixedEffName = "tvlogCl", randomEffName = "nlogCl") %>%
  structuralParameter(paramName = "MeanDelayTime", style = "LogNormal2", fixedEffName = "tvlogMeanDelayTime", randomEffName = "nlogMeanDelayTime") %>%
  structuralParameter(paramName = "ShapeParamMinusOne", style = "LogNormal2", fixedEffName = "tvlogShapeParamMinusOne", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvlogV", "tvlogShapeParamMinusOne"), value = c(5, 2)) %>%
  addSecondary(name = "tvV", definition = "exp(tvlogV)") %>%
  addSecondary(name = "tvCl", definition = "exp(tvlogCl)") %>%
  addSecondary(name = "tvMeanDelayTime", definition = "exp(tvlogMeanDelayTime)") %>%
  addSecondary(name = "tvShapeParam", definition = "exp(tvlogShapeParamMinusOne) + 1")

##
## View the model and its associated column mappings
##
print(model)

##
## Manually map those un-mapped model variables to their corresponding input data columns
##
model <- model %>%
  colMapping(c(A1 = "Dose"))

##
## View updated column mappings
##
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                          Model fitting  -----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##
## Run the model with the default host
##
## Engine arguments are defined through ellipsis (additional argument)
##
##    - Set the engine method to be QPREM
##
##    - Use the default value for all the other relevant arguments
##
job <- fitmodel(model, method = "QRPEM")


## View estimation results
print(job$Overall)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                      Diagnostic plots ------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


##******************************************************************************************************************************************
##
##   Command-line -----
##
##      All the functions provided in the xpose package can be used. Here we only demonstrate several of these functions.
##
##******************************************************************************************************************************************
## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
xp <- xposeNlmeModel(model, job)


### .......................................................................................................................................
###
### Observations against population or individual predictions ----
###
### .......................................................................................................................................

## observations against population predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")

## observations against individual predictions
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


### .......................................................................................................................................
###
###    CWRES against population predictions (PRED) or independent variable ----
###
### .......................................................................................................................................

## CWRES against population predictions
res_vs_pred(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")

## CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")


### .......................................................................................................................................
###
###           Individual plots  -----
###
### .......................................................................................................................................

## Observations, individual predictions and population predictions plotted against the independent variable for every individual
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


### .......................................................................................................................................
###
###          Distribution plots of ETA  -----
###
### .......................................................................................................................................

eta_distrib(xp)


##******************************************************************************************************************************************
##
##    Mode results shiny app (in Certara.ModelResults package) -----
##
##        Alternatively, one can view/customize diagnostic plots as well as estimation results through model results shiny app, which can
##        also be used to to generate R script and report as well as the associated R markdown.
##        For details on this app as well as how to use it, please visit the following link
##
##               https://certara.github.io/R-model-results/index.html.
##
##        Here we only demonstrate how to invoke this shiny app through either model object or the xpose data bases created above.
##
##******************************************************************************************************************************************
##
## Invoke model results shiny app through model object defined above
##
library(Certara.ModelResults)
resultsUI(model)

##
## Alternatively, one can invoke model results shiny app through xpose data bases created above
##
resultsUI(xpdb = xp)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                      VPC   -------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates for VPC simulation
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = "OneCpt_GammaDistributedAbsorptionDelay_VPC")

## View model
print(modelVPC)


##******************************************************************************************************************************************
##
##                                Run VPC for the model  -----
##
##******************************************************************************************************************************************

## Run VPC using the default host, default values for the relevant NLME engine arguments, and default values for VPC arguments
vpcJob <- vpcmodel(modelVPC)


##******************************************************************************************************************************************
##
##                               Observed and simulated data ----
##
##******************************************************************************************************************************************
## Observed data
dt_ObsData <- vpcJob$predcheck0

## Simulated data
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
##  Next, we will demonstrate how to create VPC plots through this package. Note that this example only contains one observed variable
##  and both simulated and observed data meet the requirements set by tidyvpc package. Hence, there is no need to do any data preprocessing
##  for this example.
##
##******************************************************************************************************************************************

library(tidyvpc)
## Create a binless VPC plot
##
plot_binless_vpc <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot(plot_binless_vpc)

##
## Create a binning VPC plot: binning on x-variable itself
##
plot_binning_vpc <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binning(bin = IVAR) %>%
  vpcstats()
plot(plot_binning_vpc)

## Compare plots
library(egg)
ggarrange(
  plot(plot_binless_vpc),
  plot(plot_binning_vpc, legend.position = "none"),
  labels = c("Binless", "Binning")
)

##******************************************************************************************************************************************
##
##    Create VPC plots through VPC results shiny app (in Certara.VPCResults package) ----
##
##
##        Alternatively, one can create/customize VPC plots through VPC results shiny app, which can also be used to
##
##          - generate corresponding tidyvpc code to reproduce the VPC ouput from R command line
##
##          - generate report as well as the associated R markdown.
##
##        Here we only demonstrate how to invoke this shiny app.
##
##******************************************************************************************************************************************
##
library(Certara.VPCResults)
vpcResultsUI(observed = dt_ObsData, simulated = dt_SimData)
