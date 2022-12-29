####################################################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to
##
##    - Define the base model (described by a two-compartment model with IV bolus) through built-in model library,
##      fit this model to the data, create commonly used diagnostic plots, and perform VPC and then create binnless
##      and binned VPC plots through open source package "tidyvpc".
##
##        * Load the input dataset and visualize the data
##
##        * Define the base model as well as mapping model variables to their corresponding input data columns
##
##        * Fit the base model
##
##        * Import estimation results to xpose database to create commonly used diagnostic plots
##
##        * Perform VPC for the base model and create binnless and binned VPC plots through open source package "tidyvpc"
##
##    - Add covariates to the base model and then identify covariates through a stepwise covariate search.
##
##    - Perform VPC for the model selected by the covariate search procedure, and create pcVPC plot through open source
##      package "tidyvpc".
##
##    - Perform Bootstrapping analysis for the model selected by the covariate search procedure.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
##
#####################################################################################################################################

# Load libraries and set working directory
library(Certara.RsNLME)
library(magrittr)
setwd("./Example12")

##===================================================================================================================================
##                      Load the input dataset and visualize the data
##===================================================================================================================================

## Set the input dataset to the built-in data "pkData"
input_data <- pkData


library(dplyr)
library(ggplot2)
## Graph drug concentration vs. time
conc_time_plot <- input_data %>%
  mutate(Subject = as.factor(Subject)) %>%
  ggplot(aes(
    x = Act_Time,
    y = Conc,
    group = Subject,
    color = Subject
  )) +
  scale_y_log10() +
  geom_line() +
  geom_point() +
  ylab("Drug Concentration \n at the central compartment")

print(conc_time_plot)


##======================================================================================================================================
##                    Define the base model and its associated column mappings
##======================================================================================================================================

## Define a basic two-Compartment population PK model with IV bolus (through built-in function "pkmodel")
## as well as its associated column mappings
## Help is available by typing ?pkmodel
model <- pkmodel(
  numCompartments = 2,
  data = input_data,
  ID = "Subject",
  Time = "Act_Time",
  A1 = "Amount",
  CObs = "Conc",
  modelName = "TwCpt_IVBolus_FOCE-ELS"
)

## View the model
print(model)


## Disable the corresponding random effects for structural parameter V2
## Change initial values for fixed effects, tvV, tvCl, tvV2, and tvCl2, to be 15, 5, 40, and 15, respectively
## Change the covariance matrix of random effects, nV, nCl, and nCl2, to be a diagonal matrix with all its diagonal elements being 0.1
## Change the standard deviation of residual error to be 0.2
model <- model %>%
  structuralParameter(paramName = "V2", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvV", "tvCl", "tvV2", "tvCl2"), value = c(15, 5, 40, 15)) %>%
  randomEffect(effects = c("nV", "nCl", "nCl2"), values = rep(0.1, 3)) %>%
  residualError(predName = "C", SD = 0.2)


## View the updated model
print(model)

##=======================================================================================================================================
##                   Fit the base model
##=======================================================================================================================================

## Fit the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
baseFitJob <- fitmodel(model)


## View estimation results
print(baseFitJob[c("Overall", "theta", "omega")])

##===================================================================================================================================
##                    Create diagnostic plots for the base model
##===================================================================================================================================

library(Certara.Xpose.NLME)
library(xpose)
## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
# Using xposeNlme() function, which uses run directory in file system to import results
xp <- xposeNlme(dir = model@modelInfo@workingDir, modelName = "TwCpt_IVBolus_FOCE-ELS")

# Using xposeNlmeModel() function, which uses model and job objects from R environment to import results
xp <- xposeNlmeModel(model, baseFitJob)

##  Observations against population or individual predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


## CWRES against population predictions or independent variable
res_vs_idv(xp, res = "CWRES",type = "ps", subtitle = "-2LL: @ofv")
res_vs_pred(xp, res = "CWRES",type = "ps", subtitle = "-2LL: @ofv")

## QQ plot of CWRES
res_qq(xp, res = "CWRES")

##  Observations, individual predictions and population predictions plotted against the independent variable for every individual
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")

##===================================================================================================================================
##                     Perform VPC for the base model
##===================================================================================================================================

## Copy the model into a new object, and accept final parameter estimates from fitting run
## as initial estimates for VPC simulation
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0("TwCpt_IVBolus_FOCE-ELS", "_VPC"))

## View the model
print(modelVPC)

## -----------------------------------------------------------------------------------------------------------------------
## Run VPC
## -----------------------------------------------------------------------------------------------------------------------

## Run VPC using the default host, default values for the relevant NLME engine arguments, and default values for VPC arguments
baseVPCJob <- vpcmodel(modelVPC)


## -----------------------------------------------------------------------------------------------------------------------
## Create binless and binned VPC plots through open source package "tidyvpc"
## -----------------------------------------------------------------------------------------------------------------------

## Simulation input dataset: predcheck0.csv
dt_ObsData <- baseVPCJob$predcheck0


## Simulation output dataset: predout.csv
dt_SimData <- baseVPCJob$predout

library(tidyvpc)
## Create a binless VPC plot
binless_vpc <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot_binless_vpc <- plot(binless_vpc)


## Create a binned VPC plot with binning method set to be "jenks"
binned_vpc <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binning(bin = "jenks") %>%
  vpcstats()
plot_binned_vpc <- plot(binned_vpc)


## Put these two plots side-by-side
egg::ggarrange(plot_binless_vpc, plot_binned_vpc, nrow = 1, ncol = 2, labels = c("binless VPC", "binned VPC"))


###################################################################################################################################

###################             Stepwise Covariate search                                                 ########################

###################################################################################################################################

## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates
covariateModel <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0("TwCpt_IVBolus_FOCE-ELS", "_CovariateSearch"))

## View the model
print(covariateModel)


#===================================================================================================================================
#       Add covariates to the base model
#===================================================================================================================================
##
## Add covariate "Sex" to structural parameter "V" and "Cl", and map "Sex" to its corresponding column, Gender, in the input dataset
## Add covariate "Age" to structural parameter "V" and "Cl" (and "Age" is automatically mapped to its corresponding column, Age, in the input dataset
## Add covariate "BW" to structural parameters "V" and "Cl", and map "BW" to its corresponding column, BodyWeight, in the input dataset
##
covariateModel <- covariateModel %>%
  addCovariate(covariate = c(Sex = "Gender"), effect = c("V", "Cl"), type = "Categorical", levels = c(0, 1), labels = c("female", "male")) %>%
  addCovariate(covariate = "Age", effect = c("V", "Cl")) %>%
  addCovariate(covariate = c(BW = "BodyWeight"), effect = c("V", "Cl"), center = "Value", centerValue = 70)

## View the updated model
print(covariateModel)

#===================================================================================================================================
#       Run stepwise covariate search
#===================================================================================================================================

## Run the stepwise covariate search using the default multicore host, default values for all the relevant NLME engine arguments,
## and default values for stepwise covariate search arguments
covariateJob <- stepwiseSearch(covariateModel)


##===================================================================================================================================
##     Load and view results from stepwise covariate search
##===================================================================================================================================

## View the reports for all the model fit diagnostic
print(covariateJob)

## Load and view the model selected by the stepwise covariate search
stepwiseLines <- readLines(paste0(covariateModel@modelInfo@workingDir,"/Stepwise.txt"))
print(stepwiseLines[grep("Scenario to use = ", stepwiseLines)])


###################################################################################################################################

###################     Fit the selected covariate model and then perform VPC analysis                ########################

###################################################################################################################################

## Keep only those covariate effects selected by the stepwise covariate search
selectedCovariateModel <- covariateModel %>%
  removeCovariate(covariate = "Age") %>%
  removeCovariate(covariate = "Sex")

## model name for the final selected covariate model

## Copy the model into a new object
finalModel <- copyModel(selectedCovariateModel, modelName = "TwCpt_IVBolus_SelectedCovariateModel_FOCE-ELS")

## View the selected covariate model
print(finalModel)


##===================================================================================================================================
##      Fit the selected covariate model
##===================================================================================================================================

## Fit the selected covariate model using the default host and default values for the relevant NLME engine arguments
finalFitJob <- fitmodel(finalModel)

## View estimation results
print(finalFitJob)


##===================================================================================================================================
##                    Create diagnostic plots for the final model
##===================================================================================================================================

## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
# Using xposeNlmeModel() function, which uses model and job objects from R environment to import results
xp <- xposeNlmeModel(finalModel, finalFitJob)


##  Observations against population or individual predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


## CWRES against population predictions or independent variable
res_vs_idv(xp, res = "CWRES",type = "ps", subtitle = "-2LL: @ofv")
res_vs_pred(xp, res = "CWRES",type = "ps", subtitle = "-2LL: @ofv")


## CWRES against covariate
res_vs_cov(xp, res = "CWRES", covariate = "BW", type = "ps")

##  Observations, individual predictions and population predictions plotted against the independent variable for every individual
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


##================================================================================================================================
##    Perform VPC for the final selected covariate model
##================================================================================================================================

## Copy the final selected covariate model into a new object, and accept final parameter estimates from fitting run
## as initial estimates for VPC simulation
finalModelVPC <- copyModel(finalModel, acceptAllEffects = TRUE, modelName = paste0("Final_TwCpt_IVBolus_FOCE-ELS", "_VPC"))

## View the model
print(finalModelVPC)

## -----------------------------------------------------------------------------------------------------------------------
##  Run VPC
## -----------------------------------------------------------------------------------------------------------------------

## Set up VPC augments to have PRED outputted to simulation output dataset "predout.csv"
## Note: To do this, one has to set outputPRED = TRUE and predCOrrection to be NOT "none"
vpcSetup <- NlmeVpcParams(predCorrection = "proportional", outputPRED = TRUE)

## Run VPC using the default host, default values for the relevant NLME engine arguments
finalVPCJob <- vpcmodel(model = finalModelVPC, outputPRED = TRUE)


## -----------------------------------------------------------------------------------------------------------------------
## Create VPC and pcVPC plots through open source package "tidyvpc"
## -----------------------------------------------------------------------------------------------------------------------

## Simulation input dataset predcheck0.csv
dt_ObsData <- finalVPCJob$predcheck0


## Simulation output dataset predout.csv
## Note: We will extract PRED column from REPLICATE = 0 and add to observed data, required for pcVPC in tidyvpc
dt_SimData <- finalVPCJob$predout

## Add PRED from REPLICATE = -1 of simulation output dataset to simulation input dataset
dt_ObsData$PRED <- dt_SimData[REPLICATE == 0]$PRED


## Create a regular VPC plot with binning method set to be "jenks"
binned_VPC <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binning(bin = "jenks") %>%
  vpcstats()
plot_binned_VPC <- plot(binned_VPC)


## Create a pcVPC plot with binning method set to be "jenks"
binned_pcVPC <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binning(bin = "jenks") %>%
  predcorrect(pred = PRED) %>%
  vpcstats()
plot_binned_pcVPC <- plot(binned_pcVPC)


## Put these two plots in a page
egg::ggarrange(plot_binned_VPC, plot_binned_pcVPC, labels = c("VPC", "pcVPC"))


###################################################################################################################################

###################    Perform bootstrapping analysis for the selected covariate model               ########################

###################################################################################################################################

## Copy the selected covariate model into a new object
finalModelBootstrap <- copyModel(finalModel, modelName = paste0("TwCpt_IVBolus_FOCE-ELS", "_Bootstrapping"))


## Run the boostrap using the default multicore host, default values for all the relevant NLME engine arguments,
## and default values for bootstrap arguments (running "BootstrapParams()" to view the default values)
finalBootstrapJob <- bootstrap(finalModelBootstrap)


## View bootstrapping estimation results for Theta, sigma, and Omega
print(finalBootstrapJob$BootTheta)
print(finalBootstrapJob$BootOmega)
print(finalBootstrapJob$BootOmegaStderr)

