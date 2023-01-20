#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                                 Description
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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load libraries and set working directory
library(Certara.RsNLME)
library(magrittr)
library(Certara.Xpose.NLME)
library(xpose)
library(tidyvpc)
setwd("./Example12")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#  Base model -----
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



##******************************************************************************************************************************************
##
##    Load the input dataset and visualize the data ------
##
##******************************************************************************************************************************************

## Set the input dataset to the built-in data "pkData"
dt_InputDataSet <- pkData


## Graph drug concentration vs. time
## This plot suggests that a two-compartment model with IV bolus is a good starting point
dt_Conc <- data.table(Time = dt_InputDataSet$Act_Time, conc = dt_InputDataSet$Conc, Subject = dt_InputDataSet$Subject)
dt_Conc$Subject <- as.factor(dt_Conc$Subject)
plot_Conc <- ggplot(dt_Conc, aes(x = Time, y = conc, group = Subject, color = Subject)) +
  scale_y_log10() +
  geom_line() +
  geom_point() +
  ylab("Drug Concentration \n at the central compartment")
print(plot_Conc)


##******************************************************************************************************************************************
##
##         Define the base model and its associated column mappings ------
##
##******************************************************************************************************************************************

## Define a basic two-Compartment population PK model with IV bolus (through built-in function "pkmodel")
## as well as its associated column mappings
## Help is available by typing ?pkmodel
ModelName <- "TwCpt_IVBolus_FOCE-ELS"
model <- pkmodel(
  numCompartments = 2,
  data = dt_InputDataSet,
  ID = "Subject",
  Time = "Act_Time",
  A1 = "Amount",
  CObs = "Conc",
  modelName = ModelName
)

## View the model and its associated column mappings
print(model)


## Disable the corresponding random effects for structural parameter V2
## Change initial values for fixed effects, tvV, tvCl, tvV2, and tvCl2, to be 15, 5, 40, and 15, respectively
## Change the covariance matrix of random effects, nV, nCl, and nCl2, to be a diagonal matrix with all its diagonal elements being 0.1
## Change the standard deviation of residual error to be 0.2
model <- model %>%
  structuralParameter(paramName = "V2", hasRandomEffect = FALSE) %>%
  fixedEffect(effect = c("tvV", "tvCl", "tvV2", "tvCl2"), value = c(15, 5, 40, 15)) %>%
  randomEffect(effect = c("nV", "nCl", "nCl2"), value = rep(0.1, 3)) %>%
  residualError(predName = "C", SD = 0.2)


## View the updated model
print(model)


##******************************************************************************************************************************************
##
##       Fit the base model -------
##
##******************************************************************************************************************************************

## Fit the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
baseFitJob <- fitmodel(model)


## View estimation results
print(baseFitJob$Overall)


##******************************************************************************************************************************************
##
##       Create diagnostic plots for the base model ------
##
## Here we demonstrate how to import estimation results to xpose database to create some commonly used diagnostic plots (through command-line).
##
## Alternatively, one can view/customize diagnostic plots as well as estimation results through model results shiny app
## (in Certara.ModelResults package), which can also be used to to generate R script and report as well as the associated R markdown.
## This shiny app can be invoked through either the model object created above
##
##           resultsUI(model)
##
## or the xpose data base created below
##
##           resultsUI(xpdb = xp)
##
## For details on this app as well as how to use it, please visit the following link
##
##           https://certara.github.io/R-model-results/index.html.
##
##******************************************************************************************************************************************

## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
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


##******************************************************************************************************************************************
##
##       Perform VPC for the base model -------
##
##******************************************************************************************************************************************

## Copy the model into a new object, and accept final parameter estimates from fitting run
## as initial estimates for VPC simulation
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0(ModelName, "_VPC"))

## View the model
print(modelVPC)


### .......................................................................................................................................
###
###      Run VPC -----
###
### .......................................................................................................................................

## Run VPC using the default host, default values for the relevant NLME engine arguments, and default values for VPC arguments
baseVPCJob <- vpcmodel(modelVPC)


### .......................................................................................................................................
###
###      Observed and simulated data -----
###
### .......................................................................................................................................

##
## Observed dataset: predcheck0
##
dt_ObsData <- baseVPCJob$predcheck0

##
## Simulated dataset: predout
##
dt_SimData <- baseVPCJob$predout


### .......................................................................................................................................
###
###
### Create VPC plots through "tidyvpc" package----
###
###
###  The "tidyvpc" package provides support for both continuous and categorical VPC using both binning and binless methods.
###  For details on this package, please visit the following link:
###
###              https://certara.github.io/tidyvpc/index.html
###
###  Next, we will demonstrate how to create binless and binning VPC plots through this package. Note that this example only contains one
###  observed variable and both simulated and observed data meet the requirements set by tidyvpc package. Hence, there is no need to do
###  any data preprocessing for this example.
###
###  Alternatively, one can create/customize VPC plots through VPC results shiny app (in Certara.VPCResults package), which can also be used to
###
###          - generate corresponding tidyvpc code to reproduce the VPC ouput from R command line
###
###          - generate report as well as the associated R markdown.
###
###  For this example, VPC results shiny app can be directly invoked through the following commands:
###
###         vpcResultsUI(observed = dt_ObsData, simulated = dt_SimData)
###
### .......................................................................................................................................

##
## Create a binless VPC plot
##
binless_vpc <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binless() %>%
  vpcstats()
plot_binless_vpc <- plot(binless_vpc, legend.position = "none") +
  ggplot2::ggtitle("Binless")

##
## Create a binning VPC plot with binning method set to be "jenks"
##
binned_vpc <- observed(dt_ObsData, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData, ysim = DV) %>%
  binning(bin = "jenks") %>%
  vpcstats()
plot_binned_vpc <- plot(binned_vpc, legend.position = "left") +
  ggplot2::ggtitle("Binning")


##
## Put these two plots side-by-side
##
egg::ggarrange(plot_binned_vpc,
               plot_binless_vpc,
               nrow = 1, ncol = 2,
               top = "VPC Comparison"
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                 Stepwise Covariate search   ------
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates
covariateModel <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0(ModelName, "_CovariateSearch"))

## View the model
print(covariateModel)


##******************************************************************************************************************************************
##
##       Add covariates to the base model ------
##
##******************************************************************************************************************************************
##
## Add categorical covariate "Sex" to structural parameter "V" and "Cl", and map "Sex" to its corresponding column, Gender, in the input dataset
## Add covariate "Age" to structural parameter "V" and "Cl" (and "Age" is automatically mapped to its corresponding column, Age, in the input dataset
## Add covariate "BW" to structural parameters "V" and "Cl", and map "BW" to its corresponding column, BodyWeight, in the input dataset
##
covariateModel <- covariateModel %>%
  addCovariate(covariate = c(Sex = "Gender"), effect = c("V", "Cl"), type = "Categorical", levels = c(0, 1), labels = c("female", "male")) %>%
  addCovariate(covariate = "Age", effect = c("V", "Cl")) %>%
  addCovariate(covariate = c(BW = "BodyWeight"), effect = c("V", "Cl"), center = "Value", centerValue = 70)

## View the updated model and its associated column mappings
print(covariateModel)

##******************************************************************************************************************************************
##
##       Run stepwise covariate search -----
##
##******************************************************************************************************************************************

## Run the stepwise covariate search using the default multicore host, default values for all the relevant NLME engine arguments,
## and default values for stepwise covariate search arguments
##
## From the end of console outputs, we see that
##
##          Scenario to use = cstep009  V-BW Cl-BW 001001
##
covariateJob <- stepwiseSearch(covariateModel)


## View model fit diagnostic for all scenarios
print(covariateJob$Scenario[covariateJob$BestScenario])


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#         VPC analysis of the final selected covariate model   -----------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Keep only those covariate effects selected by the stepwise covariate search
selectedCovariateModel <- covariateModel %>%
  removeCovariate(covariate = "Age") %>%
  removeCovariate(covariate = "Sex")

## model name for the final selected covariate model
ModelName <- "TwCpt_IVBolus_SelectedCovariateModel_FOCE-ELS"

## Copy the model into a new object
finalModel <- copyModel(selectedCovariateModel, modelName = ModelName)

## View the selected covariate model
print(finalModel)


##******************************************************************************************************************************************
##
##   Fit the selected covariate model -------
##
##******************************************************************************************************************************************

## Fit the selected covariate model using the default host and default values for the relevant NLME engine arguments
finalFitJob <- fitmodel(finalModel)

## View estimation results
print(finalFitJob$Overall)


##******************************************************************************************************************************************
##
##          Create diagnostic plots for the final model -------
##
##
## Again, here we demonstrate how to import estimation results to xpose database to create some commonly used diagnostic plots (through command-line).
##
## Alternatively, one can view/customize diagnostic plots as well as estimation results through model results shiny app
## (in Certara.ModelResults package), which can also be used to to generate R script and report as well as the associated R markdown.
## This shiny app can be invoked through either the model object created above
##
##           resultsUI(finalModel)
##
## or the xpose data base created below
##
##           resultsUI(xpdb = xp)
##
## For details on this app as well as how to use it, please visit the following link
##
##           https://certara.github.io/R-model-results/index.html.
##
##******************************************************************************************************************************************
## Remove previously created xpose data base
remove(xp)


## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
xp <- xposeNlmeModel(finalModel, finalFitJob)


##  Observations against population or individual predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


## CWRES against population predictions or independent variable
res_vs_idv(xp, res = "CWRES",type = "ps", subtitle = "-2LL: @ofv")
res_vs_pred(xp, res = "CWRES",type = "ps", subtitle = "-2LL: @ofv")


## CWRES against covariate BW
res_vs_cov(xp, covariate = "BW", res = "CWRES", type = "ps")


##  Observations, individual predictions and population predictions plotted against the independent variable for every individual
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


##******************************************************************************************************************************************
##
##    Perform VPC for the final selected covariate model ------
##
##******************************************************************************************************************************************

## Copy the final selected covariate model into a new object, and accept final parameter estimates from fitting run
## as initial estimates for VPC simulation
finalModelVPC <- copyModel(finalModel, acceptAllEffects = TRUE, modelName = paste0(ModelName, "_VPC"))

## View the model
print(finalModelVPC)


### .......................................................................................................................................
###
###       Run VPC -----
###
### .......................................................................................................................................

##
## Run VPC with PRED outputted using the default host and default values for the relevant NLME engine arguments
##
## Note: Here VPC argument, outputPRED, is defined through ellipsis (additional argument).
##       Alternatively, one can define VPC arguments through vpcParams argument (type ?vpcmodel for more information on this)
##
finalVPCJob <- vpcmodel(model = finalModelVPC, outputPRED = TRUE)


### .......................................................................................................................................
###
###     Observed and simulated data ------
###
### .......................................................................................................................................

## Remove previously defined observed and simulated data
rm(dt_ObsData, dt_SimData)


## predcheck0 contains observed data for all continuous observed variables
dt_ObsData <- finalVPCJob$predcheck0


##
## predout contains simulated data for all observed variables
##
##  Note: If PRED is requested to be outputted (i.e., outputPRED = TRUE), then it also contains PRED for continuous observed variables
##
dt_SimData <- finalVPCJob$predout


### .......................................................................................................................................
###
###      Create VPC plots through "tidyvpc" package -----
###
###
###  The "tidyvpc" package provides support for both continuous and categorical VPC using both binning and binless methods.
###  For details on this package, please visit the following link:
###
###              https://certara.github.io/tidyvpc/index.html
###
###  Next, we will demonstrate how to create VPC and pcVPC plots for the final model. Note that this example contains 1 observed variable with
###  PRED outputted. Hence, to use this package, we have to do some data preprocessing on both simulated and observed data to meet the
###  requirements set by the tidyvpc package.
###
###
###     Alternatively, one can create/customize VPC plots through VPC results shiny app (in Certara.VPCResults package), which can also be used to
###
###          - generate corresponding tidyvpc code to reproduce the VPC ouput from R command line
###
###          - generate report as well as the associated R markdown.
###
###  For this example, VPC results shiny app can be directly invoked through the following commands:
###
###         vpcResultsUI(observed = dt_ObsData, simulated = dt_SimData)
###
###  Note: The shiny app will automatically preprocess the data as what we did below for tidyvpc package.
### .......................................................................................................................................

##
## Copy/Rename the simulated data for consistency
##
dt_SimData_tidyvpc <- dt_SimData


##
## Add PRED from REPLICATE = 0 of of simulated dataset to observed data
##
dt_ObsData_tidyvpc <- dt_ObsData
dt_ObsData_tidyvpc$PRED <- dt_SimData_tidyvpc[REPLICATE == 0]$PRED


##
## Create a regular VPC plot with binning method set to be "jenks"
##
binned_VPC_finalModel <- observed(dt_ObsData_tidyvpc, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc, ysim = DV) %>%
  binning(bin = "jenks") %>%
  vpcstats()
plot_binned_VPC_finalModel <- plot(binned_VPC_finalModel)


##
## Create a pcVPC plot with binning method set to be "jenks"
##
binned_pcVPC_finalModel <- observed(dt_ObsData_tidyvpc, x = IVAR, yobs = DV) %>%
  simulated(dt_SimData_tidyvpc, ysim = DV) %>%
  binning(bin = "jenks") %>%
  predcorrect(pred = PRED) %>%
  vpcstats()
plot_binned_pcVPC_finalModel <- plot(binned_pcVPC_finalModel)


## Put these two plots in a page
egg::ggarrange(plot_binned_VPC_finalModel, plot_binned_pcVPC_finalModel, labels = c("VPC", "pcVPC"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#        Bootstrapping analysis for the final selected covariate model    ----------
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Copy the selected covariate model into a new object
finalModelBootstrap <- copyModel(finalModel, modelName = paste0(ModelName, "_Bootstrapping"))


## Run the boostrap using the default multicore host, default values for all the relevant NLME engine arguments,
## and default values for bootstrap arguments (running "BootstrapParams()" to view the default values)
finalBootstrapJob <- bootstrap(finalModelBootstrap)


## View bootstrapping estimation results for Theta, sigma, and Omega
print(finalBootstrapJob$BootTheta)
print(finalBootstrapJob$BootOmega)
print(finalBootstrapJob$BootOmegaStderr)

