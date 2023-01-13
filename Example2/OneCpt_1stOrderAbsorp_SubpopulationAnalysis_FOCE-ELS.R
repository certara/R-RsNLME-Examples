#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                               Description
##
##
## The purpose of this example is to demonstrate how to do a subpopulation type of analysis. Specifically, the input dataset,
## OneCpt_1stOrderAbsorp_SubpopulationAnalysis.csv, contains a column "Source". For the data corresponding to each value of "Source",
## we want to fit it to the same model (with the estimation results also separated by the Source value).
##
## We also demonstrate how to import estimation results to xpose database to create some commonly used diagnostic plots
## for each analysis.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load necessary packages and setup working directory
library(Certara.RsNLME)
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
setwd("./Example2")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#       Model  ---------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##
##  Load the input dataset
##
InputDataSetName <- "OneCpt_1stOrderAbsorp_SubpopulationAnalysis"
dt_InputDataSet <- fread(paste0(InputDataSetName, ".csv"))


##******************************************************************************************************************************************
##
##                            Define the model and the associated column mapping ----
##
##******************************************************************************************************************************************

# model name
ModelName <- paste0(InputDataSetName, "_FOCE-ELS")

#
# Define a one-compartment model with 1st-order absorption as well as the associated column mapping
# Set the initial value of fixed effect, tvV, to 5
# Set the covariance matrix of random effects to be a diagonal matrix with all its elements being 0.01
#
model <- pkmodel(
  absorption = "FirstOrder",
  data = dt_InputDataSet,
  ID = "ID",
  Time = "Time",
  Aa = "Dose",
  CObs = "CObs",
  modelName = ModelName
) %>%
  fixedEffect(effect = "tvV", value = 5) %>%
  randomEffect(effect = c("nKa", "nV", "nCl"),
               value = rep(0.01, 3))

#
# View the model and its associated column mappings
#
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                         Model Fitting   -----------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# List of sort variables
sortColumnSetUp <- SortColumns("Source")

## Run the model using the multicore host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
localMulticoreHost <- hostParams(hostName = "Local_Multicore", parallelMethod = "Multicore", numCores = 4)

job <- sortfit(model, hostPlatform = localMulticoreHost, sortColumns = SortColumns("Source"))

## View estimation results
print(job$Overall)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                       Diagnostic plots  ----------
#
#
# Here we demonstrate how to use ggplot2 to plot convergence data and DV vs PRED

## Convergence Plots ----

convergenceData <- job$ConvergenceData

ggplot(convergenceData, aes(Iter, Value, group = 1)) +
  geom_line() +
  facet_wrap(~Parameter + Source, ncol = 2, scales = "free") +
  xlab("Iteration") +
  ylab("")

dvpredData <- job$residuals %>%
  select(Source, DV, PRED)

ggplot(dvpredData, aes(PRED, DV)) +
  facet_wrap(~Source, scales = "fixed") +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  xlab("Population Prediction") +
  ylab("Observed Value")
