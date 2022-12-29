###################################################################################################################################
##                              Description
##
##
## The purpose of this example is to demonstrate how to do a subpopulation type of analysis. Specifically,
## the input dataset, OneCpt_1stOrderAbsorp_SubpopulationAnalysis.csv, contains a column "Source". For each
## value of "Source", we want to fit the same model but have the results separated by the Source value.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
##
###################################################################################################################################

## Set up environment variables, load necessary packages
library(Certara.RsNLME)
library(magrittr)
setwd("./Example2")


###################################################################################################################################

#########      Load the input dataset, and define the model and its associated column mapping                      ###############

###################################################################################################################################


##===================================================================================================================================
##                             Load the input dataset
##===================================================================================================================================

input_data <-  read.csv("OneCpt_1stOrderAbsorp_SubpopulationAnalysis.csv")

##===================================================================================================================================
##                           Define the model and the associated column mapping
##===================================================================================================================================

# model name
model_name <- "OneCpt_1stOrderAbsorp_SubpopulationAnalysis_FOCE-ELS"

# ---------------------------------------------------------------------------------------------------------------------------------
# Define a one-compartment model with 1st-order absorption as well as the associated column mapping
# Set the initial value of fixed effect, tvV, to 5
# Set the covariance matrix of random effects to be a diagonal matrix with all its elements being 0.01
# --------------------------------------------------------------------------------------------------------------------------------
model <- pkmodel(
  absorption = "Extravascular",
  data = input_data,
  ID = "ID",
  Time = "Time",
  Aa = "Dose",
  CObs = "CObs",
  modelName = ModelName
) %>%
  fixedEffect(effect = "tvV", value = 5) %>%
  randomEffect(effect = c("nKa", "nV", "nCl"),
               value = rep(0.01, 3))

# View the model
print(model)


###################################################################################################################################

###################                        Model Fitting                                                     ###################

###################################################################################################################################

## Run the model using the multicore host and default values for the relevant NLME engine arguments e.g., params
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.

local_multicore_host <- hostParams(hostName = "Local_Multicore", parallelMethod = "Multicore", numCores = 4)

job <- sortfit(model, hostPlatform = local_multicore_host, sortColumns = SortColumns("Source"))

## View estimation results
print(job[c("Overall", "theta", "omega")])

## Plot Convergence Data
library(ggplot2)

convergence_data <- job$ConvergenceData

ggplot(convergence_data, aes(Iter, Value, group = 1)) +
  geom_line() +
  facet_wrap(~Parameter + Source, ncol = 2, scales = "free") +
  xlab("Iteration") +
  ylab("")




###################################################################################################################################

###################                      Diagnostic plots                                                 ########################

###################################################################################################################################

library(dplyr)
library(ggplot2)

## Extract columns of interest from job$residuals and create data for plotting
dv_pred_data <- job$residuals %>%
  select(Source, DV, PRED)

ggplot(dv_pred_data, aes(PRED, DV)) +
  facet_wrap(~Source, scales = "fixed") +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  xlab("Population Prediction") +
  ylab("Observed Value")
