###################################################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to create a model involving steady state through RsNlme.
## The model demonstrated is a one-compartment model with first-order absorption having no time lag.
##
####
###################################################################################################################################
## Set up environment variables, load necessary packages, and set up commonly used host
library(Certara.RsNLME)
library(magrittr)
setwd("./Example1")



#####################################################################################################################################

###############      Load the input dataset, and define the model and the associated column mappings                  ###############

#####################################################################################################################################

##===================================================================================================================================
##                            Load the input dataset
##===================================================================================================================================
input_data <- read.csv("OneCpt_1stOrderAbsorp_SteadyState.csv")

##===================================================================================================================================
##                  Create the model and map model variables to their corresponding input data columns
##===================================================================================================================================

## Model name
model_name <- "OneCpt_1stOrderAbsorp_SteadyState_FOCE-ELS"

##----------------------------------------------------------------------------------------------------------------------------------
## Define a basic one-compartment PK model with 1st-order absorption as well as the associated column mapping
##
## Change the initial values of fixed effects, tvV and tvCl, to 10 and 2, respectively.
##----------------------------------------------------------------------------------------------------------------------------------
model <- pkmodel(
  numCompartments = 1,
  absorption = "Extravascular",
  data = input_data,
  ID = "id",
  Time = "time",
  Aa = "Dose",
  CObs = "CObs",
  modelName = model_name
) %>%
  fixedEffect(effect = c("tvV", "tvCl"), value = c(10, 2))


##----------------------------------------------------------------------------------------------------------------------------------
## Add steady state dose mapping information to the column mapping file
##
## Note: the model object must have basic column mappings defined
##----------------------------------------------------------------------------------------------------------------------------------
model <- addSteadyState(model, SS = "SS", II = "II")


## Note: for this problem, an alternative way to implement the steady state is through "addDoseCycle" with code given below
##    model <- addDoseCycle(model, type = "SteadyState", name = "Aa", amount = "Dose", II = "II", colName = "SS")


#######################################################################################################################################

###################                        Model Fitting                                              ################################

#######################################################################################################################################

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)


## View estimation results
job[c("Overall", "theta", "omega")]

## Plot Convergence Data
library(ggplot2)

convergence_data <- job$ConvergenceData

ggplot(convergence_data, aes(Iter, Value, group = 1)) +
  geom_line() +
  facet_wrap(~Parameter, ncol = 2, scales = "free") +
  xlab("Iteration") +
  ylab("")






