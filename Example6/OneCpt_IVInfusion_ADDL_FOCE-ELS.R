#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                              Description
##
## The purpose of this example is to demonstrate how to create a model involving ADDL through RsNlme.
## The model demonstrated is a one-compartment model with IV infusion.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Load necessary packages and set working directory
library(Certara.RsNLME)
library(magrittr)
library(data.table)

if (Sys.getenv("RSTUDIO") == 1) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
} else {
  setwd("./Example6")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#      Model  -------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##
##  Load the input dataset
##
InputDataSetName <- "OneCpt_IVInfusion_ADDL"
dt_InputDataSet <- fread(paste0(InputDataSetName, ".csv"))

##******************************************************************************************************************************************
##
##                            Create the model and the associated column mappings ----
##
##******************************************************************************************************************************************

## Model name
ModelName <- paste0(InputDataSetName, "_FOCE-ELS")

##
## Define a basic one-compartment PK model with IV infusion as well as the associated column mapping
##
## Change the initial values of fixed effect "tvV" to 5.
##
model <- pkmodel(
  infusionAllowed = TRUE,
  data = dt_InputDataSet,
  ID = "Subject",
  Time = "time",
  A1 = "Dose",
  A1_Rate = "Rate",
  CObs = "DV",
  modelName = ModelName
) %>%
  fixedEffect(effect = "tvV", value = 5)


##******************************************************************************************************************************************
##
##  Add ADDL dose mapping information to the column mapping file -----
##
## Note: the model object must have basic column mappings defined
##
##******************************************************************************************************************************************
model <- model %>%
  addADDL(ADDL = "ADDL", II = "II")


## Note: for this problem, an alternative way to implement ADDL is through "addDoseCycle" with code given below
##    model <- addDoseCycle(model, type = "ADDL", name = "A1", administration = "Infusion", amount = "Dose", II = "II", rate = "Rate", colName = "ADDL")


##
## View the updated column mappings
##
print(model)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                        Model Fitting  -----------
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, FOCE-ELS is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)


## View estimation results
print(job$Overall)
