#####################################################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to
##
##    - create a model involving BQL and covariates
##
##    - edit the model
##
##    - fit the edited model
##
##    - import estimation results to xpose database to create commonly used diagnostic plots
##
##    - create the VPC plot through some open source package
##
## The model demonstrated is a one-compartment model with IV bolus, where both V and Cl depend on some
## continuous covariates as shown below
##
##    - V = tvV * (BW/30)^dVdBW * exp(nV)
##    - Cl = tvCl * (BW/30)^dCldBW * (PMA^Gam/(PMA^Gam + PMA50^Gam)) * exp(nCl)
##
## where BW and PMA are covariates.
##
##
## Note: To run this file, please set the working directory to the location where this file is located.
##
#####################################################################################################################################

## Set up environment variables, load necessary packages, and set up commonly used host
library(Certara.RsNLME)
library(magrittr)
setwd("./Example4")


#####################################################################################################################################

############  Load the input dataset, define the model and associated column mapping, and then edit the model       ###############

#####################################################################################################################################


##===================================================================================================================================
##                            Load the input dataset
##===================================================================================================================================

input_data <- read.csv("OneCpt_IVBolus_ContCovariatesOnClV_BQL.csv")


##===================================================================================================================================
##                           Define the model and column mappings
##==================================================================================================================================
## Define a basic one-compartment model with IV bolus as well as the associated column mappings
model <-
  pkmodel(
    data = input_data,
    ID = "ID",
    Time = "Time",
    A1 = "Dose",
    CObs = "CObs",
    modelName = "OneCpt_IVBolus_ContCovariatesOnClV_BQL_Laplacian"
  )

##-----------------------------------------------------------------------------------------------------------------------------------------
## Add covariate "BW" to structural parameters V and Cl ("BW" is automatically mapped to its corresponding column "BW" in the input dataset)
##
## Add covariate "PMA" to the model ("PMA" is automatically mapped to its corresponding column "PMA" in the input dataset)
##
## Set initial values of fixed effects "tvV", "tvCl", "dVdBW", "dCldBW" to be 20, 20, 1, 1, respectively
##
## Set the covariance matrix of random effects nV and nCl to be a diagonal matrix with diagonal elements being 0.1 and 0.2, respectively
##
## Specify observations contains BQL data and map the BQL flag variable CObsBQL to its corresponding column in the input dataset
##-----------------------------------------------------------------------------------------------------------------------------------------
model <- model %>%
  addCovariate(covariate = "BW", effect = c("V", "Cl"), center = "Value", centerValue = 30) %>%
  addCovariate(covariate = "PMA") %>%
  fixedEffect(effect = c("tvV", "tvCl", "dVdBW", "dCldBW"), value = c(20, 20, 1, 1)) %>%
  randomEffect(effects = c("nV", "nCl"), values = c(0.1, 0.2)) %>%
  residualError(predName = "C", isBQL = TRUE, CObsBQL = "CObsBQL")


## View the model
print(model)


##===================================================================================================================================
##            Edit the model to include the effect of "PMA" on the structural parameter "Cl"
##===================================================================================================================================

## Run the code below to open the text editor to edit the PML codes
model <- editModel(model)


## To incorporate the effect of "PMA" on the structural parameter "Cl",
##
##    - add "* (PMA^Gam/(PMA^Gam + PMA50^Gam))" to the right-hand side of stparm statement for "Cl"
##
## In other words, change the following statements
##
##    stparm(Cl = tvCl * ((BW/30)^dCldBW)   * exp(nCl))
##
## to
##
##    stparm(Cl = tvCl * (BW/30)^dCldBW * (PMA^Gam/(PMA^Gam + PMA50^Gam)) * exp(nCl))
##
## Then add the following statements to the codes (right before "ranef" statement) to define the newly introduced fixed effects
##
##    fixef(PMA50 = c(, 5, ))
##    fixef(Gam = c(, 1, ))
##
## Then click the "Save" button to update the model.

## View the updated model
print(model)


###########################################################################################################################################

###################                    Model Fitting                                                                  ####################

###########################################################################################################################################

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note: the default values for the relevant NLME engine arguments are chosen based on the model, type ?engineParams for details.
##       For example, for this example, Laplacian is the default method for estimation, and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)


## View estimation results
print(job[c("Overall", "theta", "omega")])


#########################################################################################################################################

###################                      Diagnostic plots                                                     ##########################

########################################################################################################################################

library(Certara.Xpose.NLME)
library(xpose)
## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
xp <- xposeNlmeModel(model, job)

##===================================================================================================================================
##            Observations against population or individual predictions
##===================================================================================================================================
# observations against population predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")

# observations against individual predictions
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


##===================================================================================================================================
##           Residuals against population predictions or independent variable
##===================================================================================================================================
# CWRES against population predictions
res_vs_pred(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")

# CWRES against the independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")


##===================================================================================================================================
## Observations, individual predictions and population predictions plotted against the independent variable for every individual
##===================================================================================================================================
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk", page = 1)


##===================================================================================================================================
##          Distribution plots of ETA
##===================================================================================================================================
eta_distrib(xp)


#####################################################################################################################################

###################                    VPC                                                                       ###################

#####################################################################################################################################


## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates
modelVPC <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0("OneCpt_IVBolus_ContCovariatesOnClV_BQL", "_VPC"))

## View the model
print(modelVPC)


##===================================================================================================================================
##                                   Run VPC for the model
##===================================================================================================================================

## Run VPC using the default host, default values for the relevant NLME engine arguments, and default values for VPC arguments
vpcJob <- vpcmodel(modelVPC)

##===================================================================================================================================
##                             Using vpc library to do the VPC plots
##===================================================================================================================================

LLOQ <- 0.01

## Simulation input dataset, predcheck0.csv, which resets the value of observed variable having LLOQ value to BLOQ
dt_ObsData <- vpcJob$predcheck0

# Note: if the data.table package is installed, the above data.frame is additionally of class data.table, which allows
# us to use data pre-processing syntax from the data.table package e.g., setnames() to rename columns.
## Clean the simulation input dataset to make it ready to create VPC plots through "vpc" package
library(data.table)
setnames(dt_ObsData, c("IVAR", "ID5"), c("TIME", "ID"))
dt_ObsData$DV[dt_ObsData$DV == "BLOQ"] <- LLOQ
dt_ObsData$DV <- as.numeric(dt_ObsData$DV)


## Simulation output dataset, predout.csv
dt_SimData <- vpcJob$predout

## Clean the simulation output dataset to make it ready to create VPC plots through "vpc" package
setnames(dt_SimData, c("ID5", "IVAR"), c("ID", "TIME"))


#----------------------------------------------------------------------------------------------------------
# Create VPC plots through "vpc" package
#   - Create a VPC plot for un-censored data that shows the censor limit (LLOQ) as a horizontal line
#   - Create a VPC for the probability of left-censored data
# Note: need to set the value of lloq to be above the LLOQ so that observation equal to LLOQ
# will not be treated as actual observations, see http://vpc.ronkeizer.com/censored-data.html for details
#----------------------------------------------------------------------------------------------------------
lloq_value <- LLOQ + 1e-8

# Create a VPC plot for un-censored data that shows the censor limit (LLOQ) as a horizontal line
# Using the vpc package
library(vpc)
plot_VPC <- vpc(
  sim = dt_SimData,
  obs = dt_ObsData,
  lloq = lloq_value,
  log_y = TRUE,
  log_y_min = 1e-9
)

# Create a VPC for the probability of left-censored data
plot_VPC_cens <- vpc_cens(sim = dt_SimData,  obs = dt_ObsData, lloq = lloq_value)

# put these two VPC plots in a page
egg::ggarrange(plot_VPC, plot_VPC_cens)
