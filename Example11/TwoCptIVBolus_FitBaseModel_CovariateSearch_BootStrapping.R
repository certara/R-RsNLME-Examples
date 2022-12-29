####################################################################################################################################
##                              Description
##
## The purpose of this example is to demonstrate how to
##
##    - Define and fit the base model (described by a two-compartment model with IV bolus)
##
##        * Load the input dataset and visualize the data
##
##        * Define the base model as well as mapping model variables to their corresponding input data columns
##
##        * Using the provided initial estimates shiny app, estimatesUI, to visually determine
##          a set of reasonable initial values for fixed effects
##
##        * Fit the base model with initial estimates picked from shiny app
##
##        * Import estimation results to xpose database to create commonly used diagnostic plots
##
##    - Add covariates to the base model and then identify covariates through a stepwise covariate search.
##
##    - Perform bootstrapping analysis for the model selected by the covariate search procedure.
##
##
## Note: To run this file, set the working directory to the location of this file.
##
#####################################################################################################################################

# Load libraries and set working directory
library(Certara.RsNLME)
library(magrittr)
setwd("./Example11")

##===================================================================================================================================
##                      Load the input dataset and visualize the data
##===================================================================================================================================

## Set the input dataset
input_data <- Certara.RsNLME::pkData

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

## Facet by Gender for Exploration
conc_time_plot +
  facet_grid(~ Gender)


##======================================================================================================================================
##                           Define the model and its associated column mappings
##======================================================================================================================================

## Define a basic two-compartment population PK model with IV bolus as well as its associated column mappings
## Help is available by typing ?pkmodel
model <- pkmodel(
  numCompartments = 2,
  data = input_data,
  columnMap = FALSE,
  modelName = "TwCpt_IVBolus_FOCE-ELS"
)

## Change the standard deviation of error variable to 0.2. (Help is available by typing ?residualError)
## Note:  One can use the piping %>% operator for "residualError" function
model <- residualError(model, predName = "C", SD = 0.2)

## View the model
print(model)

## Map Columns

model <- model %>%
  colMapping(id = Subject, time = Act_Time, A1 = Amount, CObs = Conc)


##========================================================================================================================================
## Use the provided initial estimates shiny app, estimatesUI, to visually determine a set of reasonable initial values for fixed effects
##========================================================================================================================================

## Invoke the initial estimates GUI to pick reasonable initial values for fixed effects
model <- estimatesUI(model)
## Click the "Save and Close" button to update model with fixed effect values specified in Shiny GUI
## The picked values are tvV = 16, tvCl = 7, tvV2 = 41, tvCl2 = 14

## View the model
print(model)


##=======================================================================================================================================
##         Fit the base model with the initial estimates picked from the shiny app
##=======================================================================================================================================

## Run the model using the default host and default values for the relevant NLME engine arguments
## Note:  The default values for the relevant NLME engine arguments are chosen based on the model. Type ?engineParams for details.
##       For this example, FOCE-ELS is the default method for estimation and Sandwich is the default method for
##       standard error calculations.
job <- fitmodel(model)


## View estimation results
print(job[c("Overall", "theta", "omega")])

##===================================================================================================================================
##                      Diagnostic plots
##===================================================================================================================================

## Imports results of an NLME run into xpose database to create commonly used diagnostic plots
# Using xposeNlme() function, which uses run directory in file system to import results
xp <- xposeNlme(dir = model@modelInfo@workingDir, modelName = "TwCpt_IVBolus_FOCE-ELS")

# Using xposeNlmeModel() function, which uses model and job objects from R environment to import results
xp <- xposeNlmeModel(model, job)

## List all available variables in an xpdb object
list_vars(xp)

## Observations against population or individual predictions
dv_vs_pred(xp, type = "p", subtitle = "-2LL: @ofv")
dv_vs_ipred(xp, type = "p", subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")


## CWRES against population predictions or independent variable
res_vs_idv(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")
res_vs_pred(xp, res = "CWRES", type = "ps", subtitle = "-2LL: @ofv")


## Observations, individual predictions and population predictions plotted against the independent variable for every individual
ind_plots(xp, subtitle = "-2LL: @ofv, Eps shrinkage: @epsshk")

##  Distribution plots of ETA
eta_distrib(xp)

## Using Certara.ModelResults Shiny Application

library(Certara.ModelResults)

resultsUI(xpdb = xp)


###################################################################################################################################

###################                      Covariate search                                                 ########################

###################################################################################################################################

## Copy the model into a new object, and accept final parameter estimates from fitting run as initial estimates
covariateModel <- copyModel(model, acceptAllEffects = TRUE, modelName = paste0("TwCpt_IVBolus_FOCE-ELS", "_CovariateSearch"))

## View the model
print(covariateModel)


#===================================================================================================================================
#       Add covariates to the base model
#===================================================================================================================================
##
## Add categorical covariate "Sex" to structural parameter "Cl", and map "Sex" to its corresponding data column "Gender", which is
## of class character with value being either "female" or "male"
##
## Add continuous covariate "Age" to structural parameter "V" (and "Age" is automatically mapped to its corresponding data column "Age")
##
## Add continuous covariate "BW" to structural parameters "V" and "Cl", and map "BW" to its corresponding data column "BodyWeight"
##
covariateModel <- covariateModel %>%
  addCovariate(covariate = c(Sex = "Gender"), effect = "Cl", type = "Categorical", levels = c(0, 1), labels = c("female", "male")) %>%
  addCovariate(covariate = "Age", effect = "V") %>%
  addCovariate(covariate = c(BW = "BodyWeight"), effect = c("V", "Cl"), center = "Value", centerValue = 70)

## View the model
print(covariateModel)

#===================================================================================================================================
#       Run stepwise covariate search
#===================================================================================================================================

## Run stepwise covariate search using the default multicore host, default values for all the relevant NLME engine arguments,
## and default values for stepwise covariate search arguments
covariateJob <- stepwiseSearch(covariateModel)


##===================================================================================================================================
##     Load and view results from covariate search
##===================================================================================================================================

## View the reports for model fit diagnostic
print(covariateJob)

## Load and view the model selected by the stepwise covariate search
stepwiseLines <- readLines(paste0(covariateModel@modelInfo@workingDir,"/Stepwise.txt"))
cat(paste0(stepwiseLines, collapse = "\n"))
cat(stepwiseLines[grep("Scenario to use = ", stepwiseLines)])

library(flextable)
flextable(covariateJob) %>%
  add_header_lines(values = "Covariate Stepwise Search") %>%
  bold(part = "header") %>%
  add_footer_lines(stepwiseLines[grep("Scenario to use = ", stepwiseLines)]) %>%
  bold(i = 7, part = "body") %>%
  bold(part = "footer")

###################################################################################################################################

###################                      Bootstrapping                                                    ########################

###################################################################################################################################

## Keep only those covariate effects selected by the covariate search
selectedCovariateModel <- covariateModel %>%
  removeCovariate(covariate = "Age") %>%
  removeCovariate(covariate = "Sex")

## View the model
print(selectedCovariateModel)


##===================================================================================================================================
##    Run the bootstrap
##===================================================================================================================================

## Copy the model into a new object
bootModel <- copyModel(selectedCovariateModel, modelName = paste0("TwCpt_IVBolus_FOCE-ELS", "_Bootstrapping"))


## Run the bootstrap using the default multicore host, default values for all the relevant NLME engine arguments,
## and default values for bootstrap arguments (running "BootstrapParams()" to view the default values)
bootstrapJob <-  bootstrap(bootModel)


##===================================================================================================================================
##    View bootstrap estimation results
##===================================================================================================================================
print(bootstrapJob)

