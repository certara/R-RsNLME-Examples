# Example 12: Two Compartment IV Bolus - Stepwise Covariate Search, Bootstrapping, & VPC

The purpose of this example is to demonstrate how to

   - Define the base model (described by a two-compartment model with IV bolus) through built-in model library,
     fit this model to the data, create commonly used diagnostic plots, and perform VPC and then create binnless
     and binned VPC plots through open source package "tidyvpc".

       * Load the input dataset and visualize the data

       * Define the base model as well as mapping model variables to their corresponding input data columns

       * Fit the base model

       * Import estimation results to xpose database to create commonly used diagnostic plots

       * Perform VPC for the base model and create binnless and binned VPC plots through open source package "tidyvpc"

   - Add covariates to the base model and then identify covariates through a stepwise covariate search.

   - Perform VPC for the model selected by the covariate search procedure, and create pcVPC plot through open source
     package "tidyvpc".

   - Perform Bootstrapping analysis for the model selected by the covariate search procedure.
