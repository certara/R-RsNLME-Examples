# Example 11: Stepwise Covariate Search & Bootstrapping (Two Compartment IV Bolus)

The purpose of this example is to demonstrate how to:

-   Define and fit the base model (described by a two-compartment model with IV bolus)

    -   Load the input dataset and visualize the data

    -   Define the base model as well as mapping model variables to their corresponding input data columns

    -   Using the provided initial estimates shiny app, estimatesUI, to visually determine a set of reasonable initial values for fixed effects

    -   Fit the base model with initial estimates picked from shiny app

    -   Import estimation results to xpose database to create commonly used diagnostic plots

-   Add covariates to the base model and then identify covariates through a stepwise covariate search.

-   Perform bootstrapping analysis for the model selected by the covariate search procedure.
