
# Example 4: One Compartment IV Bolus

The purpose of this example is to demonstrate how to:

* create a model involving BQL and covariates
* edit the model
* fit the edited model
* import estimation results to xpose database to create commonly used diagnostic plots
* create the VPC plot through some open source package

The model demonstrated is a one-compartment model with IV bolus, where both V and Cl depend on some
continuous covariates as shown below:

* `V = tvV * (BW/30)^dVdBW * exp(nV)`
* `Cl = tvCl * (BW/30)^dCldBW * (PMA^Gam/(PMA^Gam + PMA50^Gam)) * exp(nCl)`

Where BW and PMA are continuous covariates.
