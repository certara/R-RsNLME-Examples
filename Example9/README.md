
# Example 9: PKPD (One Compartment IV Bolus - Indirect Limited Inhibition)

This example involves both PK and PD, where PK is described by a one-compartment model with IV bolus, and
PD is described by an indirect model with the loss of a mediator inhibited.

* Create the model through build-in model library and define the associated column mappings
* Fit the model
* Import estimation results to xpose database to create commonly used diagnostic plots
* Perform VPC for the final model
* Create VPC plots through open source package `tidyvpc`
