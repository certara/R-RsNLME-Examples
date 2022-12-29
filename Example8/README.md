
# Example 8: One Compartment First-Order Absorption Event Hazard

The purpose of this example is to demonstrate how to

* edit a build-it model
* fit the edited model

The example demonstrated is a model involving both continuous and event observations

* PK: a one-compartment model with first-order absorption having no time lag,
* Event model: hazard function is described by an Imax model given by `1 - C/(C + IC50)`

Here `C` denotes the drug concentration at the central compartment, and `IC50` is the drug concentration necessary to produce half-maximal inhibition.


Note: To run this file, please set the working directory to the location where this file is located.
