# Example 11: One Compartment First Order Absorption Emax Categorical Model with VPC

The purpose of this file is to demonstrate how to

  - Fit a model defined by PML codes, where

       * the PK portion of the model is described by a one-compartment model with first-order absorption,

       * the PD portion of the model is described by an Emax model and a categorical model with three categories

    See `OneCpt1stOrderAbsorp_Emax_CategoricalModel.mdl` in the same directory where this file is located for details.

  - Import estimation results to xpose database to create some commonly used diagnostic plots for each continuous observed variable

  - Perform VPC for the model

  - Create VPC plots through open source package "tidyvpc" (command-line usage) and VPC results shiny app (in Certara.VPCResults package)

