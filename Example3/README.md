# Example 3: One Compartment Gamma Distributed Absorption Delay VPC

The purpose of this example is to demonstrate how to:

-   create a gamma absorption delay model through built-in model library

  * update structural parameter forms
  
  * change initial values of fixed effects
  
  * add secondary parameters

-   fit the model

-   create commonly used diagnostic plots through both command-line and mode results shiny app (in Certara.ModelResults package)

-   perform VPC and create VPC plots through "tidyvpc" package (command-line) or VPC results shiny app (in Certara.VPCResults package)

The model demonstrated is a one-compartment model with 1st-order clearance and the delay time between the administration time of the drug and the time when the drug molecules reach the central compartment is assumed to be gamma distributed. In other words, the model is described as follows:

     dA1(t)/dt = sum_{i=1}^{m}D_{i}gammaPDF(t - t_{Di}; rateParameter, shapeParameter) - Cl/V * A1(t)

Here

-   A1 denotes the drug amount at central compartment with V and Cl respectively being the central volume distribution and central clearance rate

-   m denotes the number of bolus dosing events

-   D\_{i} is the amount of dose administered at time t\_{Di} for the ith dosing events

-   gammaPDF denotes the probability density function of a gamma distribution with with mean = MeanDelayTime and shape parameter being (ShapeParamMinusOne + 1).

For more information on distribute delays, please see <https://onlinehelp.certara.com/phoenix/8.3/index.html#t=topics%2FDiscrete_and_distributed_delays.htm%23XREF_92535_Discrete_and>
