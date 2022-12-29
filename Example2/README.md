
# Example 2: One Compartment First-Order Absorption Subpopulation Analysis

The purpose of this example is to demonstrate how to do a subpopulation type of analysis. Specifically,
the input dataset, `OneCpt_1stOrderAbsorp_SubpopulationAnalysis.csv`, contains a column "Source". For each
value of "Source", we want to fit the same model but have the results separated by the Source value.  This is performed using the `sortfit()` function in the `Certara.RsNLME` package.

We also demonstrate how to extract estimations results from the returned `job` object and plot `DV` vs `PRED` using the `ggplot2` package. The resulting plot is faceted by the column we used for our `sortfit()` e.g., the `Source` column.

