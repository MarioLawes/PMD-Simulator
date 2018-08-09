# Shiny App for 2-MM and 3-MM Designs with Planned Missing Data
This shiny app can be used to find the optimal sample size constallations for Two-Method Measurement (2-MM) and Three-Method Measurement (3-MM) Designs with planned missing data (see Lawes, Schultze, and Eid, in press; Graham, Taylor, Olchowski, & Cumsille, 2006).

A running version of this shiny app is hosted on https://mariolawes.shinyapps.io/r-shiny-app/

# Mode 1: Compute Sample Size Constallation
Possible sample size constallations and corresponding missing data proprtions for the given costs of the methods, the available budget and the specified increment in which the numer of cheap method is increased in each step is computed. The resulting table can be downloaded as .csv or .Rdata.

# Mode 2: Finding the Most Efficient Design
The simulations of Lawes et al. (in press) and Graham et al. (2006) in respect to 2-MM and 3-MM Designs can be easily replicated with a different set of cost-ratios of the cheap and expensive method as well as different model parameters (i.e., factor loadings, factor correlations). Also 2-MM and 3-MM Designs can be compared in terms of their power for testing the latent regression coeficient between the construct and the outcome. 
The number of replications should be increased (at least 500 reps) for the final model choice to retain trustworthy results. For testing different models in a exploratory manner, a smaller number of replications can be sufficient.
