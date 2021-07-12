## Test environments
* local Ubuntu 18.04, R 3.4.4
* win-builder (devel and release)

## R CMD check results
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Anoop Shah <anoop@doctors.org.uk>’

New submission

## Changes after initial CRAN submission (12/07/2021)

Apologies - incorrect version uploaded on 9/7/2021.

Description text modified as per recommendations.
Description of SNOMED CT added with website reference.

The package does not modify the .GlobalEnv. The only
reference to .GlobalEnv is to enable the user to retrieve an
object from the global environment for convenience.

Now the only interaction with .GlobalEnv is in the function
getSNOMED in loadSNOMED.R.
