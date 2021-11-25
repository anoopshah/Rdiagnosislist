## Test environments
* local Ubuntu 20.04, R 3.6.3
* win-builder (devel and release)

## R CMD check results
Note_to_CRAN_maintainers
Maintainer: ‘Anoop Shah <anoop@doctors.org.uk>’

Status: OK

## Changes in this version 1.0 (25 Nov 2021)

Multiple new features and changes to the SNOMEDcodelist
function, some features are not backward compatible.

## Changes after initial CRAN submission (12 Jul 2021)

Apologies - incorrect version uploaded on 9/7/2021.

Description text modified as per recommendations.
Description of SNOMED CT added with website reference.

The package does not modify the .GlobalEnv. The only
reference to .GlobalEnv is to enable the user to retrieve an
object from the global environment for convenience.

Now the only interaction with .GlobalEnv is in the function
getSNOMED in loadSNOMED.R.
