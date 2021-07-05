## Test environments
* local Ubuntu 18.04, R 3.4.4
* win-builder (devel and release)

## R CMD check results
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Anoop Shah <anoop@doctors.org.uk>’

New submission

## Changes after initial CRAN submission

Description text modified as per guidance.
Description of SNOMED CT added with website reference.
No objects are in .GlobalEnv are created or modified. Now
all references to .GlobalEnv are in a single function and
the only interaction with .GlobalEnv is to retrieve an
object to assist with interactive use of the functions.
