## Test environments
* local Ubuntu 20.04, R 3.6.3, R 4.1.0
* win-builder (devel and release)
* rhub clang20, gcc13, gcc14, ubuntu-clang, ubuntu-gcc12

## R CMD check results
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Anoop D. Shah <anoop@doctors.org.uk>’

Days since last update: 6

There is also a note about an invalid DOI in the
DESCRIPTION file, but it is actually correct.

## Changes in this version 1.4.1 (4 Feb 2025)
Relaxation of checking of identical data.tables when saving
and loading SNOMED dictionaries to avoid error in tests. No
changes to functionality of the actual package.
