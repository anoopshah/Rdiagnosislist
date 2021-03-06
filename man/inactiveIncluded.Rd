% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concepts.R
\name{inactiveIncluded}
\alias{inactiveIncluded}
\title{Check if inactive terms are included in SNOMED CT dictionary}
\usage{
inactiveIncluded(SNOMED = getSNOMED())
}
\arguments{
\item{SNOMED}{environment containing SNOMED dictionary, defaults
to an object named 'SNOMED' in the global environment}
}
\value{
TRUE or FALSE (logical vector of length one)
}
\description{
Checks the active_only flag in the metadata of a SNOMED
environment to determine whether inactive terms are
included
}
\examples{
# Create a TEST environment and load the sample dictionaries
TEST <- sampleSNOMED()
inactiveIncluded(TEST)
assign('metadata', list(active_only = TRUE), envir = TEST)
inactiveIncluded(TEST)
}
