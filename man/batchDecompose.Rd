% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addComposeLookupToCDB.R,
%   R/createComposeLookup.R
\name{batchDecompose}
\alias{batchDecompose}
\title{Creates a set of lookups for SNOMED composition}
\usage{
batchDecompose(conceptIds, CDB, output_filename, SNOMED = getSNOMED(), ...)

batchDecompose(conceptIds, CDB, output_filename, SNOMED = getSNOMED(), ...)
}
\arguments{
\item{conceptIds}{SNOMED CT concept IDs for creating decompositions}

\item{CDB}{concept database environment, containing a table called
FINDINGS}

\item{output_filename}{filename of output file}

\item{SNOMED}{environment containing a SNOMED dictionary}

\item{...}{out}
}
\value{
TRUE if successful

TRUE if successful
}
\description{
Creates composition lookup table for a set of SNOMED CT concepts
and exports the results to a file

Creates composition lookup table for a set of SNOMED CT concepts
and exports the results to a file
}
\seealso{
decompose, compose, addComposeLookupToCDB

decompose, compose, createComposeLookup
}
