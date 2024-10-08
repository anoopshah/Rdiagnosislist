% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hierarchy.R
\name{semanticType}
\alias{semanticType}
\title{Retrieves semantic types using the text 'tag' in the description}
\usage{
semanticType(conceptIds, SNOMED = getSNOMED())
}
\arguments{
\item{conceptIds}{character or integer64 vector of SNOMED concept IDs}

\item{SNOMED}{environment containing a SNOMED dictionary}
}
\value{
a character vector of semantic tags corresponding to the conceptIDs
}
\description{
Uses the fully specified name in the DESCRIPTION table. If there are
multiple fully specified names, the name with the most recent
effectiveTime will be used.
}
\examples{
SNOMED <- sampleSNOMED()

semanticType(as.SNOMEDconcept(c('Heart failure', 'Is a')))
}
