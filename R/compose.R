#' WORK IN PROGRESS
#'
#' Extracts SNOMED CT concepts from appropriate places in the 
#' hierarchy to create a set of CDB files in an environment.
#' Uses WordNet and manual synonyms if available.
#'
#' @param finding SNOMEDfinding object
#' @param CDB SNOMED CT concept database, as created by createCDB.
#'   An environment containing the following data tables: FINDINGS,
#'   QUAL, CAUSES, BODY, FINDINGS, OTHERSUB, OVERLAP
#' @param TRANSITIVE transitive closure table
#' @param TRANSITIVE transitive closure table
#' @return a SNOMEDfinding object with one or more refinements of
#'   the SNOMED concept if possible
#' @examples
#' # Not run
#' # refineSNOMEDfinding
refineSNOMEDfinding <- function(finding, CDB, TRANSITIVE,
	SNOMED, composeLookup, single_output = TRUE){
	# Input: a SNOMEDfinding
	# Output: a SNOMEDfinding 
	# D is the decompose table created by concatenating rows of the 
	# output of decompose
	# Remove rows with outstanding text
	
	# Suggest to create this function in a new package
	# RdiagnosisNER which has the spacy dependency
	
	# use overlap to do linking
	
	# use decomposition of self to find ancestor and try different
	# decompositions
	# return either a single best match or multiple matches as per
	# requirements (e.g. multiple may be more useful for research)
	
	# remove attributes which are now accounted for in the main concept
	
}
