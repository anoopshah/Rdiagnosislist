#' WORK IN PROGRESS
#'
#' Extracts SNOMED CT concepts from appropriate places in the 
#' hierarchy to create a set of CDB files in an environment.
#' Uses WordNet and manual synonyms if available.
#'
#' @param SNOMED environment containing a SNOMED dictionary
#' @param WN WordNet data.table as returned by downloadWordnet
#'   containing WordNet data from appropriate
#'   categories, in the format: cat (character), wordnetId (integer64),
#'   synonyms (list), parents (list), adj (list)
#' @param MANUAL_SYNONYMS 
#' @return environment containing the following data tables: FINDINGS,
#'   QUAL, CAUSES, BODY, FINDINGS, OTHERSUB, OVERLAP
#' @seealso [addWordnet()]
#' @references \url{https://wordnet.princeton.edu/}
#' @examples
#' # Not run
#' # WORDNET <- downloadWordnet()
compose <- function(SNOMEDdetail, CDB, TRANSITIVE, composeLookup){
	# D is the decompose table created by concatenating rows of the 
	# output of decompose
	# Remove rows with outstanding text
	
}

#' WORK IN PROGRESS
#'
#' Extracts SNOMED CT concepts from appropriate places in the 
#' hierarchy to create a set of CDB files in an environment.
#' Uses WordNet and manual synonyms if available.
#'
#' @param SNOMED environment containing a SNOMED dictionary
#' @param WN WordNet data.table as returned by downloadWordnet
#'   containing WordNet data from appropriate
#'   categories, in the format: cat (character), wordnetId (integer64),
#'   synonyms (list), parents (list), adj (list)
#' @param MANUAL_SYNONYMS 
#' @return environment containing the following data tables: FINDINGS,
#'   QUAL, CAUSES, BODY, FINDINGS, OTHERSUB, OVERLAP
#' @seealso [addWordnet()]
#' @references \url{https://wordnet.princeton.edu/}
#' @examples
#' # Not run
#' # WORDNET <- downloadWordnet()
ner <- function(text, CDB, TRANSITIVE, composeLookup){
	
	
}
