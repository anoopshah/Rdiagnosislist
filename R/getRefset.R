#' Retrieves a Refset from the REFSET table
#'
#' @param conceptIds character or integer64 vector of Refset SNOMED
#'   concept IDs, or something that can be coerced to a SNOMEDconcept
#' @param SNOMED environment containing a SNOMED dictionary
#' @return a SNOMEDconcept vector of conceptIds of members of the
#'   selected refset(s)
#'   
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' getRefset(c('Renal clinical finding simple reference set',
#'   'Care planning activities simple reference set'))
getRefset <- function(conceptIds, SNOMED = getSNOMED()){
	refsetId <- NULL
	conceptIds <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
	if (length(conceptIds) == 0){
		message('No refset specified')
		return(as.SNOMEDconcept(bit64::integer64(0), SNOMED = SNOMED))
	} else if (any(conceptIds %in% SNOMED$REFSET$refsetId)){
		return(as.SNOMEDconcept(SNOMED$REFSET[refsetId %in%
			conceptIds]$referencedComponentId, SNOMED = SNOMED))
	} else {
		message('No refset found')
		return(as.SNOMEDconcept(bit64::integer64(0), SNOMED = SNOMED))
	}
}
