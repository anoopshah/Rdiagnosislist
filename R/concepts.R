#' Check if inactive terms are included in SNOMED dictionary
#'
#' Checks the active_only flag in the metadata of a SNOMED
#' environment to determine whether inactive terms are
#' included
#'
#' @param SNOMED environment containing SNOMED dictionary, defaults
#'   to an object named 'SNOMED' in the global environment
#' @return TRUE or FALSE (logical vector of length one)
inactiveIncluded <- function(SNOMED = get('SNOMED', envir = globalenv())){
	if (is.null(SNOMED$metadata$active_only)){
		TRUE
	} else if (SNOMED$metadata$active_only == TRUE){
		FALSE
	} else {
		TRUE
	}
}

#' Obtain the SNOMED CT concept IDs for a set of terms
#'
#' Carries out an exact or regular expression match to
#' return the concept ID for a set of search terms
#'
#' @param terms character vector of terms or words to match
#' @param active_only whether or not to include inactive concepts
#' @param exact_match if TRUE, only an exact (case sensitive)
#'   match is performed. If FALSE, a regular expression match
#'   is performed.
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @return a vector of unique SNOMED CT concept IDs in integer64 format
#' @export
conceptId <- function(terms, active_only = TRUE,
	exact_match = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	MATCHED <- do.call('rbind', lapply(terms, function(term){
		if (exact_match){
			tomatch <- data.table(term = term)
			SNOMED$DESCRIPTION[tomatch, on = 'term'][,
				.(active, conceptId)]
		} else {
			tomatch <- term
			SNOMED$DESCRIPTION[term %like% tomatch][,
				.(active, conceptId)]
		}
	}))
	
	if (active_only & inactiveIncluded(SNOMED)){
		MATCHED <- MATCHED[active == TRUE]
	}
	MATCHED <- MATCHED[, .(id = conceptId)]
	if (active_only){
		# Note that this filtering is essential - even if inactive
		# concepts are excluded, they may be included in the
		# descriptions.
		unique(SNOMED$CONCEPT[MATCHED, on = 'id'][active == TRUE]$id)
	} else {
		unique(MATCHED$id)
	}
}

#' Check that concept IDs are in the correct format
#' 
#' Checks if a vector of conceptIds of type integer64, and 
#' converts them to integer64 if necessary.
#'
#' @param conceptIds character or integer64 vector 
#' @return conceptIds in integer64 format
checkConcepts <- function(conceptIds){
	conceptIds <- unlist(conceptIds) # ensure that it is a vector
	
	if (class(conceptIds) == 'character'){
		return(as.integer64(conceptIds))
	} else if (class(conceptIds) == 'integer64'){
		# correct format
		return(conceptIds)
	} else {
		stop('conceptId must be supplied in character or integer64 format; ',
			class(conceptIds), 'is not acceptable.')
	}
}

#' Obtain descriptions for a set of SNOMED CT terms
#'
#' Returns the descriptions matching a set of concept IDs from
#' a SNOMED dictionary
#'
#' @param conceptIds character or integer64 vector
#' @param include_synonyms whether to return only the Fully Specified
#'    Name (default) or all synonyms
#' @param active_only whether to include only active descriptions
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @return a data.table with the following columns: id, conceptId, type
#'   (only if include_synonyms = TRUE), term,
#'   active (only if active_only = FALSE)
#' @export
#' @examples
description <- function(conceptIds, include_synonyms = FALSE,
	active_only = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	# Check that conceptIds is a vector of strings or integer64 values

	# FSN     '900000000000003001'
	# Synonym '900000000000013009'
	TOMATCH <- data.table(conceptId = checkConcepts(conceptIds))
	if (include_synonyms == FALSE){
		OUT <- SNOMED$DESCRIPTION[TOMATCH, on = 'conceptId'][
			typeId == as.integer64('900000000000003001')][,
			list(id, conceptId, term, active)]
	} else {
		OUT <- SNOMED$DESCRIPTION[TOMATCH, on = 'conceptId'][,
			list(id, conceptId,
			type = ifelse(typeId == as.integer64('900000000000003001'),
			'Fully specified name', 'Synonym'), term, active)]
	}
	if (active_only & inactiveIncluded(SNOMED)){
		OUT <- OUT[active == TRUE]
	}
	if (active_only){
		OUT[, active := NULL]
	}
	OUT[]
}
