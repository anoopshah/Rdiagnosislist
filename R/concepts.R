#' Check if inactive terms are included in SNOMED CT dictionary
#'
#' Checks the active_only flag in the metadata of a SNOMED
#' environment to determine whether inactive terms are
#' included
#'
#' @param SNOMED environment containing SNOMED dictionary, defaults
#'   to an object named 'SNOMED' in the global environment
#' @return TRUE or FALSE (logical vector of length one)
#' @export
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- sampleSNOMED()
#' inactiveIncluded(TEST)
#' assign('metadata', list(active_only = TRUE), envir = TEST)
#' inactiveIncluded(TEST)
inactiveIncluded <- function(SNOMED = get('SNOMED', envir = globalenv())){
	if (is.null(SNOMED$metadata$active_only)){
		TRUE
	} else if (SNOMED$metadata$active_only == TRUE){
		FALSE
	} else {
		TRUE
	}
}

#' Returns the SNOMED CT concept IDs for a set of terms
#'
#' Carries out an exact or regular expression match to
#' return the concept ID for a set of search terms
#'
#' @param terms character vector of terms to match
#' @param active_only whether or not to include inactive concepts
#' @param exact_match if TRUE, only an exact (case sensitive)
#'   match is performed. If FALSE, a regular expression match
#'   is performed.
#' @param unique whether to include no more than one instance of each
#'   SNOMED CT concept
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @param ... additional arguments to send to grepl if using
#'   regular expression matching
#' @return a vector of unique SNOMED CT concept IDs in integer64 format
#' @import data.table
#' @export
#' @examples
#' conceptId('Heart failure', SNOMED = sampleSNOMED())
conceptId <- function(terms, active_only = TRUE,
	exact_match = TRUE, unique = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	# Declare names to be used for non-standard evaluation for R CMD check
	active <- NULL
	
	if (exact_match){
		MATCHED <- SNOMED$DESCRIPTION[data.table(term = as.character(terms)),
			list(active, conceptId), on = 'term']
	} else {
		matched <- rep(FALSE, nrow(SNOMED$DESCRIPTION))
		for (x in terms){
			matched <- matched | grepl(x, SNOMED$DESCRIPTION$term)
		}
		MATCHED <- SNOMED$DESCRIPTION[matched, list(active, conceptId)]
	}
	
	# Limit to active descriptions if needed
	if (active_only & inactiveIncluded(SNOMED)){
		MATCHED <- MATCHED[active == TRUE]
	}
	
	# Limit to active concepts
	if (active_only){
		if (unique){
			unique(SNOMED$CONCEPT[MATCHED[, list(id = conceptId)], on = 'id'][
				active == TRUE]$id)
		} else {
			SNOMED$CONCEPT[MATCHED[, list(id = conceptId)], on = 'id'][
				active == TRUE]$id
		}
	} else {
		if (unique){
			unique(MATCHED$conceptId)
		} else {
			MATCHED$conceptId
		}
	}
}

#' Check that concept IDs are in the correct format
#' 
#' Checks if a vector of conceptIds of type integer64, and 
#' converts them to integer64 if necessary.
#'
#' @param conceptIds character or integer64 vector 
#' @return conceptIds in integer64 format
#' @importFrom bit64 as.integer64
#' @export
#' @examples
#' checkConcepts('900000000000003001')
checkConcepts <- function(conceptIds){
	conceptIds <- unlist(conceptIds) # ensure that it is a vector
	
	if (class(conceptIds) == 'character'){
		return(as.integer64(conceptIds))
	} else if (class(conceptIds) == 'integer64'){
		# correct format
		return(conceptIds)
	} else {
		stop('conceptId must be supplied in character or integer64 format; ',
			class(conceptIds), ' is not acceptable.')
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
#' myconcepts <- conceptId('Heart failure', SNOMED = sampleSNOMED())
#' description(myconcepts, include_synonyms = FALSE, SNOMED = sampleSNOMED())
description <- function(conceptIds,
	include_synonyms = FALSE, active_only = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	# Check that conceptIds is a vector of strings or integer64 values
	# FSN     '900000000000003001'
	# Synonym '900000000000013009'
	
	# Declare names used for non-standard evaluation for R CMD check
	id <- term <- active <- typeId <- NULL
	
	CONCEPTS <- data.table(conceptId = checkConcepts(conceptIds),
		order = seq_along(conceptIds))
	TOMATCH <- data.table(conceptId = unique(CONCEPTS$conceptId))
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
	# Restore original order
	OUT <- OUT[CONCEPTS, on = 'conceptId']
	setkey(OUT, order)
	OUT[, order := NULL]
	# Remove inactive terms if necessary
	if (active_only){
		if (inactiveIncluded(SNOMED)){
			OUT <- OUT[active == TRUE]
		}
		OUT[, active := NULL]
	}
	OUT[]
}

