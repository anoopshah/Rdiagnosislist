#' Check if inactive terms are included in SNOMED dictionary
#'
#' Checks the active_only flag in the metadata of a SNOMED
#' environment to determine whether inactive terms are
#' included
#'
#' @param SNOMED environment containing SNOMED dictionary, defaults
#'   to an object named 'SNOMED' in the global environment
#' @return TRUE or FALSE (logical vector of length one)
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' ALL <- new.env()
#' assign('metadata', list(active_only = TRUE), envir = ALL)
#' ACTIVE_ONLY <- new.env()
#' assign('metadata', list(active_only = TRUE), envir = ACTIVE_ONLY)
#' inactiveIncluded(ALL)
#' inactiveIncluded(ACTIVE_ONLY)
inactiveIncluded <- function(SNOMED = get('SNOMED', envir = globalenv())){
	if (is.null(SNOMED$metadata$active_only)){
		TRUE
	} else if (SNOMED$metadata$active_only == TRUE){
		FALSE
	} else {
		TRUE
	}
}

#' Sample SNOMED CT dictionary
#'
#' Returns an environment containing a selection of SNOMED CT
#' terms, their relationships and descriptions which are
#' provided with the package
#'
#' @return environment containing four data.table objects:
#'   CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#'   and a list named 'metadata'
#' @export
#' @examples
#' TEST <- sampleSNOMED()
#' inactiveIncluded(TEST)
#' conceptId('Heart failure', TEST)
sampleSNOMED <- function(){
	SNOMED <- new.env()
	data(CONCEPT, envir = SNOMED)
	data(RELATIONSHIP, envir = SNOMED)
	data(STATEDRELATIONSHIP, envir = SNOMED)
	data(DESCRIPTION, envir = SNOMED)
	assign('metadata', value = list(source = 'sample',
		active_only = FALSE), envir = SNOMED)
	return(SNOMED)
}

#' Returns the SNOMED CT concept IDs for a set of terms
#'
#' Carries out an exact or regular expression match to
#' return the concept ID for a set of search terms
#'
#' @param terms character vector of terms to match, or a single character
#'   string containing a regular expression to match (if exact_match = FALSE)
#' @param active_only whether or not to include inactive concepts
#' @param exact_match if TRUE, only an exact (case sensitive)
#'   match is performed. If FALSE, a regular expression match
#'   is performed.
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @return a vector of unique SNOMED CT concept IDs in integer64 format
#' @export
#' @examples
#' conceptId('Heart failure', SNOMED = sampleSNOMED())
conceptId <- function(terms, active_only = TRUE,
	exact_match = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	DESC <- setDT(SNOMED$DESCRIPTION)
	if (exact_match){
		MATCHED <- DESC[data.table(term = terms), list(active, conceptId), on = 'term']
	} else {
		MATCHED <- DESC[term %like% terms, list(active, conceptId)]
	}
	
	if (active_only & inactiveIncluded(SNOMED)){
		MATCHED <- MATCHED[active == TRUE]
	}
	MATCHED <- MATCHED[, .(id = conceptId)]
	if (active_only){
		# Note that this filtering is essential - even if inactive
		# concepts are excluded, they may be included in the
		# descriptions.
		unique(setDT(SNOMED$CONCEPT)[MATCHED, on = 'id'][active == TRUE]$id)
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
#' @export
#' @examples
#' checkConcepts('900000000000003001')
#' checkConcepts(as.integer64('900000000000003001'))
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
#' myconcepts <- conceptId('Heart failure', SNOMED = sampleSNOMED())
#' description(myconcepts, include_synonyms = FALSE, SNOMED = sampleSNOMED())
description <- function(conceptIds, include_synonyms = FALSE,
	active_only = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	# Check that conceptIds is a vector of strings or integer64 values

	# FSN     '900000000000003001'
	# Synonym '900000000000013009'
	TOMATCH <- data.table(conceptId = checkConcepts(conceptIds))
	if (include_synonyms == FALSE){
		OUT <- setDT(SNOMED$DESCRIPTION)[TOMATCH, on = 'conceptId'][
			typeId == as.integer64('900000000000003001')][,
			list(id, conceptId, term, active)]
	} else {
		OUT <- setDT(SNOMED$DESCRIPTION)[TOMATCH, on = 'conceptId'][,
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
