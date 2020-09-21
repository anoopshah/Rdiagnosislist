
inactiveIncluded <- function(SNOMED = get('SNOMED', envir = globalenv())){
	# Checks SNOMED metadata to determine whether inactive terms are
	# included in dictionaries
	if (is.null(SNOMED$metadata$active_only)){
		TRUE
	} else if (SNOMED$metadata$active_only == TRUE){
		FALSE
	} else {
		TRUE
	}
}


conceptId <- function(term, active_only = TRUE,
	exact_match = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	
	if (exact_match){
		tomatch <- data.table(term = term)
		MATCHED <- SNOMED$DESCRIPTION[tomatch, on = 'term'][,
			.(active, conceptId)]
	} else {
		tomatch <- term
		MATCHED <- SNOMED$DESCRIPTION[term %like% tomatch][,
			.(active, conceptId)]
	}
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

checkConcepts <- function(conceptIds){
	# Checks that a set of conceptIds is valid, and converts to integer64 if necessary.
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
