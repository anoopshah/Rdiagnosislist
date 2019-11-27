
checkActive <- function(active_only, SNOMED){
	# Checks SNOMED metadata to determine whether inactive terms are
	# included in dictionaries
	if (active_only){
		# Might need to check for term activity if SNOMED contains
		# inactive terms
		if (is.null(SNOMED$metadata$active_only)){
			TRUE
		} else if (SNOMED$metadata$active_only == TRUE){
			FALSE
		} else {
			TRUE
		}
	} else {
		FALSE
	}
}

conceptId <- function(term, SNOMED, active_only = TRUE,
	exact_match = TRUE){
	check_active <- checkActive(active_only, SNOMED)
	
	if (exact_match){
		tomatch <- data.table(term = term)
		MATCHED <- SNOMED$DESCRIPTION[tomatch, on = 'term'][,
			.(active, conceptId)]
	} else {
		tomatch <- term
		MATCHED <- SNOMED$DESCRIPTION[term %like% tomatch][,
			.(active, conceptId)]
	}
	if (active_only){
		MATCHED <- MATCHED[active == TRUE]
	}
	MATCHED <- MATCHED[, .(id = conceptId)]
	if (check_active){
		unique(SNOMED$CONCEPT[MATCHED, on = 'id'][active == TRUE]$id)
	} else {
		unique(MATCHED$id)
	}
}

description <- function(conceptIds, SNOMED,
	include_synonyms = FALSE, active_only = TRUE){
	check_active <- checkActive(active_only, SNOMED)
	
	# FSN     '900000000000003001'
	# Synonym '900000000000013009'
	TOMATCH <- data.table(conceptId = as.integer64(conceptIds))
	if (include_synonyms == FALSE){
		OUT <- SNOMED$DESCRIPTION[TOMATCH, on = 'conceptId'][
			typeId == as.integer64('900000000000003001')][,
			list(id, conceptId, term, active)]
	} else {
		OUT <- SNOMED$DESCRIPTION[TOMATCH][,
			list(id, conceptId,
			type = ifelse(typeId == as.integer64('900000000000003001'),
			'Fully specified name', 'Synonym'), term, active)]
	}
	if (active_only){
		OUT <- OUT[active == TRUE]
		OUT[, active := NULL]
	}
	return(OUT)
}
