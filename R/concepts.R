
conceptId <- function(term, activeOnly = TRUE){
	if (activeOnly){
		activeOK <- 1
	} else {
		activeOK <- c(0,1)
	}
	tomatch <- data.table(term = term)
	setkey(DESCRIPTION, term)
	unique(DESCRIPTION[tomatch][conceptId %in% CONCEPT[active %in% activeOK]$id][
		active %in% activeOK]$conceptId)
}

description <- function(conceptIds, includeSynonyms = FALSE,
	activeOnly = TRUE){
	if (activeOnly){
		activeOK <- 1
	} else {
		activeOK <- c(0,1)
	}
	# FSN     '900000000000003001'
	# Synonym '900000000000013009'
	if (includeSynonyms == FALSE){
		DESCRIPTION[conceptId %in% conceptIds][
			typeId == as.integer64('900000000000003001')][,
			list(id, conceptId, term)]
	} else {
		DESCRIPTION[conceptId %in% conceptIds][
			conceptId %in% CONCEPT[active %in% activeOK]$id][,
			list(id, conceptId,
			type = ifelse(typeId == as.integer64('900000000000003001'),
			'Fully specified name', 'Synonym'), term)][order(type, term)]
	}
}
