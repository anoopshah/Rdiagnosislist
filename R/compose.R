#' Select more specific SNOMED CT concepts based on attributes
#'
#' Finds the most specific SNOMED CT concepts that matches the
#' combination of a root concept and attributes. Based on a
#' composeLookup table
#'
#' @param conceptId SNOMED CT concept to refine
#' @param CDB SNOMED CT concept database, as created by createCDB.
#'   An environment containing the following data tables: FINDINGS,
#'   QUAL, CAUSES, BODY, FINDINGS, OTHERSUB, OVERLAP, TRANSITIVE
#' @param composeLookup lookup table created by createComposeLookup
#' @param attributes_conceptIds SNOMED concept Ids of attributes of concept
#'   e.g. laterality, severity, acuteness
#' @param due_to_conceptIds SNOMED concept Ids of cause
#' @param without_conceptIds SNOMED concept Ids of conditions stated to be
#'   absent
#' @param with_conceptIds SNOMED concept Ids of conditions also present
#' @param SNOMED environment containing SNOMED CT tables
#' @param show_all_matches whether to stop if an exact match is found,
#'   or continue to search for all potential matches
#' @return a refined SNOMED concept Id
#' @examples
#' # Not run
#' # refineSNOMEDfinding
compose <- function(conceptId, CDB, composeLookup,
	attributes_conceptIds = bit64::integer64(0),
	due_to_conceptIds = bit64::integer64(0),
	without_conceptIds = bit64::integer64(0),
	with_conceptIds = bit64::integer64(0),
	SNOMED = getSNOMED, show_all_matches = FALSE){
	
	NA_concept <- bit64::as.integer64(NA)
	setattr(NA_concept, 'class', c('SNOMEDconcept', 'integer64'))
	
	# Harmonise and append NA for each attribute
	expand <- function(x){
		if (length(x) == 0){
			return(bit64::integer64(0))
		} else {
			x <- as.SNOMEDconcept(x, SNOMED = SNOMED)
		}
		x <- unique(c(x,
			CDB$OVERLAP[findingId %in% x]$otherId,
			CDB$OVERLAP[otherId %in% x]$findingId))
		ancestors(x, SNOMED = SNOMED,
			TRANSITIVE = CDB$TRANSITIVE, include_self = TRUE)
	}
	
	harmonise <- function(x, limitToFindings = FALSE){
		if (is.null(x)) x <- NA_concept
		if (length(x) == 0) x <- NA_concept
		if (limitToFindings){
			union(intersect(expand(
				as.SNOMEDconcept(bit64::as.integer64(x), SNOMED = SNOMED)
				), CDB$FINDINGS$conceptId), NA_concept)
		} else {
			union(expand(as.SNOMEDconcept(bit64::as.integer64(x),
				SNOMED = SNOMED)), NA_concept)
		}
	}
	
	# Ensure correct data types
	due_to_search <- harmonise(due_to_conceptIds, TRUE)
	attributes_exact <- union(union(as.SNOMEDconcept(
		bit64::as.integer64(due_to_conceptIds)),
		as.SNOMEDconcept(bit64::as.integer64(attributes_conceptIds))),
		NA_concept)
	attributes_search <- harmonise(attributes_exact)
	without_search <- harmonise(without_conceptIds, TRUE)
	with_search <- harmonise(with_conceptIds, TRUE)
	
	conceptId <- as.SNOMEDconcept(conceptId, SNOMED = SNOMED)
	root_search <- expand(conceptId)
	
	# Find highest number of attribute fields in this composeLookup
	SUBSET <- composeLookup[rootId %in% root_search &
		without %in% without_search &
		with %in% with_search &
		due_to %in% due_to_search &
		attr_1 %in% attributes_search]
	SUBSET_EXACT <- SUBSET[attr_1 %in% attributes_exact]
	
	# Find highest number of attribute fields in this composeLookup
	max_attr <- max(as.numeric(sub('^attr_', '', 
		names(composeLookup)[names(composeLookup) %like% '^attr_'])))
	
	if (max_attr > 1){
		for (j in 2:max_attr){
			attr_x_name <- paste0('attr_', j)
			if (nrow(SUBSET) > 0){
				SUBSET <- SUBSET[get(attr_x_name) %in% attributes_search]
			}
			# get() will create a warning if SUBSET_EXACT is an empty
			# data.table
			if (nrow(SUBSET_EXACT) > 0){
				SUBSET_EXACT <- SUBSET_EXACT[get(attr_x_name) %in%
					attributes_exact]
			}
		}
	}
	
	if (nrow(SUBSET) == 0){
		return(conceptId)
	}
	
	# Try for an exact match
	matchIds <- unique(as.SNOMEDconcept(SUBSET_EXACT$origId,
		SNOMED = SNOMED))
	
	# If no exact match, return all possible matches
	if (length(matchIds) == 0){
		matchIds <- unique(as.SNOMEDconcept(SUBSET$origId,
			SNOMED = SNOMED))
	}
	# Remove all matches which are an ancestor of another match
	i <- 1
	while (i <= length(matchIds)){
		ancIds <- ancestors(matchIds[i], SNOMED = SNOMED,
			TRANSITIVE = CDB$TRANSITIVE, include_self = FALSE)
		if (length(intersect(matchIds, ancIds)) > 0){
			matchIds <- setdiff(matchIds, ancIds)
			i <- 1
		} else {
			i <- i + 1
		}
	}
	
	# Return the matches
	as.SNOMEDconcept(matchIds)
}


OLDcompose <- function(conceptId, CDB, composeLookup,
	attributes_conceptIds = bit64::integer64(0),
	due_to_conceptIds = bit64::integer64(0),
	without_conceptIds = bit64::integer64(0),
	with_conceptIds = bit64::integer64(0),
	SNOMED = getSNOMED){
	
	# Harmonise and append NA for each attribute
	expand <- function(conceptIds){
		if (length(conceptIds) == 0){
			return(bit64::integer64(0))
		} else {
			conceptIds <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
		}
		conceptIds <- unique(c(conceptIds,
			CDB$OVERLAP[findingId %in% conceptIds]$otherId,
			CDB$OVERLAP[otherId %in% conceptIds]$findingId))
		ancestors(conceptIds, SNOMED = SNOMED,
			TRANSITIVE = CDB$TRANSITIVE, include_self = TRUE)
	}
	
	harmonise <- function(x, limitToFindings = FALSE){
		if (limitToFindings){
			unique(c(intersect(expand(
				as.SNOMEDconcept(bit64::as.integer64(x),
				SNOMED = SNOMED)), CDB$FINDINGS$conceptId), NA))
		} else {	
			unique(c(expand(as.SNOMEDconcept(bit64::as.integer64(x),
				SNOMED = SNOMED)), NA))
		}
	}
	
	# Ensure correct data types
	attributes_conceptIds <- harmonise(attributes_conceptIds)
	due_to_conceptIds <- harmonise(due_to_conceptIds)
	without_conceptIds <- harmonise(without_conceptIds)
	with_conceptIds <- harmonise(with_conceptIds)
	
	conceptId <- as.SNOMEDconcept(conceptId, SNOMED = SNOMED)
	root_search <- expand(conceptId)
	
	# Find highest number of attribute fields in this composeLookup
	max_attr <- max(as.numeric(sub('^attr_', '', 
		names(composeLookup)[names(composeLookup) %like% '^attr_'])))
	
	composeLookup[, valid := rootId %in% ancestors(conceptId,
		SNOMED = SNOMED, TRANSITIVE = CDB$TRANSITIVE, include_self =TRUE)]
	
	if (length(without_conceptIds) > 0){
		composeLookup[, valid := valid & (is.na(without) |
			without %in% limitToFindings(expand(without_conceptIds)))]
	} else {
		composeLookup[, valid := valid & is.na(without)]
	}

	if (length(with_conceptIds) > 0){
		composeLookup[, valid := valid & (is.na(with) |
			with %in% limitToFindings(expand(with_conceptIds)))]
	} else {
		composeLookup[, valid := valid & is.na(with)]
	}
	
	if (length(due_to_conceptIds) > 0){
		composeLookup[, valid := valid & (is.na(due_to) |
			due_to %in% limitToFindings(expand(due_to_conceptIds)))]
		attributes_conceptIds <- union(due_to_conceptIds,
			attributes_conceptIds)
	} else {
		composeLookup[, valid := valid & is.na(due_to)]
	}

	# due to may be included as an attribute
	# (e.g. 'osteoporotic fracture')
	if (length(attributes_conceptIds) > 0){
		attributes_search <- expand(attributes_conceptIds)
		for (j in 1:max_attr){
			attr_x_name <- paste0('attr_', j)
			composeLookup[, valid := valid &
				(is.na(get(attr_x_name)) |
				get(attr_x_name) %in% attributes_search)]
		}
	} else {
		composeLookup[, valid := valid & is.na(attr_1)]
	}
	
	if (sum(composeLookup$valid) == 0){
		return(conceptId)
	}
	
	# Remove all matches which are an ancestor of another match
	matchIds <- unique(as.SNOMEDconcept(
		composeLookup[valid == TRUE]$origId, SNOMED = SNOMED))

	# To rewrite this lookup more simply - to search on NA

	i <- 1
	while (i <= length(matchIds)){
		ancIds <- ancestors(matchIds[i], SNOMED = SNOMED,
			TRANSITIVE = CDB$TRANSITIVE, include_self = FALSE)
		if (length(intersect(matchIds, ancIds)) > 0){
			matchIds <- setdiff(matchIds, ancIds)
			i <- 1
		} else {
			i <- i + 1
		}
	}
	
	# Return the matches
	as.SNOMEDconcept(matchIds)
}


