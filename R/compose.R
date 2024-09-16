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
#' composeLookup lookup table created by createComposeLookup
#' attributes_conceptIds SNOMED concept Ids of attributes of concept
#'   e.g. laterality, severity, acuteness
#' due_to_conceptIds SNOMED concept Ids of cause
#' without_conceptIds SNOMED concept Ids of conditions stated to be
#'   absent
#' with_conceptIds SNOMED concept Ids of conditions also present
#' @param SNOMED environment containing SNOMED CT tables
#' @return a refined SNOMED concept Id
#' @examples
#' # Not run
#' # refineSNOMEDfinding
refineSNOMEDfinding <- function(conceptId, CDB, composeLookup,
	attributes_conceptIds = bit64::as.integer64(0),
	due_to_conceptIds = bit64::as.integer64(0),
	without_conceptIds = bit64::as.integer64(0),
	with_conceptIds = bit64::as.integer64(0),
	SNOMED = getSNOMED()){
	# Input: a SNOMEDfindings
	# Output: a SNOMEDfindings 
	# Also consider including 'causing'
	# causing_conceptIds = bit64::as.integer64(0) - not using currently
	
	# use decomposition of self to find ancestor and try different
	# decompositions
	# return either a single best match or multiple matches as per
	# requirements (e.g. multiple may be more useful for research)
	
	# must have columns: conceptId, attributes
	# optional columns: severity, body site, due_to, causing
	conceptId <- as.SNOMEDconcept(conceptId, SNOMED = SNOMED)
	
	expand <- function(conceptIds){
		if (length(conceptIds) == 0){
			return(bit64::as.integer64(0))
		}
		conceptIds <- unique(c(conceptIds,
			CDB$OVERLAP[findingId %in% conceptIds]$otherId,
			CDB$OVERLAP[otherId %in% conceptIds]$findingId))
		ancestors(conceptIds, SNOMED = SNOMED,
			TRANSITIVE = CDB$TRANSITIVE, include_self = TRUE)
	}
	
	limitToFindings <- function(conceptIds){
		intersect(conceptIds, CDB$FINDINGS$conceptId)
	}
	
	# For 'causing': use due_to in reverse - not currently doing
#~ 	if (length(causing_conceptIds) > 0){
#~ 		# Reverse search on 'due to'
#~ 		# Attributes do not apply
#~ 		origconceptId <- conceptId
#~ 		return(rbindlist(lapply(1:length(causing_conceptIds), function(x){
#~ 			refineSNOMEDfinding(conceptId = causing_conceptIds[x],
#~ 				CDB = CDB, attributes_conceptIds := bit64::as.integer64(0),
#~ 				due_to_conceptIds = origconceptId,
#~ 				causing_conceptIds = bit64::as.integer64(0)
#~ 				without_conceptIds = without_conceptIds, SNOMED = SNOMED)
#~ 		})))
#~ 	}
	
	# Find highest number of attribute fields in this composeLookup
	max_attr <- max(as.numeric(sub('^attr_', '', 
		names(composeLookup)[names(composeLookup) %like% '^attr_'])))
	
	composeLookup[, valid := rootId %in% ancestors(conceptId,
		SNOMED = SNOMED, TRANSITIVE = CDB$TRANSITIVE, include_self =TRUE)]
	
	if (length(without_conceptIds) > 0){
		composeLookup[, valid := valid & (is.na(without) |
			without %in% limitToFindings(expand(without_conceptIds)))]
	}

	if (length(with_conceptIds) > 0){
		composeLookup[, valid := valid & (is.na(with) |
			with %in% limitToFindings(expand(with_conceptIds)))]
	}
	
	if (length(due_to_conceptIds) > 0){
		composeLookup[, valid := valid & (is.na(due_to) |
			due_to %in% limitToFindings(expand(due_to_conceptIds)))]
	}

	if (length(attributes_conceptIds) > 0){
		attributes_search <- expand(attributes_conceptIds)
		for (j in 1:max_attr){
			attr_x_name <- paste0('attr_', j)
			composeLookup[, valid := valid &
				(is.na(get(attr_x_name)) |
				get(attr_x_name) %in% attributes_search)]
		}
	}
	
	if (sum(composeLookup$valid) == 0){
		return(conceptId)
	}
	
	# Remove all matches which are an ancestor of another match
	matchIds <- unique(as.SNOMEDconcept(
		composeLookup[valid == TRUE]$origId, SNOMED = SNOMED))
	i <- 1
	while (i <= length(matchIds)){
		ancIds <- ancestors(matchIds[i], SNOMED = SNOMED,
			TRANSITIVE = CDB$TRANSITIVE, include_self = FALSE)
		if (any(matchIds %in% ancIds)){
			matchIds <- setdiff(matchIds, ancIds)
			i <- 1
		} else {
			i <- i + 1
		}
	}
	
	# Return the matches
	as.SNOMEDconcept(matchIds)
}


