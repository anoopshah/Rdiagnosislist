#' Obtain related concepts for a set of SNOMED CT concepts
#'
#' Returns concepts with a particular relation to a supplied set of
#' SNOMED CT concepts
#'
#' @param conceptIds character or integer64 vector
#' @param typeId concept ID of relationship type.
#'   Defaults to 116680003 = Is a
#' @param tables vector of names of relationship table(s) to use;
#'   by default use both RELATIONSHIP and STATEDRELATIONSHIP
#' @param reverse whether to reverse the relationship
#' @param recursive 
#' @return a data.table with the following columns: id, conceptId, type
#'   (only if include_synonyms = TRUE), term,
#'   active (only if active_only = FALSE)
#' @export
#' @examples
#' # Load sample SNOMED CT dictionary
#' SNOMED <- sampleSNOMED()
#'
#' # Example: anatomical site of a finding
#' findingSite <- function(x){
#'   relatedConcepts(conceptId(x),
#'     typeId = conceptId('Finding site'))
#' }
#' 
#' description(findingSite('Heart failure'))
#' # Heart structure (body structure)
relatedConcepts <- function(conceptIds,
	typeId = as.integer64('116680003'),
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP'),
	reverse = FALSE, recursive = FALSE, active_only = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	# Returns the original concepts and the linked concepts

	conceptIds <- checkConcepts(conceptIds)
	typeId <- checkConcepts(typeId)
	TOLINK <- data.table(sourceId = conceptIds, typeId = typeId)
	setkey(TOLINK, sourceId, typeId)
	OUT <- data.table(active = logical(0),
		conceptId = bit64::integer64(0))
	# Retrieve relationship table
	addRelationship <- function(tablename, OUT){
		TABLE <- get(tablename, envir = SNOMED)
		if (reverse){
			setkey(TABLE, destinationId, typeId)
			OUT <- rbind(OUT, TABLE[TOLINK][,
				list(active, conceptId = sourceId)])
		} else {
			setkey(TABLE, sourceId, typeId)
			OUT <- rbind(OUT, TABLE[TOLINK][,
				list(active, conceptId = destinationId)])
		}
		OUT
	}

	# Add relationships from each table
	for (table in tables){
		OUT <- addRelationship(table, OUT)
	}

	# Limit to active terms if required
	if (active_only){
		out <- OUT[active == TRUE]$conceptId
	} else {
		out <- OUT$conceptId
	}

	# Recursion if appropriate
	if (recursive == TRUE){
		out <- sort(unique(c(conceptIds, out)))
		if (length(conceptIds) < length(out)){
			# Recurse
			return(relatedConcepts(conceptIds = out,
				typeId = typeId, SNOMED = SNOMED, tables = tables,
				reverse = reverse, recursive = TRUE,
				active_only = active_only))
		} else {
			return(unique(out))
		}
	} else {
		return(unique(out))
	}
}

#' Parents and children of SNOMED CT concepts
#'
#' Returns concepts with 'Is a' or inverse 'Is a'
#' relationship with a set of target concepts
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
#' @param SNOMED environment containing a SNOMED dictionary
#' @param childIds character or integer64 vector of SNOMED concept IDs for children
#' @param parentIds character or integer64 vector of SNOMED concept IDs for parents
#' @param ... other arguments to pass to relatedConcepts
#' @return a bit64 vector of SNOMED CT concepts
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' description(parents(conceptId('Heart failure')))
#' description(children(conceptId('Heart failure')))
#' hasParents(conceptId('Left heart failure'), conceptId('Heart failure'))
#' hasChildren(conceptId('Heart failure'), conceptId('Left heart failure'))
parents <- function(conceptIds,
	SNOMED = get('SNOMED', envir = globalenv()), ...){
	conceptIds <- checkConcepts(unique(conceptIds))
	parentIds <- relatedConcepts(conceptIds = conceptIds,
		typeId = as.integer64('116680003'),
		reverse = FALSE, recursive = TRUE, SNOMED = SNOMED, ...)
	# Exclude originals (note cannot use setdiff function with int64)
	parentIds[!(parentIds %in% conceptIds)]
}

#' @rdname parents
children <- function(conceptIds,
	SNOMED = get('SNOMED', envir = globalenv()), ...){
	conceptIds <- checkConcepts(unique(conceptIds))
	childIds <- relatedConcepts(conceptIds = conceptIds,
		typeId = as.integer64('116680003'),
		reverse = TRUE, recursive = TRUE, SNOMED = SNOMED, ...)
	# Exclude originals (note cannot use setdiff function with int64)
	childIds[!(childIds %in% conceptIds)]
}

#' @rdname parents
hasParents <- function(childIds, parentIds,
	SNOMED = get('SNOMED', envir = globalenv()), ...){
	hasAttributes(childIds, parentIds, SNOMED,
		typeId = as.integer64('116680003'), ...)
}

#' @rdname parents
hasChildren <- function(parentIds, childIds,
	SNOMED = get('SNOMED', envir = globalenv()), ...){
	hasAttributes(childIds, parentIds, SNOMED,
		typeId = as.integer64('116680003'), ...)
}

#' Whether a SNOMED CT concept has a particular attribute
#'
#' For each concept in the , whether it has the attribute
#' in the second list. Returns a vector of Booleans.
#'
#' @param sourceIds character or integer64 vector of SNOMED concept IDs for children
#' @param destinationIds character or integer64 vector of SNOMED concept IDs for parents
#' @param typeIds character or integer64 vector of SNOMED concept IDs
#'   for renationship types. Defaults to 116680003 = 'Is a' (child/parent)
#' @param SNOMED environment containing a SNOMED dictionary
#' @param tables character vector of relationship tables to use
#' @return a bit64 vector of SNOMED CT concepts
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' hasAttributes(parents(conceptId('Heart failure')))
hasAttributes <- function(sourceIds, destinationIds,
	typeIds = as.integer64('116680003'),
	SNOMED = get('SNOMED', envir = globalenv()), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
	# IN PROGRESS
	# For each concept in the first list, whether it has the attribute
	# in the second list
	# Returns a vector of Booleans
	# Defaults to whether the 
	
	# Recycle refId and typeId if necessary 
	#TEMP <- data.table(conceptIds)
	#RELATIONSHIP
	#STATEDRELATIONSHIP
	
	# TO check ois a data.table 
	
	# Check with relationshpi then statedrelationship  (source, dest, type)
	# return booolean of matches
	# then check with other and to an OR
	TOMATCH <- data.table(sourceId = checkConcepts(sourceIds),
		destinationId = checkConcepts(destinationIds),
		typeId = checkConcepts(typeIds))
	
	# add matches and combine Boolean
	addRelationship <- function(tablename, out){
		TABLE <- as.data.table(get(tablename, envir = SNOMED))
		out | !is.na(TABLE[TOMATCH,
			on = c('sourceId', 'destinationId', 'typeId')]$id)
	}
	
	# Blank output logical vector
	out <- logical(nrow(TOMATCH))
	# Add relationships from each table
	for (table in tables){
		out <- addRelationship(table, out)
	}
	out
}

#' Returns all attributes of a given SNOMED CT concept
#'
#' For each concept in the , whether it has the attribute
#' in the second list. Returns a vector of Booleans.
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
#' @param SNOMED environment containing a SNOMED dictionary
#' @param tables character vector of relationship tables to use
#' @return a data.table with the following columns:
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' attributes(conceptId('Heart failure'))
hasAttributes <- function(conceptIds,
	SNOMED = get('SNOMED', envir = globalenv()), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
	# Retrieves a table of attributes for a given set of concepts
	attribute_ids <- attributeId(attributes)
	rbindlist(lapply(attribute_ids, function(x){
		relatedConcepts(conceptIds, typeId = x,
		SNOMED = SNOMED, ...)
	}))
}
