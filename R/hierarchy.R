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

#' Whether SNOMED CT concepts have particular attributes
#'
#' For each concept in the , whether it has the attribute
#' in the second list. Returns a vector of Booleans.
#'
#' @param sourceIds character or integer64 vector of SNOMED concept IDs
#'   for children, recycled if necessary
#' @param destinationIds character or integer64 vector of SNOMED concept
#'   IDs for parents, recycled if necessary
#' @param typeIds character or integer64 vector of SNOMED concept IDs
#'   for renationship types, recycled if necessary.
#'   Defaults to 116680003 = 'Is a' (child/parent)
#' @param SNOMED environment containing a SNOMED dictionary
#' @param tables character vector of relationship tables to use
#' @return a vector of Booleans stating whether the attribute exists
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' hasAttributes(conceptId('Heart failure'),
#'   conceptId('Heart structure'), conceptId('Finding site'))
hasAttributes <- function(sourceIds, destinationIds,
	typeIds = as.integer64('116680003'),
	SNOMED = get('SNOMED', envir = globalenv()), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
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

#' Retrieve all attributes of a set of SNOMED CT concepts
#'
#' Returns the portion of the SNOMED CT relationship tables containing
#' relationships where the given concepts are either the source or the 
#' destination.
#'
#' @param conceptId character or integer64 vector of SNOMED concept IDs
#' @param SNOMED environment containing a SNOMED dictionary
#' @param tables character vector of relationship tables to use
#' @return a data.table with the following columns: 
#'   sourceId (concept ID of source for relationship),
#'   destinationId (concept ID of source for relationship),
#'   typeId (concept ID of relationship type),
#'   typeName (description of relationship type)
#'
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' attributes(conceptId('Heart failure'))
attributes <- function(conceptIds,
	SNOMED = get('SNOMED', envir = globalenv()), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
	# Retrieves a table of attributes for a given set of concepts
	# add matches and combine Boolean

	OUT <- do.call('rbindlist', lapply(tables, function(table){
		get(table, envir = SNOMED)[
			sourceId %in% conceptIds | destinationId %in% conceptIds,
			list(sourceId, destinationId, typeId, relationshipGroup)]
	}), use.names = TRUE, fill = TRUE)
	OUT[, sourceDesc := description(sourceId)]
	OUT[, destinationDesc := description(destinationId)]
	OUT[, typeDesc := description(typeId)]
	OUT
	
	TOMATCH <- data.table(sourceId = checkConcepts(conceptIds),
		destinationId = checkConcepts(destinationIds),
		typeId = checkConcepts(typeIds))
	
	OUT <- 
	
	# add matches and combine Boolean
	for (table in tables){
		
	}
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
	
}

#' Retrieves semantic types using the text 'tag' in the description
#'
#' IN PROGRESS
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
#' @param SNOMED environment containing a SNOMED dictionary
#' @return a character vector of semantic tags corresponding to the conceptIDs 
#'   
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' semanticType(conceptId(c('Heart failure', 'Is a')))
semanticType <- function(conceptIds,
	SNOMED = get('SNOMED', envir = globalenv())){
	conceptIds <- checkConcepts(conceptIds)
	descriptions <- description(conceptIds)
	setkey(descriptions, 'conceptId')
	descriptions[I(conceptIds)]
}

#' Retrieves closest single ancestor within a given set of SNOMED CT concepts
#'
#' Returns a . If multiple ancestors are possible, the concept is not
#' translated. This can be used to translate concepts into simpler forms
#' for display, e.g. 
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
#' @param SNOMED environment containing a SNOMED dictionary
#' @param tables character vector of relationship tables to use
#' @return a data.table with the following columns:
#'   
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' closestSingleAncestor(conceptId('Fetal heart failure'),
#'   conceptId(c('Heart failure', 'Acute heart failure')))
closestSingleAncestor <- function(conceptIds, ancestorIds,
	SNOMED = get('SNOMED', envir = globalenv()), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
	
	matched <- rep(FALSE, len(conceptIds))
	recursionlimit <- 10
	while(any(matched == FALSE) & recursionlimit > 0){
		conceptIds[matched == FALSE]
		recursionlimit <- recursionlimit - 1
	}
}
