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
#' @param conceptIds character or integer64 vector
#' @param SNOMED environment containing a SNOMED dictionary
#' @param ... other arguments to pass to relatedConcepts
#' @return a bit64 vector of SNOMED CT concepts
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' description(parents(conceptId('Heart failure')))
#' description(children(conceptId('Heart failure')))
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

hasChildren <- function(childIds, parentIds,
	SNOMED = get('SNOMED', envir = globalenv()), ...){
	hasAttributes(childIds, parentIds, SNOMED,
		typeId = as.integer64('116680003'), ...)
}

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

attributes <- function(conceptIds, attributes,
	SNOMED = get('SNOMED', envir = globalenv()), ...){
	# Retrieves a table of attributes for a given set of concepts
	attribute_ids <- attributeId(attributes)
	rbindlist(lapply(attribute_ids, function(x){
		relatedConcepts(conceptIds, typeId = x,
		SNOMED = SNOMED, ...)
	}))
}

	# Converts an attribute as a character string to a conceptId for
	# the attribute. However, if attribute is already a conceptId
	# it is left as it is

attributeId <- function(attributes,
	SNOMED = get('SNOMED', envir = globalenv())){
	attribute_ids <- checkConcepts(attributes)
	if (any(is.na(attribute_ids))){
		tomatch <- is.na(attribute_ids)
		TOMATCH <- data.table(term = tolower(as.character(attributes[tomatch])))
		attribute_ids[tomatch] <- TOMATCH[SNOMED$DESCRIPTION, on = 'term']$conceptId
	}
	attribute_ids
}
