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
#' @param recursive whether to re-apply the function on the outputs
#' @param active_only whether to limit the output to active concepts only
#' @param SNOMED environment containing a SNOMED dictionary
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
#'   relatedConcepts(as.SNOMEDconcept(x),
#'     typeId = as.SNOMEDconcept('Finding site'))
#' }
#' 
#' description(findingSite('Heart failure'))
#' # Heart structure (body structure)
relatedConcepts <- function(conceptIds,
	typeId = bit64::as.integer64('116680003'),
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP'),
	reverse = FALSE, recursive = FALSE, active_only = TRUE,
	SNOMED = getSNOMED()){
	# Returns the original concepts and the linked concepts

	active <- sourceId <- destinationId <- conceptId <- NULL

	conceptIds <- as.SNOMEDconcept(conceptIds)
	
	# If no concepts supplied, return an empty vector
	if (length(conceptIds) == 0){
		return(conceptIds)
	}
	
	typeId <- as.SNOMEDconcept(typeId)
	if (reverse){
		TOLINK <- data.table(destinationId = conceptIds, typeId = typeId)
	} else {
		TOLINK <- data.table(sourceId = conceptIds, typeId = typeId)
	}
	OUT <- data.table(active = logical(0),
		conceptId = bit64::integer64(0))
	# Retrieve relationship table
	addRelationship <- function(tablename, OUT){
		TABLE <- get(tablename, envir = SNOMED)
		if (reverse){
			OUT <- rbind(OUT, TABLE[TOLINK,
				on = c('destinationId', 'typeId')][,
				list(active, conceptId = sourceId)])
		} else {
			OUT <- rbind(OUT, TABLE[TOLINK,
				on = c('sourceId', 'typeId')][,
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
			return(as.SNOMEDconcept(unique(out)))
		}
	} else {
		return(as.SNOMEDconcept(unique(out)))
	}
}

#' Ancestors and descendants of SNOMED CT concepts
#'
#' Returns concepts with 'Is a' or inverse 'Is a'
#' relationship with a set of target concepts. 
#' Ancestors include parents and all higher relations.
#' Descendants include children and all lower relations.
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
#' @param SNOMED environment containing a SNOMED dictionary
#' @param ... other arguments to pass to relatedConcepts
#' @return a bit64 vector of SNOMED CT concepts
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' parents('Heart failure')
#' children('Heart failure')
#' ancestors('Heart failure')
#' descendants('Heart failure')
parents <- function(conceptIds,
	SNOMED = getSNOMED(), ...){
	conceptIds <- as.SNOMEDconcept(unique(conceptIds))
	parentIds <- relatedConcepts(conceptIds = conceptIds,
		typeId = bit64::as.integer64('116680003'),
		reverse = FALSE, recursive = FALSE, SNOMED = SNOMED, ...)
	# Exclude originals	
	if (length(parentIds) > 0){
		return(as.SNOMEDconcept(parentIds[!(parentIds %in% conceptIds)]))
	} else {
		return(parentIds)
	}
}

#' @rdname parents
#' @export
ancestors <- function(conceptIds,
	SNOMED = getSNOMED(), ...){
	conceptIds <- as.SNOMEDconcept(unique(conceptIds))
	parentIds <- relatedConcepts(conceptIds = conceptIds,
		typeId = bit64::as.integer64('116680003'),
		reverse = FALSE, recursive = TRUE, SNOMED = SNOMED, ...)
	# Exclude originals	
	if (length(parentIds) > 0){
		return(as.SNOMEDconcept(parentIds[!(parentIds %in% conceptIds)]))
	} else {
		return(parentIds)
	}
}

#' @rdname parents
#' @export
children <- function(conceptIds,
	SNOMED = getSNOMED(), ...){
	conceptIds <- as.SNOMEDconcept(unique(conceptIds))
	childIds <- relatedConcepts(conceptIds = conceptIds,
		typeId = bit64::as.integer64('116680003'),
		reverse = TRUE, recursive = FALSE, SNOMED = SNOMED, ...)
	# Exclude originals
	if (length(childIds) > 0){
		return(as.SNOMEDconcept(childIds[!(childIds %in% conceptIds)]))
	} else {
		return(childIds)
	}
}

#' @rdname parents
#' @export
descendants <- function(conceptIds,
	SNOMED = getSNOMED(), ...){
	conceptIds <- as.SNOMEDconcept(unique(conceptIds))
	childIds <- relatedConcepts(conceptIds = conceptIds,
		typeId = bit64::as.integer64('116680003'),
		reverse = TRUE, recursive = TRUE, SNOMED = SNOMED, ...)
	# Exclude originals
	if (length(childIds) > 0){
		return(as.SNOMEDconcept(childIds[!(childIds %in% conceptIds)]))
	} else {
		return(childIds)
	}
}

#' Whether SNOMED CT concepts have particular attributes
#'
#' For each concept in the first list, whether it has the attribute
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
#' hasAttributes(c('Heart failure', 'Acute heart failure'),
#'   c('Heart structure', 'Heart failure'),
#'   c('Finding site', 'Is a'))
hasAttributes <- function(sourceIds, destinationIds,
	typeIds = bit64::as.integer64('116680003'),
	SNOMED = getSNOMED(), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
	TOMATCH <- data.table(sourceId = as.SNOMEDconcept(sourceIds),
		destinationId = as.SNOMEDconcept(destinationIds),
		typeId = as.SNOMEDconcept(typeIds))
	
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
	return(out)
}

#' Retrieve all attributes of a set of SNOMED CT concepts
#'
#' Returns the portion of the SNOMED CT relationship tables containing
#' relationships where the given concepts are either the source or the 
#' destination.
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
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
#' attrConcept(as.SNOMEDconcept('Heart failure'))
attrConcept <- function(conceptIds,
	SNOMED = getSNOMED(), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
	# Retrieves a table of attributes for a given set of concepts
	# add matches and combine Boolean
	sourceId <- destinationId <- typeId <- relationshipGroup <- NULL
	sourceDesc <- destinationDesc <- typeDesc <- NULL

	MATCHSOURCE <- data.table(sourceId =
		as.SNOMEDconcept(conceptIds, SNOMED = SNOMED))
	MATCHDEST <- data.table(destinationId =
		as.SNOMEDconcept(conceptIds, SNOMED = SNOMED))
	OUT <- rbind(rbindlist(lapply(tables, function(table){
			get(table, envir = SNOMED)[MATCHSOURCE, on = 'sourceId',
			list(sourceId, destinationId, typeId, relationshipGroup)]
		}), use.names = TRUE, fill = TRUE),
		rbindlist(lapply(tables, function(table){
			get(table, envir = SNOMED)[MATCHDEST, on = 'destinationId',
			list(sourceId, destinationId, typeId, relationshipGroup)]
		}), use.names = TRUE, fill = TRUE)
	)
	OUT[, sourceDesc := description(sourceId, SNOMED = SNOMED)$term]
	OUT[, destinationDesc := description(destinationId,
		SNOMED = SNOMED)$term]
	OUT[, typeDesc := description(typeId, SNOMED = SNOMED)$term]
	return(OUT[])
}

#' Retrieves semantic types using the text 'tag' in the description
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
#' @param SNOMED environment containing a SNOMED dictionary
#' @return a character vector of semantic tags corresponding to the conceptIDs 
#'   
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' semanticType(as.SNOMEDconcept(c('Heart failure', 'Is a')))
semanticType <- function(conceptIds,
	SNOMED = getSNOMED()){
	tag <- term <- NULL
	
	conceptIds <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
	DESC <- description(conceptIds, SNOMED = SNOMED)
	DESC[, tag := sub('^.*\\(([[:alnum:]\\/\\+ ]+)\\)$', '\\1', term)]
	return(DESC$tag)
}

#' Retrieves closest single ancestor within a given set of SNOMED CT
#' concepts
#'
#' Returns a vector of SNOMED CT concept IDs for an ancestor of each
#' concept that is within a second list. If multiple ancestors are
#' included in the second list, the concept is not simplified (i.e.
#' the original concept ID is returned).
#' This functionality can be used to translate concepts into simpler
#' forms for display, e.g. 'Heart failure' instead of 'Heart failure
#' with reduced ejection fraction'.
#'
#' @param conceptIds character or integer64 vector of SNOMED concept IDs
#'   for concepts for which an ancestor is sought
#' @param ancestorIds character or integer64 vector of SNOMED concept IDs
#'   for possible ancestors
#' @param SNOMED environment containing a SNOMED dictionary
#' @param tables character vector of relationship tables to use
#' @return a data.table with the following columns:
#'   originalId (integer64) = original conceptId,
#'   ancestorId (integer64) = closest single ancestor, or original
#'   concept ID if no ancestor is included in the 
#'   
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' original_terms <- c('Systolic heart failure', 'Is a',
#'   'Heart failure with reduced ejection fraction',
#'   'Acute kidney injury due to circulatory failure (disorder)')
#' # Note in this example 'Is a' has no parents in ancestors,
#' # and acute kidney failure has two parents in ancestors
#' # so neither of the parents will be chosen.
#' # Also test out inclusion of duplicate concepts.
#'
#' ancestors <- simplify(c(as.SNOMEDconcept(original_terms),
#'   as.SNOMEDconcept(original_terms)[3:4]),
#'   as.SNOMEDconcept(c('Heart failure', 'Acute heart failure',
#'   'Cardiorenal syndrome (disorder)')))
#' print(cbind(original_terms, description(ancestors$ancestorId)$term))
simplify <- function(conceptIds, ancestorIds,
	SNOMED = getSNOMED(), 
	tables = c('RELATIONSHIP', 'STATEDRELATIONSHIP')){
	found <- keep_orig <- anymatch <- originalId <- NULL
	ancestorId <- conceptId <- NULL

	DATA <- data.table(conceptId = conceptIds,
		originalId = conceptIds, found = FALSE, anymatch = FALSE,
		keep_orig = FALSE, order = 1:length(conceptIds))
	# order = identifier for the original concept (in case of duplicates)
	# original = original concept
	# conceptId = candidate closest single ancestor
	# found = whether this row is a match to closest ancestor
	# anymatch = whether any match is found for this concept
	# keep_orig = whether to keep original because 0 or > 1 matches

	recursionlimit <- 10
	# Loop while any of the concepts are unmatched and recursion
	# limit is not reached
	while(any(DATA$anymatch == FALSE) & recursionlimit > 0){
		# Check for matches
		DATA[conceptId %in% ancestorIds, found := TRUE]
		# Keep original (ignore match) if more than one match
		DATA[, keep_orig := keep_orig | sum(found) > 1, by = order]
		DATA[, anymatch := any(found), by = order]
		# anymatch means at least one match has been found,
		# or a decision has been made to keep the original term
		DATA[keep_orig == TRUE, anymatch := TRUE]
		
		# Expand ancestors for terms without a match
		if (any(DATA$anymatch == FALSE)){
			EXPANDED <- DATA[anymatch == FALSE][,
				list(conceptId = parents(conceptId, SNOMED = SNOMED,
				tables = tables)),
				by = list(originalId, found, anymatch, keep_orig, order)]
			DATA <- rbind(DATA, EXPANDED)
		}
		recursionlimit <- recursionlimit - 1
	}
	
	# Keep original if no matches
	DATA[, keep_orig := keep_orig | (anymatch == FALSE), by = order]
	# If keeping the original concept, keep only the first row
	DATA[keep_orig == TRUE, found := c(TRUE, rep(FALSE, .N - 1)), by = order]
	DATA <- DATA[found == TRUE]
	setkey(DATA, order)
	# Now there should be one row per order
	stopifnot(DATA$order == seq_along(conceptIds))
	data.table::setnames(DATA, 'conceptId', 'ancestorId')
	DATA[keep_orig == TRUE, ancestorId := originalId]
	DATA[, order := NULL]
	DATA[, keep_orig := NULL]
	DATA[, found := NULL]
	DATA[, anymatch := NULL]
	return(DATA)
}


 
