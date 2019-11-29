
# Generate a function to retrieve descendants of a particular SNOMED code
getRelatedConcepts <- function(conceptIds, typeId, SNOMED,
	tables = c('Relationship', 'StatedRelationship'),
	reverse = FALSE, recursive = FALSE, active_only = TRUE){
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
				.(active, conceptId = sourceId)])
		} else {
			setkey(TABLE, sourceId, typeId)
			OUT <- rbind(OUT, TABLE[TOLINK][,
				.(active, conceptId = destinationId)])
		}
		OUT
	}

	if ('Relationship' %in% tables){
		OUT <- addRelationship('RELATIONSHIP', OUT)
	}
	if ('StatedRelationship' %in% tables){
		OUT <- addRelationship('STATEDRELATIONSHIP', OUT)
	}
	if (active_only){
		out <- OUT[active == TRUE]$conceptId
	} else {
		out <- OUT$conceptId
	}
	if (recursive == TRUE){
		out <- sort(unique(c(conceptIds, out)))
		if (length(conceptIds) < length(out)){
			# Recurse
			return(getRelatedConcepts(conceptIds = out,
				typeId = typeId, SNOMED = SNOMED, tables = tables,
				reverse = reverse, recursive = TRUE,
				active_only = active_only))
		} else {
			return(out)
		}
	} else {
		return(out)
	}
}

parents <- function(conceptIds, SNOMED, ...){
	conceptIds <- checkConcepts(unique(conceptIds))
	parentIds <- getRelatedConcepts(conceptIds = conceptIds,
		typeId = as.integer64('116680003'),
		reverse = FALSE, recursive = TRUE, SNOMED = SNOMED, ...)
	# Exclude originals (note cannot use setdiff function with int64)
	parentIds[!(parentIds %in% conceptIds)]
}

children <- function(conceptIds, SNOMED, ...){
	conceptIds <- checkConcepts(unique(conceptIds))
	childIds <- getRelatedConcepts(conceptIds = conceptIds,
		typeId = as.integer64('116680003'),
		reverse = TRUE, recursive = TRUE, SNOMED = SNOMED, ...)
	# Exclude originals (note cannot use setdiff function with int64)
	childIds[!(childIds %in% conceptIds)]
}

has_attribute <- function(conceptIds, refId, SNOMED,
	typeId = as.integer64('116680003'), ...){
	# IN PROGRESS
	# For each concept in the first list, whether it has the parent
	# in the second list
	# Returns a vector of Booleans
	TEMP <- data.table(conceptIds)
	RELATIONSHIP
	STATEDRELATIONSHIP
}

get_attributes <- function(conceptIds, attributes, ...){
	# Retrieves a table of attributes for a given set of concepts
	attribute_ids <- attributeId(attributes)
	rbindlist(lapply(attribute_ids, function(x){
		getRelatedConcepts(conceptIds, typeId = x, ...)	
	}))
}

attributeId <- function(attributes){
	# Converts an attribute as a character string to a conceptId for
	# the attribute. However, if attribute is already a conceptId
	# it is left as it is
	attribute_ids <- checkConcepts(attributes)
	if (any(is.na(attribute_ids))){
		tomatch <- is.na(attribute_ids)
		TOMATCH <- data.table(term = tolower(as.character(attributes[tomatch])))
		attribute_ids[tomatch] <- TOMATCH[ATTRIBUTES, on = 'term']$conceptId
	}
	attribute_ids
}