
# Generate a function to retrieve descendants of a particular SNOMED code
getRelatedConcepts <- function(conceptIds, typeIds,
	tables = c('Relationship', 'StatedRelationship'),
	reverse = FALSE, recursive = FALSE, envir = .GlobalEnv,
	activeOnly = TRUE){
	# Returns the original concepts and the linked concepts
	if (activeOnly){
		activeOK <- 1
	} else {
		activeOK <- c(0, 1)
	}
	
	conceptIds <- sort(unique(as.integer64(conceptIds)))
	typeIds <- as.integer64(typeIds)
	TOLINK <- data.table(sourceId = conceptIds)
	setkey(TOLINK, sourceId)
	out <- bit64::integer64(0)
	# Retrieve relationship table
	addRelationship <- function(tablename, out){
		TABLE <- get(tablename, envir = envir)
		if (reverse){
			setkey(TABLE, destinationId)
			out <- sort(unique(c(out,
				TABLE[TOLINK][typeId %in% typeIds &
				active %in% activeOK]$sourceId)))
		} else {
			setkey(TABLE, sourceId)
			out <- sort(unique(c(out,
				TABLE[TOLINK][typeId %in% typeIds &
				active %in% activeOK]$destinationId)))
		}
		out
	}

	if ('Relationship' %in% tables){
		out <- addRelationship('RELATIONSHIP', out)
	}
	if ('StatedRelationship' %in% tables){
		out <- addRelationship('STATEDRELATIONSHIP', out)
	}
	if (recursive == TRUE){
		out <- sort(unique(c(conceptIds, out)))
		if (length(conceptIds) < length(out)){
			# Recurse
			return(getRelatedConcepts(conceptIds = 
				sort(unique(c(conceptIds, out))),
				typeIds = typeIds, tables = tables,
				reverse = reverse, recursive = TRUE, envir = envir))
		} else {
			return(out)
		}
	} else {
		return(out)
	}
}

parents <- function(conceptIds, ...){
	conceptIds <- as.integer64(conceptIds)
	parentIds <- getRelatedConcepts(conceptIds = conceptIds,
		typeIds = as.integer64('116680003'),
		reverse = FALSE, recursive = TRUE, ...)
	# Exclude originals (note cannot use setdiff function with int64)
	parentIds[!(parentIds %in% parentIds)]
}

children <- function(conceptIds, ...){
	conceptIds <- as.integer64(conceptIds)
	childIds <- getRelatedConcepts(conceptIds = conceptIds,
		typeIds = as.integer64('116680003'),
		reverse = TRUE, recursive = TRUE, ...)
	# Exclude originals (note cannot use setdiff function with int64)
	childIds[!(childIds %in% conceptIds)]
}
