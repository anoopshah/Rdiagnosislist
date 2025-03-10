#' Creates a set of lookups for SNOMED composition
#'
#' Creates composition lookup table for a set of SNOMED CT concepts
#' and exports the results to a file

#' @param conceptIds SNOMED CT concept IDs for creating decompositions
#' @param CDB concept database environment, containing a table called
#'   FINDINGS 
#' @param SNOMED environment containing a SNOMED dictionary
#' @param output_filename filename of output file 
#' @param ... other arguments to pass through to decompose
#' @return TRUE if successful 
#' @family CDB functions
#' @export
#' @seealso decompose, compose, addComposeLookupToCDB
batchDecompose <- function(conceptIds, CDB, output_filename,
	SNOMED = getSNOMED(), ...){
		
	# Decare symbols for R check
	origId <- NULL
	
	conceptIds <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
	started <- FALSE
	for (i in seq_along(conceptIds)){
		D <- NULL
		try(D <- decompose(conceptIds[i], CDB = CDB, SNOMED = SNOMED, ...))
		if (is.null(D)){
			message('Error in analysing concept ', conceptIds[i], '(',
				description(conceptIds[i])$term[1], ')')
		} else if (nrow(D) == 0){
			D <- D[1]
			D[, origId := conceptIds[i]]
			fwrite(D[1], output_filename, append = started)
			started <- TRUE
		} else {
			fwrite(D, output_filename, append = started)
			started <- TRUE
		}
	}
	return(started)
}

#' Add composition lookups to CDB
#'
#' Creates a composition lookup table for a set of SNOMED CT concepts
#' based on output of `decompose', and adds it to the CDB
#' 
#' @param decompositions vector of filenames of decompose output (read
#'   by fread) or data.frame containing outputs of decompose function
#' @param CDB concept database environment, containing a table called
#'   FINDINGS
#' @param maxcol maximum number of attributes columns. If NULL it is
#'   determined from the data. It might be helpful to specify it so that
#'   downstream databases and programs know exactly how many columns to
#'   expect. We suggest setting it to 10 which should handle all 
#'   possible SNOMED CT concept decompositions.
#' @param SNOMED environment containing a SNOMED CT dictionary
#' @param ... other arguments to pass to fread
#' @return CDB environment with an additional data.table `COMPOSELOOKUP'
#'   with columns rootId, attr_1 ... attr_X (up to
#'   maxcol), with, due_to, without, origId (all with data type
#'   integer64 and class 'SNOMEDconcept')
#' @export
#' @family CDB functions
#' @seealso decompose, compose, batchDecompose
addComposeLookupToCDB <- function(decompositions, CDB, maxcol = 10,
	SNOMED = getSNOMED(), ...){
		
	# Declare symbols to avoid R check error
	.temp <- rootId <- other_conceptId <- due_to <- after <- NULL
	body_site <- severity <- stage <- laterality <- NULL
	attrId <- freq <- rootId <- freq <- .temp2 <- without <- NULL
	
	sct_concept_colnames <- c('rootId', 'with', 'due_to',
		'after', 'without', 'body_site', 'severity', 'stage',
		'laterality', 'origId')
	if (is.character(decompositions)){
		D <- rbindlist(lapply(decompositions, function(x){
			fread(x, colClasses = list(character =
				c(sct_concept_colnames, 'other_conceptId')), ...)
		}), fill = TRUE)
	} else {
		D <- copy(as.data.table(decompositions))
	}
	
	D <- D[!duplicated(D)]
	
	for (i in sct_concept_colnames){
		D[, .temp := as.SNOMEDconcept(bit64::as.integer64(D[[i]]),
			SNOMED = SNOMED)]
		D[.temp == 0, .temp := NA]
		# explicit conversion using bit64::as.integer64 is to ensure
		# that missing values and '' are handled correctly
		D[, (i) := NULL]
		setnames(D, '.temp', i)
	}
	
	# Remove rows with outstanding text
	D <- D[!(other_conceptId %like% '[[:alpha:]]')]
	D[, other_conceptId := gsub('^ +| +$', '', other_conceptId)]

	# Remove rows without rootId or without a valid decomposition
	D <- D[!is.na(rootId)]
	D <- D[!(is.na(with) & is.na(due_to) & is.na(after) &
		is.na(without) & is.na(body_site) & is.na(severity) &
		is.na(stage) & is.na(laterality) & other_conceptId == '')]

	# Separate due to findings; due to anything else is other_attr
	D[!(due_to %in% CDB$FINDINGS$conceptId) & !is.na(due_to),
		other_conceptId := paste(due_to, other_conceptId)]
	D[!(due_to %in% CDB$FINDINGS$conceptId) & !is.na(due_to),
		due_to := NA_integer64_]
	
	# Combine 'due_to' and 'after' because very few SNOMED CT concepts
	# have both with different concepts in each. If due_to and after
	# are different concepts, do not use this decomposition as it is
	# too complex for this algorithm
	D <- D[is.na(due_to) | is.na(after) | after == due_to]
	D[is.na(due_to) & !is.na(after), due_to := after]
	
	# Add body site, severity, stage to other_conceptId
	D[!is.na(body_site), other_conceptId := paste(body_site, 
		other_conceptId)]
	D[!is.na(severity), other_conceptId := paste(severity,
		other_conceptId)]
	D[!is.na(stage), other_conceptId := paste(stage, other_conceptId)]
	D[!is.na(laterality), other_conceptId := paste(laterality,
		other_conceptId)]
	
	# Prepend root conceptId so that it is accessible when sorting
	# attributes by frequency 
	D[, other_conceptId := paste(rootId, other_conceptId)]
	D[, other_conceptId := strsplit(other_conceptId, ' ')]
	
	# Create a frequency table of other_attr per rootId
	FREQ <- D[, list(.temp = unlist(other_conceptId)), by = rootId]
	FREQ[, attrId := bit64::as.integer64(.temp)]
	setattr(FREQ$attrId, 'class', c('SNOMEDconcept', 'integer64'))
	FREQ <- FREQ[, list(freq = .N), by = list(attrId, rootId)][
		order(rootId, freq)]
	FREQ <- FREQ[!attrId == rootId]
	
	# Sort by ascending order of frequency
	D[, attrId := lapply(other_conceptId, function(x){
			if (length(x) <= 1){
				as.SNOMEDconcept(bit64::integer64(0), SNOMED = SNOMED)
			} else {
				x <- unique(as.SNOMEDconcept(x, SNOMED = SNOMED))
				FREQ[rootId == x[1]][(data.table(attrId = x[2:length(x)])),
					on = 'attrId'][order(freq)]$attrId
			}
		})]
	
	# Split into separate columns for fast matching
	if (is.null(maxcol)){
		maxcol <- max(sapply(D$attrId, length))
	}
	
	# Find which decompositions include body site
	BODY <- data.table(.temp2 = unique(CDB$BODY$conceptId),
		body = TRUE)
	setkeyv(BODY, '.temp2')
	D[, incl_bodysite := FALSE]

	for (i in 1:maxcol){
		# extract attribute Ids via as.character to avoid incorrect
		# conversion to numeric
		D[, .temp := sapply(attrId, function(x){
			ifelse(i > length(x), NA_character_, as.character(x[i]))
		})]
		D[, .temp2 := as.SNOMEDconcept(bit64::as.integer64(.temp),
			SNOMED = SNOMED)]
		D[, .temp := NULL]
		D[, incl_bodysite := incl_bodysite | ifelse(
			is.na(BODY[D, on = '.temp2']$body), FALSE,
			BODY[D, on = '.temp2']$body)]
		setnames(D, '.temp2', paste0('attr_', i))
	}
		
	cols_to_keep <- c('rootId', paste0('attr_', 1:maxcol), 
		'with', 'due_to', 'without', 'origId', 'incl_bodysite')
	D <- subset(D, select = cols_to_keep)
	D <- D[!duplicated(D)]
	setorderv(D, cols_to_keep)
	setkeyv(D, cols_to_keep)
	for (i in cols_to_keep) setindexv(D, i)
	CDB$COMPOSELOOKUP <- D
	CDB
}
