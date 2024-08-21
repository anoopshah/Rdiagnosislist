#' Creates a set of lookups for SNOMED composition
#'
#' Creates composition lookup table for a set of SNOMED CT concepts

#' @param conceptIds SNOMED CT concept IDs for creating decompositions
#' @param CDB concept database environment, containing a table called
#'   FINDINGS 
#' @param SNOMED environment containing a SNOMED dictionary
#' @param output_filename filename of output file 
#' @return TRUE if successful 
#' @export
#' @seealso [decompose()]
#' @examples
#' # Not run
#'
#' mylookup <- createComposeLookup(D)
batchDecompose <- function(conceptIds, CDB, output_filename,
	SNOMED = getSNOMED(), ...){
	conceptIds <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
	started <- FALSE
	for (i in seq_along(conceptIds)){
		texts <- description(conceptIds[i], include_synonyms = TRUE,
			SNOMED = SNOMED)[type == 'Synonym']$term
		D <- NULL
		try(D <- rbindlist(lapply(texts, function(the_text){
			decompose(conceptIds[i], diagnosis_text = the_text,
				CDB = CDB, SNOMED = SNOMED, ...)
		})))
		if (is.null(D)){
			message('Error in analysing concept ', conceptIds[i],
				' (', texts[1], ')')
		} else {
			fwrite(D, output_filename, append = started)
			started <- TRUE
		}
	}
	return(started)
}

#' Creates a set of lookups for SNOMED composition
#'
#' Creates composition lookup table for a set of SNOMED CT concepts

#' @param decompositions filename of batchDecompose output or
#'   data.frame containing decomposition outputs
#' @param CDB concept database environment, containing a table called
#'   FINDINGS 
#' @param SNOMED environment containing a SNOMED dictionary
#' @return data.table 
#' @export
#' @seealso [decompose()]
#' @examples
#' # Not run
#'
#' mylookup <- createComposeLookup(D)
createComposeLookup <- function(decompositions, CDB){
	if (is.character(decompositions)){
		D <- fread(decompositions)
	} else {
		D <- copy(as.data.table(D))
	}
	
	# D is the decompose table created by concatenating rows of the 
	# output of decompose
	# Remove rows with outstanding text
	D <- D[!(other_conceptId %like% '[[:alpha:]]')]
	D[, other_conceptId := gsub('^ +| +$', '', other_conceptId)]
	
	# Separate due to findings; due to anything else is other_attr
	D[!(due_to %in% CDB$FINDINGS$conceptId) & !is.na(due_to),
		other_conceptId := paste(due_to, other_conceptId)]
	D[!(due_to %in% CDB$FINDINGS$conceptId) & !is.na(due_to),
		due_to := NA_integer64_]
	
	# Add body site, severity, stage to other_conceptId
	D[!is.na(body_site), other_conceptId := paste(body_site, other_conceptId)]
	D[!is.na(severity), other_conceptId := paste(severity, other_conceptId)]
	D[!is.na(stage), other_conceptId := paste(stage, other_conceptId)]
	D[!is.na(laterality), other_conceptId := paste(laterality, other_conceptId)]
	D[, other_conceptId := paste(rootId, other_conceptId)]
	D[, other_conceptId := strsplit(other_conceptId, ' ')]
	
	# Frequency of other_attr per rootId
	FREQ <- D[, .(attrId = unlist(other_conceptId)), by = rootId]
	FREQ <- FREQ[, .(freq = .N), by = .(attrId, rootId)][order(rootId, freq)]
	
	# Sort by ascending order of frequency
	D[, attrId := lapply(other_conceptId, function(x){
			FREQ[rootId == x[1]][(data.table(attrId = x[2:length(x)])),
				on = 'attrId'][order(freq)]$attrId
		})]
	
	# Split into separate columns for fast matching
	maxcol <- max(sapply(D$attrId, length))
	for (i in 1:maxcol){
		D[, .temp := sapply(attrId, function(x) x[i])]
		setnames(D, '.temp', paste0('attr_', i))
	}
	colnames <- c('rootId', 'with', 'due_to', 'after',
		'without', paste0('attr_', 1:maxcol), 'origId')
	D <- D[, ..colnames]
	setorderv(D, colnames)
	D
}
