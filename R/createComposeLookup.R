#' Creates a set of lookups for SNOMED composition
#'
#' Creates composition lookup table for a set of SNOMED CT concepts

#' @param conceptIds for which to create lookup table
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
createComposeLookup <- function(conceptIds, CDB, SNOMED = getSNOMED(),
	...){
	conceptIds <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
	D <- rbindlist(lapply(as.character(conceptIds), function(the_concept){
	texts <- description(the_concept, include_synonyms = TRUE,
		SNOMED = SNOMED)[type == 'Synonym']$term
	rbindlist(lapply(texts, function(the_text){
		decompose(as.SNOMEDconcept(the_concept, SNOMED = SNOMED),
			diagnosis_text = the_text, CDB = CDB, SNOMED = SNOMED, ...)
		}))
	}))
	# D is the decompose table created by concatenating rows of the 
	# output of decompose
	# Remove rows with outstanding text
	D <- copy(as.data.table(D)[!(other_conceptId %like% '[[:alpha:]]')])
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
