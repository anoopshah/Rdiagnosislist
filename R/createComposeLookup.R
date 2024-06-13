#' Creates an environment containing CDB files
#'
#' Extracts SNOMED CT concepts from appropriate places in the 
#' hierarchy to create a set of CDB files in an environment.
#' Uses WordNet and manual synonyms if available.
#'
#' @param SNOMED environment containing a SNOMED dictionary
#' @param WN WordNet data.table as returned by downloadWordnet
#'   containing WordNet data from appropriate
#'   categories, in the format: cat (character), wordnetId (integer64),
#'   synonyms (list), parents (list), adj (list)
#' @param MANUAL_SYNONYMS 
#' @return environment containing the following data tables: FINDINGS,
#'   QUAL, CAUSES, BODY, FINDINGS, OTHERSUB, OVERLAP
#' @export
#' @seealso [addWordnet()]
#' @references \url{https://wordnet.princeton.edu/}
#' @examples
#' # Not run
#' # WORDNET <- downloadWordnet()
createComposeLookup <- function(D){
	# D is the decompose table created by concatenating rows of the 
	# output of decompose
	# Remove rows with outstanding text
	D <- copy(D[!(other_conceptId %like% '[[:alpha:]]')])
	D[, other_conceptId := gsub('^ +| +$', '', other_conceptId)]
	
	# Separate due to findings; due to anything else is other_attr
	D[!(due_to %in% FINDINGS$conceptId) & !is.na(due_to),
		other_conceptId := paste(due_to, other_conceptId)]
	D[!(due_to %in% FINDINGS$conceptId) & !is.na(due_to),
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
