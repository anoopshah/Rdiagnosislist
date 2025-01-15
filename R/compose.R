#' Select more specific SNOMED CT concepts based on attributes
#'
#' Finds the most specific SNOMED CT concepts that matches the
#' combination of a root concept and attributes. Based on a
#' composeLookup table
#'
#' @param conceptId SNOMED CT concept to refine
#' @param CDB SNOMED CT concept database, as created by createCDB.
#'   An environment containing the following data tables: FINDINGS,
#'   QUAL, CAUSES, BODY, FINDINGS, OTHERSUB, OVERLAP, TRANSITIVE.
#'   Must also contain a COMPOSELOOKUP table created by
#'   addComposeLookupToCDB
#' @param attributes_conceptIds SNOMED concept Ids of attributes of concept
#'   e.g. laterality, severity, acuteness
#' @param due_to_conceptIds SNOMED concept Ids of cause
#' @param without_conceptIds SNOMED concept Ids of conditions stated to be
#'   absent
#' @param with_conceptIds SNOMED concept Ids of conditions also present
#' @param SNOMED environment containing SNOMED CT tables
#' @return a refined SNOMED concept Id
#' @seealso decompose, batchDecompose, addComposeLookupToCDB
#' @examples
#' \dontrun{
#' SNOMED <- sampleSNOMED()
#' miniCDB <- createCDB(SNOMED)
#' 
#' D <- decompose('Cor pulmonale', CDB = miniCDB)
#' print(D)
#' 
#' # --------------------------------------------------------
#' # 83291003 | Cor pulmonale (disorder)
#' # --------------------------------------------------------
#' # Root : 367363000 | Right ventricular failure (disorder)
#' # - Due to : 19829001 | Disorder of lung (disorder)
#'
#' # Compile decompositions into a lookup table
#' miniCDB <- addComposeLookupToCDB(D, CDB = miniCDB))
#' 
#' compose(as.SNOMEDconcept('Right heart failure'),
#'   due_to_conceptIds = as.SNOMEDconcept('Disorder of lung'),
#'   CDB = miniCDB, composeLookup = CL)
#' # [1] "83291003 | Cor pulmonale (disorder)"
#' }
#' @export
compose <- function(conceptId, CDB,
	attributes_conceptIds = bit64::integer64(0),
	due_to_conceptIds = bit64::integer64(0),
	without_conceptIds = bit64::integer64(0),
	with_conceptIds = bit64::integer64(0),
	SNOMED = getSNOMED()){
	
	if (is.null(CDB$COMPOSELOOKUP)){
		stop(paste0('CDB requires a composition lookup table to be\n',
			'added using addComposeLookupToCDB.'))
	}

	NA_concept <- bit64::as.integer64(NA)
	setattr(NA_concept, 'class', c('SNOMEDconcept', 'integer64'))
	
	# Declare symbols to avoid R check error
	findingId <- otherId <- rootId <- without <- NULL
	due_to <- attr_1 <- multipart <- origId <- NULL
	
	# Harmonise and append NA for each attribute
	expand <- function(x){
		if (length(x) == 0){
			return(bit64::integer64(0))
		} else {
			x <- as.SNOMEDconcept(x, SNOMED = SNOMED)
		}
		x <- add_overlap(x)
		out <- ancestors(x, SNOMED = SNOMED, TRANSITIVE = CDB$TRANSITIVE)
		# For body site concepts, remove ancestor concepts with 'and'
		# or 'or'
		return(union(x, setdiff(out, CDB$BODY[
			multipart == TRUE]$conceptId)))
	}
	
	add_overlap <- function(x){
		unique(c(x,
			CDB$OVERLAP[findingId %in% x]$otherId,
			CDB$OVERLAP[otherId %in% x]$findingId))
	}
	
	harmonise <- function(x, limitToFindings = FALSE){
		if (is.null(x)) x <- NA_concept
		if (length(x) == 0) x <- NA_concept
		if (limitToFindings){
			union(intersect(expand(
				as.SNOMEDconcept(bit64::as.integer64(x), SNOMED = SNOMED)
				), CDB$FINDINGS$conceptId), NA_concept)
		} else {
			union(expand(as.SNOMEDconcept(bit64::as.integer64(x),
				SNOMED = SNOMED)), NA_concept)
		}
	}
	
	# Ensure correct data types
	conceptId <- as.SNOMEDconcept(conceptId, SNOMED = SNOMED)
	
	# Expand to include ancestors
	due_to_search <- harmonise(due_to_conceptIds, TRUE)
	attributes_search <- union(harmonise(attributes_conceptIds),
		due_to_search)
	without_search <- harmonise(without_conceptIds, TRUE)
	with_search <- harmonise(with_conceptIds, TRUE)

	# Expand root concept to include ancestors that are in
	# composeLookup (in case they provide a wider variety of
	# composition options)
	root_search <- add_overlap(conceptId)
	root_search <- union(root_search,
		CDB$COMPOSELOOKUP[origId %in% root_search]$rootId)
	
	# Create indexed data.table for fast lookup
	LOOKUP <- data.table(expand.grid(root_search, attributes_search))
	setnames(LOOKUP, c('rootId', 'attr_1'))
	setkeyv(LOOKUP, c('rootId', 'attr_1'))
	
	# Find highest number of attribute fields in this composeLookup
	max_attr <- max(as.numeric(sub('^attr_', '', 
		names(CDB$COMPOSELOOKUP)[
		names(CDB$COMPOSELOOKUP) %like% '^attr_'])))
	
	if (max_attr == 1){
		SUBSET <- merge(CDB$COMPOSELOOKUP, LOOKUP)[
			without %in% without_search &
			with %in% with_search & due_to %in% due_to_search]
	} else {
		# This code could be simplified if number of attribute fields
		# is fixed to 10
		attr_text <- paste(paste0('attr_', 2:max_attr,
				' %in% attributes_search &'), collapse = ' ')
		SUBSET <- merge(CDB$COMPOSELOOKUP, LOOKUP)[eval(parse(text =
			paste(attr_text, 'without %in% without_search &', 
				'with %in% with_search & due_to %in% due_to_search')))]
	}
	
	if (nrow(SUBSET) == 0){
		return(conceptId)
	}

	# Retrieve valid compositions
	matchIds <- unique(as.SNOMEDconcept(SUBSET$origId,
		SNOMED = SNOMED))

	# Return original concept if no compositions
	if (length(matchIds) == 0){
		return(conceptId)
	}
	
	# Remove all matches which are an ancestor of another match
	i <- 1
	while (i <= length(matchIds) & length(matchIds) > 1){
		ancIds <- ancestors(matchIds[i], SNOMED = SNOMED,
			TRANSITIVE = CDB$TRANSITIVE, include_self = FALSE)
		if (length(intersect(matchIds, ancIds)) > 0){
			matchIds <- setdiff(matchIds, ancIds)
			i <- 1
		} else {
			i <- i + 1
		}
	}
	
	# Return the matches
	matchIds
}


