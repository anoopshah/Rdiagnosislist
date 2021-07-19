#' Obtain mapped Read V2 and CTV3 concepts for SNOMED CT concepts
#'
#' Returns concepts in Read Clinical Terms Version 2 and 
#' Clinical Terms Version 3 that map to a set of SNOMED CT
#' concepts, according to a supplied mapping file.
#'
#' We recommend this function is used with a mapping file
#' containing 'forward' mappings from V2 and CTV3 codes to
#' SNOMED CT. These are intended for converting individual
#' entries in electronic health records to SNOMED CT. The maps
#' can be used in the reverse direction to 
#' derived from the NHS Digital 'Data Migration' pack. The
#' mapping file must be a data.table object with columns:
#' conceptId (integer64, unique),
#' read2_code (character list of 7-character Read V2 codes),
#' read2_term (character list of Read V2 terms),
#' ctv3_concept (character list of CTV3 concept codes),
#' ctv3_termid (character list of CTV3 term description codes)
#'
#' @param x SNOMEDcodelist or SNOMEDconcept object
#' @param mappingtable data.table containing mapping file, which must 
#'   contain a unique field 'conceptId' and fields named 'read2_code'
#'   and 'read2_term' (for mapping to Read 2) or 'ctv3_concept'
#'   and 'ctv3_termid' (for mapping to Clinical Terms Version 3).
#' @return a data.table containing the columns conceptId and . There may be multiple 
#'   rows per conceptId
#' @export
#' @seealso MAPS
#' @examples
#' # Load sample SNOMED CT dictionary
#' SNOMED <- sampleSNOMED()
#' data(MAPS)
#' 
#' # Example: Mapping a single concept
#' getMaps(SNOMEDconcept('Heart failure'), mappingtable = MAPS, to = 'read')
#' # Example: Mapping a concept and its descendants
#' getMaps(descendants(SNOMEDconcept('Heart failure')), mappingtable = MAPS, to = 'read')
#' # Example: Mapping a codelist
#' getMaps()
getMaps <- function(x, mappingtable, to = c('read2', 'ctv3'),
	SNOMED = getSNOMED()){
	# Returns the original concepts and the linked concepts as a
	# data.table
	MAPS <- mapping
	if (is.SNOMEDconcept(x)){
		MAPPED <- mappingtable[data.table(conceptId = x), on = 'conceptId']
		MAPPED
	} else if (is.SNOMEDcodelist(x)){
		MAPPED <- merge(mappingtable[x, on = 'conceptId'])
	}
}

