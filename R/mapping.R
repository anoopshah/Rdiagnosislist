#' Obtain mapped Read 2 and CTV3 concepts for SNOMED CT concepts
#'
#' Returns concepts in Read Clinical Terms Version 2 (Read 2) and 
#' Clinical Terms Version 3 (CTV3) that map to a set of SNOMED CT
#' concepts, according to a supplied mapping file.
#'
#' We recommend this function is used with a mapping file
#' containing 'forward' mappings from Read 2 and CTV3 codes to
#' SNOMED CT, as included in the NHS Digital 'Data Migration' pack.
#' These are intended for converting individual
#' entries in electronic health records to SNOMED CT. The 
#' 'forward' map files contain a SNOMED CT map for every Read 2 or
#' CTV3 code, but not all the SNOMED CT concepts are mapped.
#' Future SNOMED CT concepts will also not be mapped.
#'
#' These maps can be used for converting SNOMED CT codelists into
#' Read 2 or CTV3 format for running queries, such as to characterise
#' patient phenotypes or identify patient populations for research.
#' They cannot be used in the reverse direction (to map a Read 2/CTV3
#' codelist to SNOMED CT) because some of the SNOMED CT terms will
#' be missed out, and the list will be incomplete.
#'
#' The mapping table must be a data.table object with columns:
#' conceptId (integer64, unique),
#' read2_code (character list of 7-character Read 2 codes),
#' read2_term (character list of Read 2 terms),
#' ctv3_concept (character list of CTV3 concept codes),
#' ctv3_termid (character list of CTV3 term description codes)
#'
#' @param x SNOMEDcodelist or SNOMEDconcept object. If it is a
#'   SNOMEDconcept object it is first converted to a SNOMEDcodelist.
#' @param mappingtable data.table containing mapping in the format above.
#'   It must contain a unique field 'conceptId' and fields named
#'   'read2_code' and 'read2_term' (for mapping to Read 2)
#'   or 'ctv3_concept' and 'ctv3_termid' (for mapping to CTV3).
#' @return a data.table containing the columns conceptId and either
#'   'read2_code' and 'read2_term' (for mapping to Read 2)
#'   or 'ctv3_concept' and 'ctv3_termid' (for mapping to CTV3).
#'   There may be multiple rows per conceptId; also each Read 2 or CTV3 
#'   term may be mapped to multiple SNOMED CT concepts.
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
#' getMaps(SNOMEDcodelist(SNOMEDconcept('Heart failure')),
#'   mappingtable = MAPS, to = 'ctv3')
getMaps <- function(x, mappingtable, to = c('read2', 'ctv3'),
	SNOMED = getSNOMED()){
	to <- to[1]
	if (!(to %in% c('read2', 'ctv3'))){
		stop('"to" must be either "read2" or "ctv3"')
	}
	# Returns the original concepts and the linked concepts as a
	# data.table
	if (is.SNOMEDconcept(x)){
		x <- SNOMEDcodelist(x, include_desc = FALSE)
	}
		MAPPED <- mappingtable[data.table(conceptId = x), on = 'conceptId']
		MAPPED
	} else (is.SNOMEDcodelist(x)){
		MAPPED <- merge(mappingtable[x, on = 'conceptId'])
	}
}

