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
#'   If it is a SNOMEDcodelist it is expanded, and any existing columns
#'   named 'read2_code' or 'read2_term' (if adding Read 2 maps) or
#'   'ctv3_concept' or ctv3_termid' (if adding CTV3 maps) will be
#'   overwritten.
#' @param mappingtable data.table containing mapping in the format
#'   described in 'Details'. The MAPS dataset in this package provides
#'   a sample.
#'   It must contain a unique field 'conceptId', and fields named
#'   'read2_code' and 'read2_term' (for mapping to Read 2)
#'   or 'ctv3_concept' and 'ctv3_termid' (for mapping to CTV3).
#' @param to character vector stating which terminologies to map to,
#'   either 'read2' or 'ctv3'; include both if both maps are required.
#'   Beware that including both maps may result in a significant expansion
#'   of the number of rows if single_row_per_concept is FALSE.
#' @param single_row_per_concept (logical) if TRUE (default), the function
#'   returns a single row per concept with Read 2 and CTV3 maps returned
#'   as lists (i.e. multiple entries within a single cell). This means
#'   the output is a valid SNOMEDcodelist object. If FALSE, returns
#'   multiple rows per concept (one for each map).
#' @return a data.table containing the columns conceptId and either
#'   'read2_code' and 'read2_term' (for mapping to Read 2)
#'   or 'ctv3_concept' and 'ctv3_termid' (for mapping to CTV3).
#'   There may be multiple rows per conceptId; also each Read 2 or CTV3 
#'   term may be mapped to multiple SNOMED CT concepts.
#' @export
#' @seealso MAPS, loadMAPS
#' @examples
#' # Load sample SNOMED CT dictionary into the global environment
#' # so it is available to the functions in this example
#' SNOMED <- sampleSNOMED()
#' # Use the sample MAPS table in this package
#' data(MAPS)
#' 
#' # Example: Mapping a single concept
#' getMaps(SNOMEDconcept('Heart failure'), mappingtable = MAPS,
#'   to = 'read2')
#' # Example: Mapping a concept and its descendants
#' getMaps(descendants(SNOMEDconcept('Heart failure')),
#'   mappingtable = MAPS, to = 'read2')
#' # Example: Mapping a codelist
#' getMaps(SNOMEDcodelist(SNOMEDconcept('Heart failure')),
#'   mappingtable = MAPS, to = 'ctv3')
getMaps <- function(x, mappingtable = NULL, to = c('read2', 'ctv3',
	'icd10', 'opcs4'), SNOMED = getSNOMED(), single_row_per_concept = TRUE){
	# OPCS4 and ICD10 maps included in UK SNOMED CT release
	# Read V2 and CTV3 included in separate mapping table (data migration)
	read2_code <- read2_term <- NULL
	ctv3_concept <- ctv3_termid <- NULL
	icd10_code <- NULL
	opcs4_code <- NULL
	
	if (any(to %in% c('read2', 'ctv3'))){
		if (is.null(mappingtable)){
			stop('mappingtable required for mapping to read2 or ctv3')
		}
	} 
	if (!(all(to %in% c('read2', 'ctv3', 'icd10', 'opcs4')))){
		stop('each element of "to" must be either "read2", "ctv3", "icd10" or "opcs4"')
	}
	# Returns the original concepts and the linked concepts as a
	# data.table
	if (is.SNOMEDconcept(x)){
		out <- SNOMEDcodelist(x, include_desc = FALSE, SNOMED = SNOMED)
	} else if (is.SNOMEDcodelist(x)){
		out <- copy(expandSNOMED(x, SNOMED = SNOMED))
	} else {
		stop('x must be a SNOMEDcodelist or SNOMEDconcept')
	}
	# Extract the relevant codeset
	if ('read2' %in% to){
		if ('read2_code' %in% names(out)) out[, read2_code := NULL]
		if ('read2_term' %in% names(out)) out[, read2_term := NULL]
		if (single_row_per_concept){
			out <- as.SNOMEDcodelist(merge(mappingtable[,
				list(read2_code = read2_code,
				read2_term = read2_term), by = conceptId], out,
				on = 'conceptId'))
		} else {
			out <- merge(mappingtable[,
				list(read2_code = unlist(read2_code),
				read2_term = unlist(read2_term)), by = conceptId], out,
				on = 'conceptId')
		}
	}
	if ('ctv3' %in% to){
		if ('ctv3_concept' %in% names(out)) out[, ctv3_concept := NULL]
		if ('ctv3_termid' %in% names(out)) out[, ctv3_termid := NULL]
		if (single_row_per_concept){
			out <- as.SNOMEDcodelist(merge(mappingtable[,
				list(ctv3_concept = ctv3_concept,
				ctv3_termid = ctv3_termid), by = conceptId], out,
				on = 'conceptId'))
		} else {
			out <- merge(mappingtable[,
				list(ctv3_concept = unlist(ctv3_concept),
				ctv3_termid = unlist(ctv3_termid)), by = conceptId], out,
				on = 'conceptId')
		}
	}
	if ('icd10' %in% to){
		if ('icd10_code' %in% names(out)) out[, icd10_code := NULL]
		TEMP <- merge(SNOMED$EXTENDEDMAP[mapPriority == 1 & refsetId %in%
			bit64::as.integer64(c('447562003', '999002271000000101')),
			list(icd10_code = mapTarget, conceptId =
			referencedComponentId)], out, by = 'conceptId')
			
			list(icd10_code = unique(sub('\\.', '', mapTarget))),
			by = list(conceptId = referencedComponentId)]
		if (single_row_per_concept){
			out <- as.SNOMEDcodelist(merge(TEMP, out, by = 'conceptId'))
		} else {
			out <- merge(TEMP[,
				list(icd10_code = unlist(icd10_code)),
				by = conceptId], out, on = 'conceptId')
		}
	}
	if ('opcs4' %in% to){
		if ('opcs4_code' %in% names(out)) out[, opcs4_code := NULL]
		TEMP <- SNOMED$EXTENDEDMAP[, opcs4_code = ]
		if (single_row_per_concept){
			out <- as.SNOMEDcodelist(merge(TEMP[,
				list(opcs4_code = opcs4_code),
				by = conceptId], out, on = 'conceptId'))
		} else {
			out <- merge(mappingtable[,
				list(opcs4_code = unlist(opcs4_code)),
				by = conceptId], out, on = 'conceptId')
		}
	}
	out
}

