#' Obtain Read 2, CTV3, ICD-10 and OPCS4 maps for SNOMED CT concepts
#'
#' Returns concepts mapped to SNOMED CT from either the
#' SIMPLEMAP table in the SNOMED dictionay (Clinical Terms Version 3,
#' CTV3 maps, one per concept),
#' the EXTENDEDMAP table (ICD-10 and OPCS4 maps) or a separate mapping
#' table with Read Clinical Terms Version 2 (Read 2) and 
#' CTV3 maps. A sample mapping table (READMAPS) is provided.
#'
#' The mapping table can be created from the NHS Digital 'Data
#' Migration' pack files which contain 'forward' maps of Read 2 and
#' CTV3 to SNOMED CT.
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
#'   If it is a SNOMEDcodelist it is first converted to 'simple' format.
#'   Columns named 'read2_code' or 'read2_term' (if adding Read 2 maps) or
#'   'ctv3_concept' or ctv3_termid' (if adding CTV3 maps) will be
#'   overwritten.
#' @param mappingtable data.table containing mapping in the format
#'   described in 'Details'. The MAPS dataset in this package provides
#'   a sample.
#'   It must contain a unique field 'conceptId', and fields named
#'   'read2_code' and 'read2_term' (for mapping to Read 2)
#'   or 'ctv3_concept' and 'ctv3_termid' (for mapping to CTV3).
#' @param SNOMED an environment containing the SNOMED CT dictionary.
#'   If not supplied, it will be obtained using getSNOMED().
#' @param to character vector stating which terminologies to map to.
#'   Options are 'icd10', 'opcs4', 'ctv3simple' (use tables included
#'   within the SNOMED dictionary), or 'read2' or 'ctv3' (require a
#'   separate mapping table such as READMAPS).
#'   Beware that including multiple destination terminologies may
#'   result in a significant expansion
#'   of the number of rows if single_row_per_concept is FALSE.
#' @param single_row_per_concept (logical) if TRUE (default), the function
#'   returns a single row per concept with Read 2 and CTV3 maps returned
#'   as lists (i.e. multiple entries within a single cell). This means
#'   the output is a valid SNOMEDcodelist object. If FALSE, returns
#'   multiple rows per concept (one for each map).
#' @return a data.table containing the columns conceptId and either
#'   'read2_code' and 'read2_term' (for mapping to Read 2),
#'   'ctv3_concept' and 'ctv3_termid' (for mapping to CTV3 using the
#'   mapping table), 'ctv3_simple' (mapping to CTV3 using SIMPLEMAP
#'   within the SNOMED dictionary), 'icd10_code' or 'opcs4_code'
#'   (mapped using EXTENDEDMAP within the SNOMED dictionary).
#'   If single_row_per_concept is TRUE, the mapped rows are of type
#'   'list' and the output is also a SNOMEDcodelist in 'simple' format,
#'   otherwise the output may have multiple rows per
#'   conceptId. Note that each Read 2, CTV3, ICD-10 or OPCS4 term may
#'   be mapped to multiple SNOMED CT concepts.
#' @export
#' @seealso READMAPS, loadREADMAPS
#' @examples
#' # Load sample SNOMED CT dictionary into the global environment
#' # so it is available to the functions in this example
#' SNOMED <- sampleSNOMED()
#' # Use the sample READMAPS table in this package
#' data(READMAPS)
#' 
#' # Example: Mapping a single concept
#' getMaps(SNOMEDconcept('Heart failure'), mappingtable = READMAPS,
#'   to = 'read2')
#' # Example: Mapping a concept and its descendants
#' getMaps(descendants(SNOMEDconcept('Heart failure')),
#'   mappingtable = READMAPS, to = 'read2')
#' # Example: Mapping a codelist
#' getMaps(SNOMEDcodelist(SNOMEDconcept('Heart failure')),
#'   mappingtable = READMAPS, to = c('ctv3', 'ctv3simple', 'icd10'))
getMaps <- function(x, mappingtable = NULL, to = c('read2', 'ctv3',
	'icd10', 'opcs4', 'ctv3simple'), SNOMED = getSNOMED(),
	single_row_per_concept = TRUE){
	# OPCS4 and ICD10 maps included in UK SNOMED CT release
	# Read V2 and CTV3 included in separate mapping table (data migration)
	read2_code <- read2_term <- NULL
	ctv3_concept <- ctv3_termid <- ctv3_simple <- NULL
	icd10_code <- opcs4_code <- NULL
	conceptId <- mapPriority <- mapCategoryId <- mapTarget <- NULL
	referencedComponentId <- refsetId <- NULL

	
	if (any(to %in% c('read2', 'ctv3'))){
		if (is.null(mappingtable)){
			stop('mappingtable required for mapping to read2 or ctv3')
		} else {
			M <- mappingtable
		}
	} 
	if (!(all(to %in% c('read2', 'ctv3', 'icd10', 'opcs4', 'ctv3simple')))){
		stop('each element of "to" must be either "read2", "ctv3",
			"ctv3simple", "icd10" or "opcs4"')
	}
	# Returns the original concepts and the linked concepts as a
	# data.table
	if (is.SNOMEDconcept(x)){
		out <- SNOMEDcodelist(x, include_desc = FALSE, SNOMED = SNOMED)
	} else if (is.SNOMEDcodelist(x)){
		out <- data.table::copy(as.SNOMEDcodelist(x, format = 'simple',
			SNOMED = SNOMED))
	} else {
		stop('x must be a SNOMEDcodelist or SNOMEDconcept')
	}
	
	clean <- function(x){
		# removes NULLs from a list of character vectors
		lapply(x, function(i){
			if (is.null(i)) character(0) else i
		})
	}
	
	# Extract the relevant codeset
	if ('read2' %in% to){
		if ('read2_code' %in% names(out)) out[, read2_code := NULL]
		if ('read2_term' %in% names(out)) out[, read2_term := NULL]
		if (single_row_per_concept){
			out[, read2_code := clean(M[out, on = 'conceptId']$read2_code)]
			out[, read2_term := clean(M[out, on = 'conceptId']$read2_term)]
		} else {
			out <- merge(M[, list(read2_code = unlist(read2_code),
				read2_term = unlist(read2_term)), by = conceptId], out,
				on = 'conceptId')
		}
	}
	if ('ctv3' %in% to){
		if ('ctv3_concept' %in% names(out)) out[, ctv3_concept := NULL]
		if ('ctv3_termid' %in% names(out)) out[, ctv3_termid := NULL]
		if (single_row_per_concept){
			out[, ctv3_concept := clean(M[out, on = 'conceptId']$ctv3_concept)]
			out[, ctv3_termid := clean(M[out, on = 'conceptId']$ctv3_termid)]
		} else {
			out <- merge(M[, list(ctv3_concept = unlist(ctv3_concept),
				ctv3_termid = unlist(ctv3_termid)), by = conceptId], out,
				on = 'conceptId')
		}
	}
	if ('ctv3simple' %in% to){
		# Use CTV3 simple map within SNOMED dictionary
		if ('ctv3_simple' %in% names(out)) out[, ctv3_simple := NULL]
		TEMP <- merge(SNOMED$SIMPLEMAP[
			refsetId == bit64::as.integer64('900000000000497000'),
			list(ctv3_simple = list(mapTarget)),
			by = list(conceptId = referencedComponentId)],
			out[, list(conceptId)], by = 'conceptId')
		if (single_row_per_concept){
			out[, ctv3_simple := clean(TEMP[out, on = 'conceptId']$ctv3_simple)]
		} else {
			out <- merge(TEMP[, list(ctv3_simple = unlist(ctv3_simple)),
				by = conceptId], out, on = 'conceptId')
		}
	}
	if ('icd10' %in% to){
		if ('icd10_code' %in% names(out)) out[, icd10_code := NULL]
		# Limit to Map source concept is properly classified 
		# (mapCategoryId = 447637006)
		# mapPriority is almost always 1 for these maps
		# Up to 5 ICD-10 codes mapped per SNOMED CT concept, but mostly
		# just one.
		TEMP <- merge(SNOMED$EXTENDEDMAP[mapPriority == 1 &
			mapCategoryId == bit64::as.integer64('447637006') &
			(refsetId == bit64::as.integer64('447562003') |
			refsetId == bit64::as.integer64('999002271000000101')),
			list(icd10_code = list(mapTarget)),
			by = list(conceptId = referencedComponentId)],
			out[, list(conceptId)], by = 'conceptId')
		if (single_row_per_concept){
			out[, icd10_code := clean(TEMP[out, on = 'conceptId']$icd10_code)]
		} else {
			out <- merge(TEMP[, list(icd10_code = unlist(icd10_code)),
				by = conceptId], out, on = 'conceptId')
		}
	}
	if ('opcs4' %in% to){
		if ('opcs4_code' %in% names(out)) out[, opcs4_code := NULL]
		# mapCategoryId is NULL for OPCS maps
		TEMP <- merge(SNOMED$EXTENDEDMAP[mapPriority == 1 &
			refsetId == bit64::as.integer64(c('1126441000000105')),
			list(opcs4_code = list(mapTarget)),
			by = list(conceptId = referencedComponentId)],
			out[, list(conceptId)], by = 'conceptId')
		if (single_row_per_concept){
			out[, opcs4_code := clean(TEMP[out, on = 'conceptId']$opcs4_code)]
		} else {
			out <- merge(TEMP[,
				list(opcs4_code = unlist(opcs4_code)),
				by = conceptId], out, on = 'conceptId')
		}
	}
	out
}

