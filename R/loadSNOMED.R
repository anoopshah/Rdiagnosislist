#' Load SNOMED CT files from a folder(s) into R data.table objects
#'
#' Identifies relevant SNOMED CT files from a distribution and loads 
#' them into an R environment. Files from two folders (e.g.
#' International and UK versions) can be loaded together and appended.
#'
#' @param folders Vector of folder paths containing SNOMED CT files
#' @param active_only Whether to limit to current (active) SNOMED CT terms
#' @return An environment containing data.table objects: CONCEPT,
#'   DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#' @export
#' @seealso loadMAPS, CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, sampleSNOMED, getSNOMED
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- sampleSNOMED()
#'
#' # Export to temporary directory
#' for (table in c('Concept', 'Description', 'Relationship',
#'   'StatedRelationship')){
#'   write.table(get(toupper(table), envir = TEST), paste0(tempdir(),
#'     '/sct_', table, '_text.txt'), row.names = FALSE, sep = '|', quote = FALSE)
#' }
#'
#' # Try to import using the loadSNOMED function
#' TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
#'
#' # Check that reimported SNOMED dictionary is the same as the original
#' all.equal(TEST$CONCEPT, TEST2$CONCEPT)
#' all.equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
#' all.equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
#' all.equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
loadSNOMED <- function(folders, active_only = TRUE){
	.temp <- active <- term <- NULL
	
	SNOMED <- new.env()
	append <- FALSE
	for (folder in folders){
		message('Attempting to load from ', folder)
		files <- dir(folder)
		used <- rep(FALSE, length(files))
		for (filename in c('Concept', 'Description', 'StatedRelationship',
			'Relationship')){
			touse <- which(files %like% paste0('_', filename, '_')
				& used == FALSE)
			used[touse] <- TRUE
			if (length(touse == 1)){
				message('Attempting to load ', files[touse])
				TEMP <- NULL
				try(TEMP <- data.table::fread(paste0(folder, '/',
					files[touse]), quote = ""))
				if (is.null(TEMP)){
					warning('Failed to load file.')
				} else {
					message('  Loaded ', nrow(TEMP), ' rows.')
					# Save original column order
					orig_col_order <- copy(names(TEMP))
					# Convert all 'time' columns to times (YYYYMMDD format)
					toconvert <- names(TEMP)[names(TEMP) %like% 'Time']
					if (length(toconvert) > 0){
						for (i in toconvert){
							TEMP[, .temp := data.table::as.IDate(
								as.character(get(i)), '%Y%m%d')]
							TEMP[is.na(.temp), .temp := data.table::as.IDate(
								as.character(get(i)), '%Y-%m-%d')]
							if (all(is.na(TEMP[['.temp']]))){
								message('  Failed to convert ', i, ' to IDate.')
							} else {
								TEMP[, (i) := NULL]
								data.table::setnames(TEMP, '.temp', i)
								message('  Converted ', i, ' to IDate.')
							}
						}
					}
					# Convert all integer 'Id' columns to integer64
					toconvert <- names(TEMP)[which(
						(names(TEMP) %like% 'Id$') &
						(sapply(TEMP, class) == 'integer'))]
					if (length(toconvert) > 0){
						for (i in toconvert){
							message('  Converting ', i, ' to integer64.')
							TEMP[, .temp := bit64::as.integer64(get(i))]
							TEMP[, (i) := NULL]
							data.table::setnames(TEMP, '.temp', i)
						}
					}
					# Convert 'active' columns to logical
					if ('active' %in% names(TEMP)){
						if (active_only){
							message('  Limiting to active rows (', 
								sum(TEMP$active), '/', nrow(TEMP), ').')
							TEMP <- TEMP[active == TRUE]
						} else if (all(bit64::as.integer64(TEMP$active)) %in%
								bit64::as.integer64(c(0, 1))){
							message('  Converting active to logical.')
							TEMP[, .temp := as.logical(active)]
							TEMP[, active := NULL]
							data.table::setnames(TEMP, '.temp', 'active')
						}
					}
					# Restore original column order
					setcolorder(TEMP, orig_col_order)
					# Return the table or append to another partial table
					if (append){
						message('  Attempting to append to ', toupper(filename))
						EXISTING <- NULL
						try(EXISTING <- get(toupper(filename),
							envir = SNOMED, inherits = FALSE))
						if (is.null(EXISTING)){
							message('  No table in original, using new.')
						} else if (nrow(TEMP) == 0 & nrow(EXISTING) == 0){
							warning('  No data in original or new file.')
						} else if (nrow(TEMP) == 0 & nrow(EXISTING) > 0){
							message('  No data in new file, keeping original.')
							TEMP <- EXISTING
						} else if (nrow(TEMP) > 0 & nrow(EXISTING) == 0){
							message('  No data in original, using new.')
						} else {
							existingN <- nrow(TEMP)
							try(TEMP <- rbind(TEMP, EXISTING, use.names = TRUE, fill = TRUE))
							if (nrow(TEMP) > existingN){
								message('  Successfully appended.')
							}
						}
					} else {
						message('  Naming as ', toupper(filename))
					}
					assign(toupper(filename), value = TEMP, envir = SNOMED)
				}
			} else if (length(touse) > 1) {
				warning('Unable to identify correct file for ',
					filename, ' from: ',
					paste(files[touse], collapse = ', '))
			} else if (length(touse) == 0){
				message('No files matching ', filename)
			}
		}
		
		# Append files from 2nd and subsequent folders
		append <- TRUE
	}
	
	# Remove double quotes around descriptions
	SNOMED$DESCRIPTION[, term := gsub('^\\"(.+)\\"$', '\\1', term)]
	
	# Add indices to enable fast searching
	SNOMED <- createSNOMEDindices(SNOMED)
	
	# Add metadata to environment
	assign('metadata', value = list(source = folders,
		active_only = active_only), envir = SNOMED)
	
	cat('\nSNOMED CT tables loaded into environment:\n')
	data.table::tables(env = SNOMED)
	return(SNOMED)
}

#' Create indices for tables in a SNOMED environment
#'
#' Creates relevant indices for fast searching of SNOMED CT tables
#'
#' @param SNOMED environment containing data.table objects: CONCEPT,
#'   DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#' @seealso CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#' @return The environment with indices added to each table for
#'   fast searching
createSNOMEDindices <- function(SNOMED){
	id <- active <- conceptId <- typeId <- term <- active <- NULL
	sourceId <- destinationId <- NULL
	
	data.table::setindex(SNOMED$CONCEPT, id)
	data.table::setindex(SNOMED$CONCEPT, active)
	
	data.table::setindex(SNOMED$DESCRIPTION, id)
	data.table::setindex(SNOMED$DESCRIPTION, conceptId)
	data.table::setindex(SNOMED$DESCRIPTION, typeId)
	data.table::setindex(SNOMED$DESCRIPTION, term)
	data.table::setindex(SNOMED$DESCRIPTION, active)

	data.table::setindex(SNOMED$STATEDRELATIONSHIP, id)
	data.table::setindex(SNOMED$STATEDRELATIONSHIP, sourceId)
	data.table::setindex(SNOMED$STATEDRELATIONSHIP, destinationId)
	data.table::setindex(SNOMED$STATEDRELATIONSHIP, typeId)
	data.table::setindex(SNOMED$STATEDRELATIONSHIP, active)

	data.table::setindex(SNOMED$RELATIONSHIP, id)
	data.table::setindex(SNOMED$RELATIONSHIP, sourceId)
	data.table::setindex(SNOMED$RELATIONSHIP, destinationId)
	data.table::setindex(SNOMED$RELATIONSHIP, typeId)
	data.table::setindex(SNOMED$RELATIONSHIP, active)
	return(SNOMED)
}

#' Sample SNOMED CT dictionary
#'
#' Returns an environment containing a selection of SNOMED CT
#' terms, their relationships and descriptions which are
#' provided with the package
#'
#' @return environment containing four data.table objects:
#'   CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#'   and a list named 'metadata'
#' @export
#' @seealso CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, loadSNOMED, getSNOMED
#' @examples
#' TEST <- sampleSNOMED()
#' inactiveIncluded(TEST)
#' SNOMEDconcept('Heart failure', SNOMED = TEST)
#'
#' # To display metadata for this SNOMED CT dictionary
#' sampleSNOMED()$metadata
sampleSNOMED <- function(){
	SNOMED <- new.env()
	data(CONCEPT, envir = SNOMED)
	data(RELATIONSHIP, envir = SNOMED)
	data(STATEDRELATIONSHIP, envir = SNOMED)
	data(DESCRIPTION, envir = SNOMED)
	SNOMED <- createSNOMEDindices(SNOMED)
	assign('metadata', value = list(source = 'sample',
		active_only = FALSE), envir = SNOMED)
	return(SNOMED)
}

#' Retrieves SNOMED CT dictionary from the global environment
#'
#' Returns an object named 'SNOMED' from the global
#' environment. Returns an error if no such object exists,
#' or if it is not an environment containing tables named
#' CONCEPT, RELATIONSHIP, STATEDRELATIONSHIP and DESCRIPTION.
#' There is no attempt to check that these tables are actually, if available. environment containing a selection of SNOMED CT
#' terms, their relationships and descriptions which are
#' provided with the package
#'
#' @return SNOMED environment from the global environment
#' @seealso CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, loadSNOMED, sampleSNOMED
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#' SNOMED2 <- getSNOMED()
#'
#' # To display metadata for this SNOMED CT dictionary
#' SNOMED2$metadata
getSNOMED <- function(){
	SNOMED <- NULL
	SNOMED <- get('SNOMED', envir = globalenv())
	if (is.null(SNOMED)){
		stop('No object SNOMED found in global environment')
	}
	if (!is.environment(SNOMED)){
		stop('SNOMED is not an environment')
	}
	if (!('CONCEPT' %in% data.table::tables(env = SNOMED,
		silent = TRUE)$NAME)){
		stop('No table named CONCEPT in SNOMED environment')
	}
	if (!('RELATIONSHIP' %in% data.table::tables(env = SNOMED,
		silent = TRUE)$NAME)){
		stop('No table named RELATIONSHIP in SNOMED environment')
	}
	if (!('STATEDRELATIONSHIP' %in% data.table::tables(env = SNOMED,
		silent = TRUE)$NAME)){
		stop('No table named STATEDRELATIONSHIP in SNOMED environment')
	}
	if (!('DESCRIPTION' %in% data.table::tables(env = SNOMED,
		silent = TRUE)$NAME)){
		stop('No table named DESCRIPTION in SNOMED environment')
	}
	# Return the retrieved environment
	SNOMED
}


#' Load mappings from Read to SNOMED CT into an R data.table
#'
#' Creates a mapping table derived from NHS Digital
#' Data Migration distribution. These tables are available from
#' the Technology Reference data Update Distribution
#' \url{https://isd.digital.nhs.uk/trud3/user/guest/group/0/pack/9/subpack/9/releases}.
#' 
#' The final release was in April 2020. The mapping tables are
#' intended for converting entires in clinical records from
#' Read Version 2 (Read 2) to SNOMED CT, and Clinical Terms
#' Version 3 (CTV3) to SNOMED CT.
#' 
#' These maps can be used for converting SNOMED CT codelists into
#' Read 2 or CTV3 format for running queries, such as to characterise
#' patient phenotypes or identify patient populations for research.
#' They cannot be used in the reverse direction (to map a Read 2/CTV3
#' codelist to SNOMED CT) because some of the SNOMED CT terms will
#' be missed out, and the list will be incomplete.
#'
#' This function uses the following three mapping files:
#' \itemize{
#'    \item{not_assured_rcsctmap_uk}{ File containing Read 2 codes
#'      mapped to SNOMED CT, in file:
#'      'Not Clinically Assured/rcsctmap_uk_20200401000001.txt'}
#'    \item{not_assured_rctermsctmap_uk}{ File containing Read 2 terms
#'      mapped to SNOMED CT, in file:
#'      'Not Clinically Assured/rctermsctmap_uk_20200401000001.txt'}
#'    \item{assured_ctv3sctmap2_uk}{ File containing CTV3 concepts
#'      and terms mapped to SNOMED CT, in file:
#'      'Clinically Assured/ctv3sctmap2_uk_20200401000001.txt'}
#' }
#' 
#' The output data.table has the following columns:
#' \itemize{
#'   \item{conceptId}{ integer64: SNOMED CT conceptId (primary key)} 
#'   \item{read2_code}{ list: character list of 7-character Read 2 codes}
#'   \item{read2_term}{ list: character list of Read 2 terms}
#'   \item{ctv3_concept}{ list: character list of CTV3 concept codes}
#'   \item{ctv3_termid}{ list: character list of CTV3 term description codes}
#' }
#'
#' @param not_assured_rcsctmap_uk File containing Read 2 codes
#'      mapped to SNOMED CT, in file:
#'      'Not Clinically Assured/rcsctmap_uk_20200401000001.txt'
#' @param not_assured_rctermsctmap_uk File containing Read 2 terms
#'      mapped to SNOMED CT, in file:
#'      'Not Clinically Assured/rctermsctmap_uk_20200401000001.txt'
#' @param assured_ctv3sctmap2_uk File containing CTV3 concepts
#'      and terms mapped to SNOMED CT, in file:
#'      'Clinically Assured/ctv3sctmap2_uk_20200401000001.txt'
#' @return A data.table with columns conceptId, read2_code,
#'   ctv3_concept, ctv3_termid
#' @seealso MAPS, getMaps, loadSNOMED
#' @export
#' @examples
#'
loadMAPS <- function(not_assured_rcsctmap_uk,
	not_assured_rctermsctmap_uk, assured_ctv3sctmap2_uk){
		
	S_READCODE <- fread(not_assured_rcsctmap_uk)
	S_READTERM <- fread(not_assured_rctermsctmap_uk, quote = '')
	S_V3 <- fread(assured_ctv3sctmap2_uk)

	S_READCODE[MapStatus == 1,
		keep := EffectiveDate == max(EffectiveDate),
		by = .(MapId, ConceptId)]
	S_READCODE[, read2_code := paste0(ReadCode, TermCode)]
	S_V3[MAPSTATUS == 1, keep := EFFECTIVEDATE == max(EFFECTIVEDATE),
		by = .(MAPID, SCT_CONCEPTID)]

	# Keep the longest (most descriptive) Read term
	S_READTERM[, keep := nchar(Term) == max(nchar(Term)), by = MapId]

	V2MAPS <- merge(S_READCODE[keep == TRUE,
		.(conceptId = as.integer64(ConceptId), read2_code, MapId)],
		S_READTERM[keep == TRUE, .(MapId, read2_term = Term)],
		by = 'MapId')
	V3MAPS <- S_V3[keep == TRUE,
		.(conceptId = as.integer64(SCT_CONCEPTID),
		ctv3_concept = CTV3_CONCEPTID,
		ctv3_termid = CTV3_TERMID)]

	# Now convert into a one-row-per-concept table
	MAPS <- merge(V2MAPS[, .(read2_code = list(read2_code),
		read2_term = list(read2_term)), by = conceptId],
		V3MAPS[, .(ctv3_concept = list(ctv3_concept),
		ctv3_termid = list(ctv3_termid)),
		by = conceptId], by = 'conceptId')
	MAPS
}
