#' Load SNOMED CT files from a folder(s) into R data.table objects
#'
#' Identifies relevant SNOMED CT files from the 'Snapshot' of a 
#' distribution and loads them into an R environment. Files from
#' two folders (e.g. International and UK versions) can be loaded
#' together and appended.
#'
#' These files are available from the NHS Digital Technology Reference
#' Update Distribution:
#' \url{https://isd.digital.nhs.uk/trud/user/guest/group/0/home}
#'
#' (Note: May 2022 - This function needs to be updated to use the 
#' latest SNOMED CT TRUD versions including the SNOMED CT definitions).
#'
#' @param folders Vector of folder paths containing SNOMED CT files
#' @param active_only Whether to limit to current (active) SNOMED CT
#'   concepts
#' @param version Version description. If NULL, it is derived from the
#'   folder paths and expressed in the form: INT{date} & UK{date}
#' @return An environment containing data.table objects: CONCEPT,
#'   DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, REFSET,
#'   SIMPLEMAP, EXTENDEDMAP
#' @export
#' @seealso loadREADMAPS, CONCEPT, DESCRIPTION, RELATIONSHIP,
#' STATEDRELATIONSHIP, REFSET, SIMPLEMAP, EXTENDEDMAP,
#' sampleSNOMED, getSNOMED, exportSNOMEDenvir
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- sampleSNOMED()
#'
#' # Export to temporary directory
#' exportSNOMEDenvir(TEST, tempdir())
#'
#' # Try to import using the loadSNOMED function
#' TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
#'
#' # Check that reimported SNOMED dictionary is the same as the original
#' all.equal(TEST$CONCEPT, TEST2$CONCEPT)
#' all.equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
#' all.equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
#' all.equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
#' all.equal(TEST$REFSET, TEST2$REFSET)
#' all.equal(TEST$SIMPLEMAP, TEST2$SIMPLEMAP)
#' all.equal(TEST$EXTENDEDMAP, TEST2$EXTENDEDMAP)

# To modify this to load Refset and maps into the main SNOMED dictionary

loadSNOMED <- function(folders, active_only = TRUE,
	version = NULL){
	.temp <- active <- term <- NULL
	id <- correlationId <- mapTarget <- pattern <- NULL

	FILENAMES <- fread('pattern|table
_Concept_.*Snapshot|CONCEPT
_Description_.*Snapshot|DESCRIPTION
_StatedRelationship_.*Snapshot|STATEDRELATIONSHIP
_Relationship_.*Snapshot|RELATIONSHIP
Refset_SimpleMap.*Snapshot|SIMPLEMAP
Refset_ExtendedMap.*Snapshot|EXTENDEDMAP
Refset_Simple.*Snapshot|REFSET

	SNOMED <- new.env()
	append <- FALSE
	for (folder in folders){
		message('Attempting to load from ', folder)
		files <- dir(folder, recursive = TRUE, full.names = TRUE)
		used <- rep(FALSE, length(files))
		for (thispattern in FILENAMES$pattern){
			touse <- which(files %like% thispattern & used == FALSE)
			used[touse] <- TRUE
			if (length(touse) == 0){
				message('No files matching ', thispattern)
			} else {
				for (thisfile in files[touse]){
					message('Attempting to load ', sub(folder, '', thisfile))
					TEMP <- NULL
					try(TEMP <- data.table::fread(thisfile, quote = ""))
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
						#
						# Remove unnecessary columns
						# Remove 'id' and 'correlationId' columns for
						# maps and refsets to save space
						if (thispattern %like% 'Refset_'){
							if ('id' %in% names(TEMP)){
								TEMP[, id := NULL]
							}
							if ('correlationId' %in% names(TEMP)){
								TEMP[, correlationId := NULL]
								# all the same, no uesful info in this column
							}
						}
						# Remove . from ICD-10 terms
						if (thispattern == 'Refset_ExtendedMapSnapshot'){
							TEMP[, mapTarget := sub('\\.', '', mapTarget)]
						}
						# Return the table or append to another partial table
						if (append){
							message('  Attempting to append to ',
								FILENAMES[thispattern == pattern]$table)
							EXISTING <- NULL
							try(EXISTING <- get(FILENAMES[thispattern == pattern]$table,
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
								try(TEMP <- rbind(TEMP, EXISTING,
									use.names = TRUE, fill = TRUE))
								if (nrow(TEMP) > existingN){
									message('  Successfully appended.')
								}
							}
						} else {
							message('  Naming as ', FILENAMES[thispattern == pattern]$table)
						}
						assign(FILENAMES[thispattern == pattern]$table, value = TEMP,
							envir = SNOMED)
					}
				}
			}
		}
		
		# Append files from 2nd and subsequent folders
		append <- TRUE
	}
	
	# Remove double quotes around descriptions
	SNOMED$DESCRIPTION[, term := gsub('^\\"(.+)\\"$', '\\1', term)]
	
	# Add indices to enable fast searching
	SNOMED <- createSNOMEDindices(SNOMED)
	
	# Assign version
	if (is.null(version)){
		version <- paste(
			paste0(ifelse(folders %like% 'International', 
			'Int', ifelse(folders %like% 'UKClinical', 'UK', ''))),
			sub('.*PRODUCTION_([0-9]{8})T.*', '\\1', folders),
			collapse = ' & ')
	}
	
	# Add metadata to environment
	assign('metadata', value = list(source = folders,
		active_only = active_only, version = version), envir = SNOMED)
	
	cat('\nSNOMED CT tables loaded into environment:\n')
	data.table::tables(env = SNOMED)
	return(SNOMED)
}

#' Export a SNOMED environment to a folder
#'
#' Creates tab separated files which can be reloaded with relevant indices for fast searching of SNOMED CT tables
#'
#' @param SNOMED environment containing data.table objects: CONCEPT,
#'   DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, REFSET,
#'   SIMPLEMAP, EXTENDEDMAP
#' @param folder path to folder where files will be written
#' @seealso CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#' @return NULL 
#' @export
exportSNOMEDenvir <- function(SNOMED, folder){
	data.table::fwrite(get('CONCEPT', envir = SNOMED, inherits = FALSE),
		paste0(folder, '/_Concept_Snapshot.txt'), sep = '\t', quote = FALSE)
	data.table::fwrite(get('DESCRIPTION', envir = SNOMED, inherits = FALSE),
		paste0(folder, '/_Description_Snapshot.txt'), sep = '\t', quote = FALSE)
	data.table::fwrite(get('RELATIONSHIP', envir = SNOMED, inherits = FALSE),
		paste0(folder, '/_Relationship_Snapshot.txt'), sep = '\t', quote = FALSE)
	data.table::fwrite(get('STATEDRELATIONSHIP', envir = SNOMED, inherits = FALSE),
		paste0(folder, '/_StatedRelationship_Snapshot.txt'), sep = '\t', quote = FALSE)
	data.table::fwrite(get('REFSET', envir = SNOMED, inherits = FALSE),
		paste0(folder, '/Refset_SimpleSnapshot.txt'), sep = '\t', quote = FALSE)
	data.table::fwrite(get('SIMPLEMAP', envir = SNOMED, inherits = FALSE),
		paste0(folder, '/Refset_SimpleMapSnapshot.txt'), sep = '\t', quote = FALSE)
	data.table::fwrite(get('EXTENDEDMAP', envir = SNOMED, inherits = FALSE),
		paste0(folder, '/Refset_ExtendedMapSnapshot.txt'), sep = '\t', quote = FALSE)
	return(NULL)
}

#' Create indices for tables in a SNOMED environment
#'
#' Creates relevant indices for fast searching of SNOMED CT tables
#'
#' @param SNOMED environment containing data.table objects: CONCEPT,
#'   DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#' @seealso CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, 
#' REFSET, SIMPLEMAP, EXTENDEDMAP, loadSNOMED, sampleSNOMED
#' @return The environment with indices added to each table for
#'   fast searching
createSNOMEDindices <- function(SNOMED){
	id <- active <- conceptId <- typeId <- term <- active <- NULL
	sourceId <- destinationId <- NULL
	moduleId <- refsetId <- referencedComponentId <- NULL
	mapTarget <- mapGroup <- mapPriority <- mapRule <- NULL
	correlationId <- mapCategoryId <- NULL
	
	SNOMED$CONCEPT[, id := bit64::as.integer64(id)]
	SNOMED$CONCEPT[, active := bit64::as.integer64(active)]
	data.table::setkeyv(SNOMED$CONCEPT, 'id')
	
	SNOMED$DESCRIPTION[, id := bit64::as.integer64(id)]
	SNOMED$DESCRIPTION[, conceptId := bit64::as.integer64(conceptId)]
	SNOMED$DESCRIPTION[, typeId := bit64::as.integer64(typeId)]
	SNOMED$DESCRIPTION[, term := as.character(term)]
	SNOMED$DESCRIPTION[, active := as.logical(active)]
	data.table::setkeyv(SNOMED$DESCRIPTION, 'id')
	data.table::setindexv(SNOMED$DESCRIPTION, c('conceptId', 'typeId', 'term', 'active'))
	data.table::setindexv(SNOMED$DESCRIPTION, c('term', 'active'))
	data.table::setindexv(SNOMED$DESCRIPTION, c('typeId', 'active'))

	SNOMED$STATEDRELATIONSHIP[, id := bit64::as.integer64(id)]
	SNOMED$STATEDRELATIONSHIP[, sourceId := bit64::as.integer64(sourceId)]
	SNOMED$STATEDRELATIONSHIP[, destinationId := bit64::as.integer64(destinationId)]
	SNOMED$STATEDRELATIONSHIP[, typeId := bit64::as.integer64(typeId)]
	SNOMED$STATEDRELATIONSHIP[, active := as.logical(active)]
	data.table::setkeyv(SNOMED$STATEDRELATIONSHIP, c('sourceId', 'typeId', 'active'))
	data.table::setindexv(SNOMED$STATEDRELATIONSHIP, c('destinationId', 'typeId', 'active'))
	data.table::setindexv(SNOMED$STATEDRELATIONSHIP, c('sourceId', 'destinationId', 'typeId', 'active'))
	data.table::setindexv(SNOMED$STATEDRELATIONSHIP, c('destinationId', 'sourceId', 'typeId', 'active'))

	SNOMED$RELATIONSHIP[, id := bit64::as.integer64(id)]
	SNOMED$RELATIONSHIP[, sourceId := bit64::as.integer64(sourceId)]
	SNOMED$RELATIONSHIP[, destinationId := bit64::as.integer64(destinationId)]
	SNOMED$RELATIONSHIP[, typeId := bit64::as.integer64(typeId)]
	SNOMED$RELATIONSHIP[, active := as.logical(active)]
	data.table::setkeyv(SNOMED$RELATIONSHIP, c('sourceId', 'typeId', 'active'))
	data.table::setindexv(SNOMED$RELATIONSHIP, c('destinationId', 'typeId', 'active'))
	data.table::setindexv(SNOMED$RELATIONSHIP, c('sourceId', 'destinationId', 'typeId', 'active'))
	data.table::setindexv(SNOMED$RELATIONSHIP, c('destinationId', 'sourceId', 'typeId', 'active'))

	if ('REFSET' %in% ls(SNOMED)){
		SNOMED$REFSET[, moduleId := bit64::as.integer64(moduleId)]
		SNOMED$REFSET[, refsetId := bit64::as.integer64(refsetId)]
		SNOMED$REFSET[, referencedComponentId := bit64::as.integer64(referencedComponentId)]
		SNOMED$REFSET[, active := as.logical(active)]
		data.table::setkeyv(SNOMED$REFSET, c('refsetId', 'referencedComponentId'))
	}
	
	if ('SIMPLEMAP' %in% ls(SNOMED)){
		SNOMED$SIMPLEMAP[, moduleId := bit64::as.integer64(moduleId)]
		SNOMED$SIMPLEMAP[, refsetId := bit64::as.integer64(refsetId)]
		SNOMED$SIMPLEMAP[, referencedComponentId := bit64::as.integer64(referencedComponentId)]
		SNOMED$SIMPLEMAP[, mapTarget := as.character(mapTarget)]
		SNOMED$SIMPLEMAP[, active := as.logical(active)]
		data.table::setkeyv(SNOMED$SIMPLEMAP, c('refsetId', 'mapTarget', 'referencedComponentId'))
	}

	if ('EXTENDEDMAP' %in% ls(SNOMED)){	
		SNOMED$EXTENDEDMAP[, moduleId := bit64::as.integer64(moduleId)]
		SNOMED$EXTENDEDMAP[, refsetId := bit64::as.integer64(refsetId)]
		SNOMED$EXTENDEDMAP[, referencedComponentId := bit64::as.integer64(referencedComponentId)]
		SNOMED$EXTENDEDMAP[, mapGroup := as.integer(mapGroup)]
		SNOMED$EXTENDEDMAP[, mapPriority := as.integer(mapPriority)]
		SNOMED$EXTENDEDMAP[, mapRule := as.character(mapRule)]
		SNOMED$EXTENDEDMAP[, mapTarget := as.character(mapTarget)]
		# data.table::setindex(SNOMED$EXTENDEDMAP, correlationId)
		# not using correlationId because they are all the same
		SNOMED$EXTENDEDMAP[, mapCategoryId := bit64::as.integer64(mapCategoryId)]
		SNOMED$EXTENDEDMAP[, active := as.logical(active)]
		data.table::setkeyv(SNOMED$EXTENDEDMAP, c('mapPriority', 'mapCategoryId', 'refsetId',
			'referencedComponentId'))
	}

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
#' @seealso CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, 
#' REFSET, SIMPLEMAP, EXTENDEDMAP, loadSNOMED, sampleSNOMED
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
	data(REFSET, envir = SNOMED)
	data(SIMPLEMAP, envir = SNOMED)
	data(EXTENDEDMAP, envir = SNOMED)
	SNOMED <- createSNOMEDindices(SNOMED)
	assign('metadata', value = list(source = 'sample',
		active_only = FALSE, version = 'Sample'), envir = SNOMED)
	return(SNOMED)
}

#' Retrieves SNOMED CT dictionary from the global environment
#'
#' Returns an object named 'SNOMED' from the global
#' environment. Returns an error if no such object exists,
#' or if it is not an environment containing tables named
#' CONCEPT, RELATIONSHIP, STATEDRELATIONSHIP and DESCRIPTION.
#' There is no attempt to check that these tables are actually valid.
#'
#' @param SNOMEDname name of the SNOMED environment to search for
#' @return SNOMED environment from the global environment
#' @seealso CONCEPT, DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP, 
#' REFSET, SIMPLEMAP, EXTENDEDMAP, loadSNOMED, sampleSNOMED
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#' SNOMED2 <- getSNOMED()
#'
#' # To display metadata for this SNOMED CT dictionary
#' SNOMED2$metadata
getSNOMED <- function(SNOMEDname = 'SNOMED'){
	SNOMED <- NULL
	SNOMED <- get(SNOMEDname, envir = globalenv())
	if (is.null(SNOMED)){
		stop('No object SNOMED found in global environment')
	}
	if (!is.environment(SNOMED)){
		stop('SNOMED is not an environment')
	}
	if (is.null(SNOMED$CONCEPT)){
		stop('No table named CONCEPT in SNOMED environment')
	}
	if (is.null(SNOMED$RELATIONSHIP)){
		stop('No table named RELATIONSHIP in SNOMED environment')
	}
	if (is.null(SNOMED$STATEDRELATIONSHIP)){
		stop('No table named STATEDRELATIONSHIP in SNOMED environment')
	}
	if (is.null(SNOMED$DESCRIPTION)){
		stop('No table named DESCRIPTION in SNOMED environment')
	}
	if (is.null(SNOMED$REFSET)){
		warning('No table named REFSET in SNOMED environment')
	}
	if (is.null(SNOMED$SIMPLEMAP)){
		warning('No table named SIMPLEMAP in SNOMED environment')
	}
	if (is.null(SNOMED$EXTENDEDMAP)){
		warning('No table named EXTENDEDMAP in SNOMED environment')
	}
	# Return the retrieved environment
	SNOMED
}

#' Load mappings from Read to SNOMED CT into an R data.table
#'
#' Creates a mapping table derived from NHS Digital
#' Data Migration distribution. These tables are available from
#' the Technology Reference Update Distribution:
#' \url{https://isd.digital.nhs.uk/trud/user/guest/group/0/pack/9/subpack/9/releases}
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
#' @seealso READMAPS, getMaps, loadSNOMED
#' @export
loadREADMAPS <- function(not_assured_rcsctmap_uk,
	not_assured_rctermsctmap_uk, assured_ctv3sctmap2_uk){
	MapStatus <- keep <- EffectiveDate <- MapId <- NULL
	ConceptId <- read2_code <- ReadCode <- TermCode <- NULL
	MAPSTATUS <- EFFECTIVEDATE <- MAPID <- SCT_CONCEPTID <- NULL
	Term <- CTV3_CONCEPTID <- CTV3_TERMID <- read2_term <- NULL
	conceptId <- ctv3_concept <- ctv3_termid <- NULL
	
	S_READCODE <- fread(not_assured_rcsctmap_uk)
	S_READTERM <- fread(not_assured_rctermsctmap_uk, quote = '')
	S_V3 <- fread(assured_ctv3sctmap2_uk)

	S_READCODE[MapStatus == 1,
		keep := EffectiveDate == max(EffectiveDate),
		by = list(MapId, ConceptId)]
	S_READCODE[, read2_code := paste0(ReadCode, TermCode)]
	S_V3[MAPSTATUS == 1, keep := EFFECTIVEDATE == max(EFFECTIVEDATE),
		by = list(MAPID, SCT_CONCEPTID)]

	# Keep the longest (most descriptive) Read term
	S_READTERM[, keep := nchar(Term) == max(nchar(Term)), by = MapId]

	V2MAPS <- merge(S_READCODE[keep == TRUE,
		list(conceptId = as.integer64(ConceptId), read2_code, MapId)],
		S_READTERM[keep == TRUE, list(MapId, read2_term = Term)],
		by = 'MapId')
	V3MAPS <- S_V3[keep == TRUE,
		list(conceptId = as.integer64(SCT_CONCEPTID),
		ctv3_concept = CTV3_CONCEPTID,
		ctv3_termid = CTV3_TERMID)]

	# Now convert into a one-row-per-concept table
	READMAPS <- merge(V2MAPS[, list(read2_code = list(read2_code),
		read2_term = list(read2_term)), by = conceptId],
		V3MAPS[, list(ctv3_concept = list(ctv3_concept),
		ctv3_termid = list(ctv3_termid)),
		by = conceptId], by = 'conceptId')
	setkeyv(READMAPS, 'conceptId')
	READMAPS
}

