#' Load SNOMED files from a folder(s) into R data.table objects
#'
#' Identifies relevant SNOMED files from a distribution and loads 
#' them into an R environment. Files from two folders (e.g.
#' International and UK versions) can be loaded together and appended.
#'
#' @param folders Vector of folder paths containing SNOMED CT files
#' @param active_only Whether to limit to current (active) SNOMED CT terms
#' @return An environment containing data.table objects: CONCEPT,
#'   DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#' @export
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- sampleSNOMED()
#'
#' # Export to temporary directory
#' for (table in c('Concept', 'Description', 'Relationship',
#'   'StatedRelationship')){
#'   write.table(get(toupper(table), envir = TEST), paste0(tempdir(),
#'     '/sct_', table, '_text.txt'), row.names = FALSE, sep = '\t', quote = FALSE)
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
	if (!('CONCEPT' %in% data.table::tables(env = SNOMED)$NAME)){
		stop('No table named CONCEPT in SNOMED environment')
	}
	if (!('RELATIONSHIP' %in% data.table::tables(env = SNOMED)$NAME)){
		stop('No table named RELATIONSHIP in SNOMED environment')
	}
	if (!('STATEDRELATIONSHIP' %in% data.table::tables(env = SNOMED)$NAME)){
		stop('No table named STATEDRELATIONSHIP in SNOMED environment')
	}
	if (!('DESCRIPTION' %in% data.table::tables(env = SNOMED)$NAME)){
		stop('No table named DESCRIPTION in SNOMED environment')
	}
	# Return the retrieved environment
	SNOMED
}
