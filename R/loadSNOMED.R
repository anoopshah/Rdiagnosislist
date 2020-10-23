#' Load SNOMED files from a folder(s) into R data.table objects
#'
#' Identifies relevant SNOMED files from a distribution and loads 
#' them into an R environment. Files from two folders (e.g.
#' International and UK versions) can be loaded together and appended.
#'
#' @param folderpaths Vector of folder paths containing SNOMED CT files
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
#'     '/', table, '.txt'), row.names = FALSE, sep = '\t', quote = FALSE)
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
	SNOMED <- new.env()
	append <- FALSE
	for (folder in folders){
		files <- dir(folder)
		used <- rep(FALSE, length(files))
		for (filename in c('Concept', 'Description', 'StatedRelationship',
			'Relationship')){
			touse <- which(files %like% filename & used == FALSE)
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
						} else if (
							all(as.integer64(SNOMED$RELATIONSHIP$active))
							%in% as.integer64(c(0, 1))){
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
						message('  Appending to ', toupper(filename))
						TEMP2 <- get(toupper(filename), envir = SNOMED)
						try(TEMP <- rbind(TEMP, TEMP2, use.names = TRUE, fill = TRUE))
					} else {
						message('  Naming as ', toupper(filename))
					}
					assign(toupper(filename), value = TEMP, envir = SNOMED)
				}
			} else {
				warning('Unable to identify correct file from ', touse)
			}
		}
		
		# Append files from 2nd and subsequent folders
		append <- TRUE
	}
	
	# Remove double quotes around descriptions
	SNOMED$DESCRIPTION[, term := gsub('^\\"(.+)\\"$', '\\1', term)]
	
	# Add metadata to environment
	assign('metadata', value = list(source = folders,
		active_only = active_only), envir = SNOMED)
	
	cat('\nSNOMED CT tables loaded into environment:\n')
	data.table::tables(env = SNOMED)
	return(SNOMED)
}

