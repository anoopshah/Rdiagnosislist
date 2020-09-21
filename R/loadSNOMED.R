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
#' # Load from the data folder
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
					# Convert all 'time' columns to times (YYYYMMDD format)
					toconvert <- names(TEMP)[names(TEMP) %like% 'Time']
					if (length(toconvert) > 0){
						for (i in toconvert){
							message('  Converting ', i, ' to IDate.')
							try(TEMP[, .temp := data.table::as.IDate(
								as.character(get(i)), '%Y%m%d')])
							if ('.temp' %in% names(TEMP)){
								TEMP[, (i) := NULL]
								data.table::setnames(TEMP, '.temp', i)
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
					if (append){
						message('  Appending to ', toupper(filename))
						TEMP2 <- get(toupper(filename), envir = SNOMED)
						try(TEMP <- rbind(TEMP, TEMP2, use.names = TRUE))
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

