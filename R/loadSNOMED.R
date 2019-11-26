#' Load SNOMED files from a folder(s) into R data.table objects
#'
#' Identifies relevant SNOMED files from a distribution and loads 
#' them into R. Files from two folders (e.g. International and UK
#' versions) can be loaded together and appended.
#'
#' @param folderpaths Vector of folder paths containing SNOMED CT files
#' @param active_only Whether to limit to current (active) SNOMED CT terms
#' @return An environment containing data.table objects: CONCEPT,
#'   DESCRIPTION, RELATIONSHIP, STATEDRELATIONSHIP
#' @export
#' @examples
#' # Load from the data folder
#'
#' @import data.table
#' @import bit64
loadSNOMED <- function(folderpaths, active_only = TRUE){
	# Returns an environment containing the SNOMED CT files
	# loaded into data.tables
	
	append <- FALSE
	for (folderpath in folderpaths){
# assign('NOTE', paste0('SNOMED CT files loaded from ', folderpath),
#	envir = SNOMED)

		files <- dir(folder)
		used <- rep(FALSE, length(files))
		for (filename in c('Concept', 'Description', 'StatedRelationship',
			'Relationship')){
			touse <- which(files %like% filename & used == FALSE)
			used[touse] <- TRUE
			if (length(touse == 1)){
				TEMP <- data.table::fread(paste0(folder, '/', files[touse]),
					quote = "")
				# Convert all 'time' columns to times (YYYYMMDD format)
				toconvert <- names(TEMP)[names(TEMP) %like% 'Time']
				if (length(toconvert) > 0){
					for (i in toconvert){
						try(TEMP[, .temp := data.table::as.IDate(
							as.character(get(i)), '%Y%m%d')])
						if ('.temp' %in% names(TEMP)){
							TEMP[, (i) := NULL]
							data.table::setnames(TEMP, '.temp', i)
						}
					}
				}
				# Convert all integer columns to integer64
				toconvert <- names(TEMP)[
					which(sapply(TEMP, class) == 'integer')]
				if (length(toconvert) > 0){
					for (i in toconvert){
						TEMP[, .temp := bit64::as.integer64(get(i))]
						TEMP[, (i) := NULL]
						data.table::setnames(TEMP, '.temp', i)
					}
				}
				cat('\nLoaded', files[touse], '(', nrow(TEMP), 'rows).\n')
				if (append){
					TEMP2 <- get(toupper(filename), envir = SNOMED)
					try(TEMP <- rbind(TEMP, TEMP2, use.names = TRUE))
				}
				assign(toupper(filename), value = TEMP, envir = myenv)
			} else {
				warning('Unable to load', touse)
			}
		}
		
		# Append files from 2nd and subsequent folders
		append <- TRUE
	}
	return(SNOMED)
}

