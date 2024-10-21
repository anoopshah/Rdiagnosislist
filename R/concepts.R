#' Check if inactive terms are included in SNOMED CT dictionary
#'
#' Checks the active_only flag in the metadata of a SNOMED
#' environment to determine whether inactive terms are
#' included
#'
#' @param SNOMED environment containing SNOMED dictionary, defaults
#'   to an object named 'SNOMED' in the global environment
#' @return TRUE or FALSE (logical vector of length one)
#' @export
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- sampleSNOMED()
#' inactiveIncluded(TEST)
#' assign('metadata', list(active_only = TRUE), envir = TEST)
#' inactiveIncluded(TEST)
inactiveIncluded <- function(SNOMED = getSNOMED()){
	if (is.null(SNOMED$metadata$active_only)){
		TRUE
	} else if (SNOMED$metadata$active_only == TRUE){
		FALSE
	} else {
		TRUE
	}
}

#' Returns the SNOMED CT concept IDs for a set of terms
#'
#' Carries out an exact or regular expression match to
#' return the concept ID for a set of search terms, or
#' converts a character, integer or integer64 vector to a
#' SNOMEDconcept object.
#'
#' @param x character vector of terms to match, or
#'   character vector containing SNOMED CT concept IDs, or
#'   64-bit integer vector containing SNOMED CT concept IDs
#' @param active_only whether or not to include inactive concepts
#' @param exact_match if TRUE, only an exact (case sensitive)
#'   match is performed. If FALSE, a regular expression match
#'   is performed.
#' @param unique whether to include no more than one instance of each
#'   SNOMED CT concept
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @param ... additional arguments to send to grepl if using
#'   regular expression matching
#' @return a SNOMEDconcept object (vector of 64-bit integers) containing
#'   unique SNOMED CT concept IDs
#' @import data.table
#' @import bit64
#' @export
#' @examples
#' SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED()) -> hf
#' is.SNOMEDconcept(hf)
#' SNOMEDconcept('900000000000003001')
#' as.SNOMEDconcept('900000000000003001')
SNOMEDconcept <- function(x, active_only = TRUE,
	exact_match = TRUE, unique = TRUE,
	SNOMED = getSNOMED()){
	# Declare names to be used for non-standard evaluation for R CMD check
	active <- conceptId <- NULL
	
	if (length(x) == 0){
		out <- integer64(0)
		class(out) <- c('SNOMEDconcept', 'integer64')
		return(out)
	}
	if ('integer64' %in% class(x)){
		# correct format for a SNOMED CT concept ID
		out <- x
		class(out) <- c('SNOMEDconcept', 'integer64')
		return(out)
	} else if ('integer' %in% class(x)){
		# convert to integer64
		out <- bit64::as.integer64(x)
		class(out) <- c('SNOMEDconcept', 'integer64')
		return(out)
	} else if ('character' %in% class(x)){
		if (all(grepl('^[0-9]+$', x))){
			# concept ID in character format 
			out <- bit64::as.integer64(x)
			class(out) <- c('SNOMEDconcept', 'integer64')
			return(out)
		}
	} else {
		stop('term to match must be character, integer or integer64; ',
			class(x), ' is not acceptable.')
	}

	# Try to match a term description
	if (exact_match){
		if (inactiveIncluded(SNOMED)){
			MATCHED <- SNOMED$DESCRIPTION[data.table(term = x),
				list(active, conceptId), on = 'term']
		} else {
			MATCHED <- SNOMED$DESCRIPTION[data.table(term = x),
				list(conceptId), on = 'term']
		}
	} else {
		matched <- rep(FALSE, nrow(SNOMED$DESCRIPTION))
		for (i in x){
			matched <- matched | grepl(i, SNOMED$DESCRIPTION$term)
		}
		if (inactiveIncluded(SNOMED)){
			MATCHED <- SNOMED$DESCRIPTION[matched, list(active, conceptId)]
		} else {
			MATCHED <- SNOMED$DESCRIPTION[matched, list(conceptId)]
		}
	}
	
	# Limit to active descriptions if needed
	if (active_only & inactiveIncluded(SNOMED)){
		MATCHED <- MATCHED[active == TRUE]
	}
	
	# Limit to active concepts
	if (active_only){
		if (inactiveIncluded(SNOMED)){
			out <- SNOMED$CONCEPT[MATCHED[, list(id = conceptId)],
				on = 'id'][active == TRUE]$id
		} else {
			out <- merge(SNOMED$CONCEPT, MATCHED[, list(id = conceptId)],
				by = 'id')$id
		}
		if (unique){
			out <- unique(out)
		}
	} else {
		if (unique){
			out <- unique(MATCHED$conceptId)
		} else {
			out <- MATCHED$conceptId
		}
	}
	class(out) <- c('SNOMEDconcept', 'integer64')
	return(out)
}

#' @rdname SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
as.SNOMEDconcept <- function(x, ...){
	if (is.SNOMEDconcept(x)){
		return(x)
	} else {
		return(SNOMEDconcept(x, ...))
	}
}

#' Check if an object is a SNOMEDconcept
#'
#' SNOMEDconcept is an S3 class for vectors of SNOMED concept IDs
#' as 64-bit integers. This function checks whether the object has
#' the class SNOMEDconcept and is a vector of 64-bit integers.
#'
#' @param x object to check
#' @return a logical vector of length one: TRUE or FALSE
#' @family SNOMEDconcept functions
#' @export
is.SNOMEDconcept <- function(x){
	if (identical(class(x), c('SNOMEDconcept', 'integer64'))){
		TRUE
	} else {
		FALSE
	}
}

#' Display a SNOMEDconcept object with descriptions
#'
#' SNOMEDconcept is an S3 class for vectors of SNOMED concept IDs
#' as 64-bit integers. This function checks whether the object has
#' the class SNOMEDconcept and is a vector of 64-bit integers.
#'
#' @param x SNOMEDconcept object, or something that can be
#'   coerced to one
#' @param ... not required
#' @return invisibly returns a character vector of the SNOMED CT
#'   concepts with descriptions separated by pipe (|)
#' @family SNOMEDconcept functions
#' @method print SNOMEDconcept
#' @export
print.SNOMEDconcept <- function(x, ...){
	SNOMED <- NULL
	try(SNOMED <- getSNOMED(), silent = TRUE)
	
	if (length(x) > 0){
		if (is.null(SNOMED)){
			out <- as.SNOMEDconcept(x)
			class(out) <- 'integer64'
			methods::show(out)
			return(invisible(out))
		} else {
			x <- as.SNOMEDconcept(x, SNOMED = SNOMED)
			output <- paste0(x, ' | ', description(x, SNOMED = SNOMED)$term)
			
			truncateChar <- function (x, maxchar){
				convert <- nchar(x) > maxchar
				x[convert] <- paste0(substr(x[convert], 1, maxchar - 3), "...")
				x
			}
			
			methods::show(truncateChar(output, getOption("width") - 7))
			return(invisible(output))
		}
	} else {
		message('No SNOMED CT concepts\n')
		return(character(0))
	}
}

#' Obtain descriptions for a set of SNOMED CT terms
#'
#' Returns the descriptions matching a set of concept IDs from
#' a SNOMED dictionary
#'
#' @param conceptIds character or integer64 vector
#' @param include_synonyms whether to return all synonyms, or just the
#'    Fully Specified Name, ensuring just one row per concept (default)
#' @param active_only whether to include only active descriptions
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @return a data.table with the following columns: id, conceptId, type
#'   (only if include_synonyms = TRUE), term,
#'   active (only if active_only = FALSE)
#'
#' @seealso htmlCodelistHierarchy
#' @export
#' @examples
#' hf <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
#' description(hf, include_synonyms = FALSE, SNOMED = sampleSNOMED())
description <- function(conceptIds,
	include_synonyms = FALSE, active_only = TRUE,
	SNOMED = getSNOMED()){
	# Check that conceptIds is a vector of strings or integer64 values
	# FSN     '900000000000003001'
	# Synonym '900000000000013009'
	
	# Declare names used for non-standard evaluation for R CMD check
	id <- term <- active <- typeId <- conceptId <- NULL
	
	# Return an empty data.table if there is no input data
	if (length(conceptIds) == 0){
		if (include_synonyms == TRUE){
			return(data.table(id = integer64(0), conceptId = integer64(0),
				type = character(0), term = character(0)))
		} else {
			return(data.table(id = integer64(0), conceptId = integer64(0),
				term = character(0)))
		}
	}
	
	CONCEPTS <- data.table(conceptId = as.SNOMEDconcept(conceptIds),
		order = seq_along(conceptIds))
	TOMATCH <- data.table(conceptId = unique(CONCEPTS$conceptId))
	if (include_synonyms == FALSE){
		OUT <- SNOMED$DESCRIPTION[TOMATCH, on = 'conceptId'][
			typeId %in% bit64::as.integer64('900000000000003001')][,
			list(id = id[1], term = term[1]),
			by = list(conceptId, active)]
		setcolorder(OUT, c('id', 'conceptId', 'term', 'active'))
	} else {
		OUT <- SNOMED$DESCRIPTION[TOMATCH, on = 'conceptId'][,
			list(id, conceptId, type = ifelse(
			typeId %in% bit64::as.integer64('900000000000003001'),
			'Fully specified name', 'Synonym'), term, active)]
	}
	# Remove inactive terms if necessary
	if (active_only){
		if (inactiveIncluded(SNOMED)){
			OUT <- OUT[active == TRUE]
		}
		OUT[, active := NULL]
	}
	# Restore original order
	OUT <- OUT[CONCEPTS, on = 'conceptId', allow.cartesian = TRUE]
	data.table::setkeyv(OUT, 'order')
	OUT[, order := NULL]
	OUT[]
}

#' Extract acronyms stated in the description of SNOMED CT concepts
#'
#' Returns acronyms, if any, expressed within SNOMED CT descriptions
#' in the form 'ABCD - Another Bland Cardiovascular Disease'.
#'
#' @param conceptIds character or integer64 vector
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @return a data.table with the following columns: id, conceptId,
#'   type = 'Acronym', term = acronym
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#' acronyms('Heart failure')
#'
#' # Get all synonyms and acronyms
#' rva <- SNOMEDconcept('Right ventricular abnormality')
#' rbind(description(rva, include_synonyms = TRUE), acronyms(rva))
acronyms <- function(conceptIds, SNOMED = getSNOMED()){
	D <- description(conceptIds, SNOMED = SNOMED, include_synonyms = TRUE)
	terms <- D$term
	
	# Symbols to declare to avoid R check errors
	id <- conceptId <- NULL

	acronym <- rep(NA_character_, length(terms))
	words <- strsplit(terms, split = ' ')
	maybe_acronym <- sapply(words, length) >= 3 & terms %like%
		'^([[:alnum:]]+) - '
	stated_acronym <- strsplit(sapply(words, function(x) tolower(x[1])),
		split = character(0))
	stated_expansion <- tolower(sub('^([[:alnum:]]+) - (.*)$', '\\2',
		terms))
	created_acronym <- lapply(words, function(x){
		tolower(substr(x[3:length(x)], 1, 1))
	})
	
	if (any(maybe_acronym)){
		for (i in seq_along(acronym)[maybe_acronym == TRUE]){
			# 1. Check for exact match
			if (identical(stated_acronym[[i]], created_acronym[[i]])){
				acronym[i] <- words[[i]][1]
			# 2. Check for exact match for first part of the phrase
			#    e.g. 'ica - internal carotid artery stenosis'
			} else if (identical(stated_acronym[[i]],
				created_acronym[[i]][1:length(stated_acronym[[i]])])){
				acronym[i] <- paste(words[[i]][c(1,
					(length(stated_acronym[[i]]) + 3):length(words[[i]]))],
					collapse = ' ')
			# 4. Allow leeway in match if acronym is longer than 3 characters
			} else if (length(stated_acronym[[i]]) > 3 &
				length(created_acronym[[i]]) > 2 & (stated_acronym[[i]][1] ==
				created_acronym[[i]][1])) {
				# allow up to one extra character in stated_acronym 
				matches <- unlist(sapply(2:length(stated_acronym[[i]]),
					function(x){
						ifelse(identical(stated_acronym[[i]][-x],
							created_acronym[[i]]), TRUE, FALSE)
					}))
				if (any(matches)){
					acronym[i] <- words[[i]][1]
				}
			} else {
				# no acronym
			}
		}
	}
	D[!is.na(acronym), list(id, conceptId, type = 'Acronym',
		term = acronym[!is.na(acronym)])]
}

#' Set operations for SNOMEDconcept vectors
#'
#' The default set functions in the base package do not handle integer64
#' vectors correctly, so this package also provides new generic functions
#' for union, intersect and setdiff, which enable the appropriate
#' object-specific function to be called according to the class of the
#' vector. This means that SNOMEDconcept vectors will remain as 
#' SNOMEDconcept vectors when these functions are used.
#'
#' @param x SNOMEDconcept vector
#' @param y SNOMEDconcept vector, or an object that can be coerced
#'   to SNOMEDconcept by as.SNOMEDconcept
#' @return an integer64 vector of SNOMEDconcept class
#' @export
#' @examples
#' sys_acute <- SNOMEDconcept(c('Systolic heart failure',
#'   'Acute heart failure'), SNOMED = sampleSNOMED())
#' acute_left_right <- SNOMEDconcept(c('Acute heart failure',
#'   'Left heart failure', 'Right heart failure'),
#'   SNOMED = sampleSNOMED())
#' union(sys_acute, acute_left_right) 
#' intersect(sys_acute, acute_left_right)
#' setdiff(sys_acute, acute_left_right)
union.SNOMEDconcept <- function(x, y){
	out <- unique(c(x, as.SNOMEDconcept(y)))
	class(out) <- c('SNOMEDconcept', 'integer64')
	out
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
union <- function (x, y){
	UseMethod('union', x)
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @method union default
#' @export
union.default <- function(x, y){
	base::union(x, y)
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @method intersect SNOMEDconcept
#' @export
intersect.SNOMEDconcept <- function(x, y){
	if (length(y) == 0 | length(x) == 0){
		out <- bit64::integer64(0)
	} else {
		out <- unique(x[x %in% as.SNOMEDconcept(y)])
	}
	class(out) <- c('SNOMEDconcept', 'integer64')
	out
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
intersect <- function (x, y){
	UseMethod('intersect', x)
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @method intersect default
#' @export
intersect.default <- function(x, y){
	base::intersect(x, y)
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @method setdiff SNOMEDconcept
#' @export
setdiff.SNOMEDconcept <- function(x, y){
	if (length(y) == 0 | length(x) == 0){
		out <- x
	} else {
		out <- unique(x[!(x %in% as.SNOMEDconcept(y))])
	}
	class(out) <- c('SNOMEDconcept', 'integer64')
	out
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
setdiff <- function(x, y){
	UseMethod('setdiff', x)
}

#' @rdname union.SNOMEDconcept
#' @family SNOMEDconcept functions
#' @method setdiff default
#' @export
setdiff.default <- function(x, y){
	base::setdiff(x, y)
}

#' Concatenate vectors of SNOMED CT concepts
#'
#' SNOMEDconcept is an S3 class for vectors of SNOMED concept IDs
#' as 64-bit integers. This function concatenates two or more
#' SNOMEDconcept vectors.
#'
#' @param ... SNOMEDconcept vectors
#' @return concatenation of vectors
#' @family SNOMEDconcept functions
#' @method c SNOMEDconcept
#' @export
#' @examples
#' hf <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
#' hf2 <- c(hf, hf)
c.SNOMEDconcept <- function(...){
	out <- bit64::c.integer64(...)
	if (bit64::is.integer64(out)){
		class(out) <- c('SNOMEDconcept', 'integer64')
	}
	out
}

#' Unique vector of SNOMED CT concepts
#'
#' SNOMEDconcept is an S3 class for vectors of SNOMED concept IDs
#' as 64-bit integers. This function returns a vector containing only
#' unique SNOMEDconcept values.
#'
#' @param x SNOMEDconcept vector
#' @param ... other variables to pass on to the underlying 'unique' function
#' @return SNOMEDconcept vector with duplicates removed
#' @family SNOMEDconcept functions
#' @method unique SNOMEDconcept
#' @export
#' @examples
#' hf <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
#' hf2 <- c(hf, hf)
#' unique(hf2)
unique.SNOMEDconcept <- function(x, ...){
	class(x) <- 'integer64'
	out <- bit64::unique.integer64(x, ...)
	class(out) <- c('SNOMEDconcept', 'integer64')
	out
}

