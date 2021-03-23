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
inactiveIncluded <- function(SNOMED = get('SNOMED', envir = globalenv())){
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
#' converts a character or integer64 vector to a SNOMEDconcept
#' object.
#'
#' @param terms character vector of terms to match, or
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
#' @export
#' @examples
#' SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED()) -> hf
#' is.SNOMEDconcept(hf)
#' SNOMEDconcept('900000000000003001')
#' as.SNOMEDconcept('900000000000003001')
SNOMEDconcept <- function(terms, active_only = TRUE,
	exact_match = TRUE, unique = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	# Declare names to be used for non-standard evaluation for R CMD check
	active <- NULL
	
	if ('integer64' %in% class(terms)){
		# correct format
		out <- terms
		setattr(terms, 'class', c('SNOMEDconcept', 'integer64'))
		return(terms)
	} else if ('character' %in% class(terms)){
		out <- terms
		if (all(grepl('^[0-9]+$', terms))){
			# concept ID in character format 
			terms <- as.integer64(terms)
			setattr(terms, 'class', c('SNOMEDconcept', 'integer64'))
			return(terms)
		}
	} else {
		stop('conceptId must be supplied in character or integer64 format; ',
			class(conceptIds), ' is not acceptable.')
	}

	# Try to match a term description
	if (exact_match){
		MATCHED <- SNOMED$DESCRIPTION[data.table(term = terms),
			list(active, conceptId), on = 'term']
	} else {
		matched <- rep(FALSE, nrow(SNOMED$DESCRIPTION))
		for (x in terms){
			matched <- matched | grepl(x, SNOMED$DESCRIPTION$term)
		}
		MATCHED <- SNOMED$DESCRIPTION[matched, list(active, conceptId)]
	}
	
	# Limit to active descriptions if needed
	if (active_only & inactiveIncluded(SNOMED)){
		MATCHED <- MATCHED[active == TRUE]
	}
	
	# Limit to active concepts
	if (active_only){
		if (unique){
			out <- unique(SNOMED$CONCEPT[MATCHED[, list(id = conceptId)], on = 'id'][
				active == TRUE]$id)
		} else {
			out <- SNOMED$CONCEPT[MATCHED[, list(id = conceptId)], on = 'id'][
				active == TRUE]$id
		}
	} else {
		if (unique){
			out <- unique(MATCHED$conceptId)
		} else {
			out <- MATCHED$conceptId
		}
	}
	setattr(out, 'class', c('SNOMEDconcept', 'integer64'))
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
#'    coerced to one
#' @param x object to check
#' @return a logical vector of length one: TRUE or FALSE
#' @family SNOMEDconcept functions
#' @method print SNOMEDconcept
#' @export
print.SNOMEDconcept <- function(x, ...){
	SNOMED <- NULL
	try(SNOMED <- get('SNOMED', envir = globalenv()), silent = TRUE)
	
	if (length(x) > 0){
		if (is.null(SNOMED)){
			out <- as.SNOMEDconcept(x)
			setattr(out, 'class', 'integer64')
			show(out)
			return(invisible(out))
		} else {
			x <- as.SNOMEDconcept(x, SNOMED = SNOMED)
			output <- paste0(x, ' | ', description(x, SNOMED = SNOMED)$term)
			
			truncateChar <- function (x, maxchar){
				convert <- nchar(x) > maxchar
				x[convert] <- substr(x[convert], 1, maxchar - 3) %&% "..."
				x
			}
			
			show(truncateChar(output, getOption("width") - 7))
			return(invisible(output))
		}
	} else {
		cat('No SNOMED CT concepts\n')
		return(character(0))
	}
}

#' Obtain descriptions for a set of SNOMED CT terms
#'
#' Returns the descriptions matching a set of concept IDs from
#' a SNOMED dictionary
#'
#' @param conceptIds character or integer64 vector
#' @param include_synonyms whether to return only the Fully Specified
#'    Name (default) or all synonyms
#' @param active_only whether to include only active descriptions
#' @param SNOMED environment containing SNOMED dictionary. Defaults
#'   to an object named 'SNOMED' in the global environment
#' @return a data.table with the following columns: id, conceptId, type
#'   (only if include_synonyms = TRUE), term,
#'   active (only if active_only = FALSE)
#' @export
#' @examples
#' hf <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
#' description(hf, include_synonyms = FALSE, SNOMED = sampleSNOMED())
description <- function(conceptIds,
	include_synonyms = FALSE, active_only = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	# Check that conceptIds is a vector of strings or integer64 values
	# FSN     '900000000000003001'
	# Synonym '900000000000013009'
	
	# Declare names used for non-standard evaluation for R CMD check
	id <- term <- active <- typeId <- NULL
	
	CONCEPTS <- data.table(conceptId = as.SNOMEDconcept(conceptIds),
		order = seq_along(conceptIds))
	TOMATCH <- data.table(conceptId = unique(CONCEPTS$conceptId))
	if (include_synonyms == FALSE){
		OUT <- SNOMED$DESCRIPTION[TOMATCH, on = 'conceptId'][
			typeId == as.integer64('900000000000003001')][,
			list(id, conceptId, term, active)]
	} else {
		OUT <- SNOMED$DESCRIPTION[TOMATCH, on = 'conceptId'][,
			list(id, conceptId,
			type = ifelse(typeId == as.integer64('900000000000003001'),
			'Fully specified name', 'Synonym'), term, active)]
	}
	# Restore original order
	OUT <- OUT[CONCEPTS, on = 'conceptId']
	setkey(OUT, order)
	OUT[, order := NULL]
	# Remove inactive terms if necessary
	if (active_only){
		if (inactiveIncluded(SNOMED)){
			OUT <- OUT[active == TRUE]
		}
		OUT[, active := NULL]
	}
	OUT[]
}


