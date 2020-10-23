#' Convert a data.frame to a SNOMEDcodelist object
#'
#' SNOMEDcodelist is an S3 class for lists of SNOMED codes.
#' It consists of conceptId and include_children columns. The 
#' option to include children allows the creation of more succinct
#' SNOMED codelists.
#' Input is a data.frame or data.table with column names 'conceptId'
#' and optionally 'include_children'
#'
#' @param x data.frame to convert to a SNOMEDcodelist
#' @param SNOMED environment containing a SNOMED dictionary
#' @return An object of class 'SNOMEDcodelist'
#' @family SNOMEDcodelist functions
#' @aliases SNOMEDcodelist
#' @export
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- new.env()
#' data(CONCEPT, envir = TEST)
#' data(DESCRIPTION, envir = TEST)
#' data(RELATIONSHIP, envir = TEST)
#' data(STATEDRELATIONSHIP, envir = TEST) 
#'
#' my_concepts <- conceptId('Heart failure', SNOMED = TEST)
#' as.SNOMEDcodelist(data.table(conceptId = my_concepts), SNOMED = TEST)
#' as.SNOMEDcodelist(data.table(conceptId = my_concepts,
#'   include_children = TRUE), SNOMED = TEST)
as.SNOMEDcodelist <- function(x, SNOMED = get('SNOMED', envir = globalenv())){
	if (!is.data.frame(x)){
		stop('x must be a data.frame')
	}
	x <- as.data.table(x)
	if (!('conceptId' %in% names(x))){
		stop('the SNOMED conceptId must be in a column named conceptId')
	}
	x[, conceptId := checkConcepts(conceptId)]
	if ('include_children' %in% names(x)){
		x[, include_children := as.logical(include_children)]
		setattr(x, 'Expanded', FALSE)
	} else {
		x[, include_children := as.logical(FALSE)]
		setattr(x, 'Expanded', TRUE)
	}
	if (!('term' %in% names(x))){
		# Add SNOMED terms (fully specified names)
		x[, term := description(x$conceptId, SNOMED = SNOMED)$term]
	}
	class(x) <- c('data.frame', 'data.table', 'SNOMEDcodelist')
	x
}

#' Expand or contract a SNOMEDcodelist
#'
#' SNOMEDcodelist is an S3 class for lists of SNOMED codes.
#' In the 'contracted' form, it may contain only parents and not
#' child terms (to create a more succinct list). The 'Expanded'
#' form contains all concepts.
#'
#' @param x SNOMEDcodelist to expand or contract
#' @param SNOMED environment containing a SNOMED dictionary
#' @return An object of class 'SNOMEDcodelist' with attribute
#'    Expanded = TRUE
#' @family SNOMEDcodelist functions
#' @export
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- new.env()
#' data(CONCEPT, envir = TEST)
#' data(DESCRIPTION, envir = TEST)
#' data(RELATIONSHIP, envir = TEST)
#' data(STATEDRELATIONSHIP, envir = TEST) 
#'
#' my_concepts <- conceptId('Heart failure', SNOMED = TEST)
#' my_codelist <- as.SNOMEDcodelist(data.table(conceptId = my_concepts,
#'   include_children = TRUE), SNOMED = TEST)
#' expanded_codelist <- expandSNOMED(my_codelist)
#' contract(expanded_codelist)
expandSNOMED <- function(x, SNOMED = get('SNOMED', envir = globalenv())){
	# Adds children of terms marked 'include_children'
	if (!is.SNOMEDcodelist(x)){
		stop('x must be a SNOMEDcodelist')
	}
	if (attr(x, 'Expanded') == TRUE){
		return(x)
	}
	# Otherwise perform the expansion
	children_conceptIds <- children(x[include_children == TRUE]$conceptId)
	x <- rbind(x, data.table(conceptId = children_conceptIds,
		term = description(children_conceptIds, SNOMED)$term,
		include_children = NA_logical_))
	x
}

#' @rdname expandSNOMED
#' @export
contractSNOMED <- function(x, SNOMED = get('SNOMED', envir = globalenv())){
	# Checks how many SNOMED terms can be included in parents
	# and includes only additional explicit terms as necessary
	children_conceptIds <- x[include_children == FALSE]$conceptIds
	nonchildren_conceptIds <- x[is.na(include_children) |
		include_children == TRUE]$conceptIds
	setattr(x, 'Expanded', FALSE)
	x
}

#' Check if an object is a SNOMEDcodelist
#'
#' SNOMEDcodelist is an S3 class for lists of SNOMED codes.
#' This function checks whether the object has the class
#' SNOMEDcodelist. It does not check whether it contains valid data.
#'
#' @param x object to check
#' @return a logical vector of length one: TRUE or FALSE
#' @family SNOMEDcodelist functions
#' @export
is.SNOMEDcodelist <- function(x){
	if (identical(class(x), c('data.frame', 'data.table', 'SNOMEDcodelist'))){
		TRUE
	} else {
		FALSE
	}
}
