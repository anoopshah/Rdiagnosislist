#' Convert a data.frame to a SNOMEDcodelist object
#'
#' SNOMEDcodelist is an S3 class for lists of SNOMED codes.
#' It consists of conceptId and include_desc columns. The 
#' option to include descendants allows the creation of more succinct
#' SNOMED codelists.
#' Input is a data.frame or data.table with column names 'conceptId'
#' and optionally 'include_desc'
#'
#' as.SNOMEDcodelist converts its argument into a SNOMEDcodelist but
#'   leaves it unchanged if it is already a SNOMEDcodelist.
#'
#' @param x data.frame or data.table to convert to a SNOMEDcodelist.
#'   It must have a column named 'conceptId'
#' @param SNOMED environment containing a SNOMED dictionary
#' @return An object of class 'SNOMEDcodelist'
#' @family SNOMEDcodelist functions
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' my_concepts <- conceptId('Heart failure')
#' SNOMEDcodelist(data.table(conceptId = my_concepts))
#' as.SNOMEDcodelist(data.table(conceptId = my_concepts,
#'   include_desc = TRUE))
SNOMEDcodelist <- function(x, SNOMED = get('SNOMED', envir = globalenv())){
	if (!is.data.frame(x)){
		stop('x must be a data.frame')
	}
	x <- as.data.table(x)
	if (!('conceptId' %in% names(x))){
		stop('the SNOMED conceptId must be in a column named conceptId')
	}
	x[, conceptId := checkConcepts(conceptId)]
	if ('include_desc' %in% names(x)){
		x[, include_desc := as.logical(include_desc)]
		setattr(x, 'Expanded', FALSE)
	} else {
		x[, include_desc := as.logical(FALSE)]
		setattr(x, 'Expanded', TRUE)
	}
	if (!('term' %in% names(x))){
		# Add SNOMED terms (fully specified names)
		x[, term := description(x$conceptId, SNOMED = SNOMED)$term]
	}
	class(x) <- c('SNOMEDcodelist', 'data.table', 'data.frame')
	setkey(x, conceptId)
	x
}

#' @rdname SNOMEDcodelist
#' @family SNOMEDcodelist functions
#' @export
as.SNOMEDcodelist <- function(x, ...){
	if (is.SNOMEDcodelist(x)){
		return(x)
	} else {
		SNOMEDcodelist(x, ...)
	}
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
#' SNOMED <- sampleSNOMED()
#'
#' my_concepts <- conceptId('Heart failure')
#' my_codelist <- SNOMEDcodelist(data.table(conceptId = my_concepts,
#'   include_desc = TRUE))
#' expanded_codelist <- expandSNOMED(my_codelist)
#' contractSNOMED(expanded_codelist)
expandSNOMED <- function(x, SNOMED = get('SNOMED', envir = globalenv())){
	# Adds descendants of terms marked include_desc = TRUE
	# Terms are added with include_desc = NA, which shows that they
	# were automatically added, and can be removed by contractSNOMED
	if (!is.SNOMEDcodelist(x)){
		stop('x must be a SNOMEDcodelist')
	}
	if (attr(x, 'Expanded') == TRUE){
		return(x)
	}
	# Otherwise perform the expansion
	desc_conceptIds <- descendants(x[include_desc == TRUE]$conceptId)
	x <- rbind(x, data.table(conceptId = desc_conceptIds,
		term = description(desc_conceptIds, SNOMED = SNOMED)$term,
		include_desc = as.logical(NA)))
	# Restore SNOMEDcodelist class
	class(x) <- c('SNOMEDcodelist', 'data.table', 'data.frame')
	setattr(x, 'Expanded', TRUE)
	setkey(x, conceptId)
	x
}

#' @rdname expandSNOMED
#' @family SNOMEDcodelist functions
#' @export
contractSNOMED <- function(x, SNOMED = get('SNOMED', envir = globalenv())){
	# Remove terms with include_desc = NA
	if (!is.SNOMEDcodelist(x)){
		stop('x must be a SNOMEDcodelist')
	}
	if (attr(x, 'Expanded') == FALSE){
		return(x)
	}
	x <- x[!is.na(include_desc)]
	setattr(x, 'Expanded', FALSE)
	setkey(x, conceptId)
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
	if (identical(class(x), c('SNOMEDcodelist', 'data.table', 'data.frame'))){
		TRUE
	} else {
		FALSE
	}
}
