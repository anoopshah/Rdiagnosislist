#' Convert a data.frame to a SNOMEDcodelist object
#'
#' SNOMEDcodelist is an S3 class for lists of SNOMED codes.
#' It consists of conceptId and include_desc columns. The 
#' option to include descendants allows the creation of more succinct
#' SNOMED codelists.
#' 
#' Input is a data.frame or data.table with column names 'conceptId'
#' and optionally 'include_desc', which is FALSE by default, but if
#' TRUE then the codelist automatically includes all descendants of that
#' concept.
#'
#' as.SNOMEDcodelist converts its argument into a SNOMEDcodelist but
#'   leaves it unchanged if it is already a SNOMEDcodelist.
#'
#' @param x vector of SNOMED CT concept IDs, something which can
#'   be coerced to a SNOMEDconcept object, or a data.frame with
#'   a column 'conceptId' containing SNOMED CT concept concept IDs
#'   in integer64 or text format and optional column 'include_desc'
#'   (Boolean) stating whether descendants of the term should be
#'   included.
#' @param include_desc Boolean vector stating whether descendants
#'   are included, recycled if necessary. Default = TRUE.
#'   Ignored if 
#' @param SNOMED environment containing a SNOMED dictionary
#' @param ... other arguments to pass to SNOMEDcodelist
#' @return An object of class 'SNOMEDcodelist'
#' @family SNOMEDcodelist functions
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' my_concepts <- SNOMEDconcept('Heart failure')
#' SNOMEDcodelist(my_concepts)
#' SNOMEDcodelist(data.frame(conceptId = my_concepts))
#' as.SNOMEDcodelist(data.frame(conceptId = my_concepts,
#'   include_desc = TRUE))
SNOMEDcodelist <- function(x, include_desc = TRUE,
	SNOMED = get('SNOMED', envir = globalenv())){
	term <- conceptId <- NULL
	
	if (!is.list(x)){
		conceptIds <- unique(as.SNOMEDconcept(x, SNOMED = SNOMED))
		conceptIds <- conceptIds[!is.na(conceptIds)]
		# remove any missing concepts
		message(paste0('Converting ', length(conceptIds),
			' concept(s) to a codelist'))
		x <- data.table(conceptId = conceptIds,
			include_desc = include_desc)
	}
	if (!is.data.frame(x)){
		stop('x must be a data.frame')
	}
	x <- as.data.table(x)
	if (!('conceptId' %in% names(x))){
		stop('the SNOMED conceptId must be in a column named conceptId')
	}
	x[, conceptId := as.SNOMEDconcept(conceptId, SNOMED = SNOMED)]
	if ('include_desc' %in% names(x)){
		x[, include_desc := as.logical(include_desc)]
	} else {
		global_include_desc <- include_desc
		x[, include_desc := global_include_desc]
	}
	if (!('term' %in% names(x))){
		# Add SNOMED terms (fully specified names)
		x[, term := description(x$conceptId, SNOMED = SNOMED)$term]
	}
	data.table::setattr(x, 'class', c('SNOMEDcodelist', 'data.table', 'data.frame'))
	data.table::setattr(x, 'Expanded', FALSE)
	data.table::setkeyv(x, 'conceptId')
	x
}

#' @rdname SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
as.data.frame.SNOMEDconcept <- function(x, optional = NULL,
	stringsAsFactors = NULL){
	data.table::setattr(x, 'class', 'integer64')
	bit64::as.data.frame.integer64(x)
}

#' @rdname SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
as.integer64.SNOMEDconcept <- function(x){
	data.table::setattr(x, 'class', 'integer64')
	bit64::as.integer64(x)
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
#' SNOMEDcodelist is an S3 class for sets of SNOMED concepts.
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
#' my_concepts <- SNOMEDconcept('Heart failure')
#' my_codelist <- SNOMEDcodelist(data.frame(conceptId = my_concepts,
#'   include_desc = TRUE))
#' expanded_codelist <- expandSNOMED(my_codelist)
#' contractSNOMED(expanded_codelist)
expandSNOMED <- function(x, SNOMED = get('SNOMED', envir = globalenv())){
	# Adds descendants of terms marked include_desc = TRUE
	# Terms are added with include_desc = NA, which shows that they
	# were automatically added, and can be removed by contractSNOMED
	
	include_desc <- NULL
	
	if (!is.SNOMEDcodelist(x)){
		stop('x must be a SNOMEDcodelist')
	}
	if (attr(x, 'Expanded') == TRUE){
		return(x)
	}
	# Otherwise perform the expansion
	desc_conceptIds <- descendants(x[include_desc == TRUE]$conceptId,
		SNOMED = SNOMED)
	x <- rbind(x, data.table(conceptId = desc_conceptIds,
		term = description(desc_conceptIds, SNOMED = SNOMED)$term,
		include_desc = as.logical(NA)))
	# Restore SNOMEDcodelist class
	data.table::setattr(x, 'class', c('SNOMEDcodelist', 'data.table', 'data.frame'))
	data.table::setattr(x, 'Expanded', TRUE)
	data.table::setkeyv(x, 'conceptId')
	x
}

#' @rdname expandSNOMED
#' @family SNOMEDcodelist functions
#' @export
contractSNOMED <- function(x){
	# Remove terms with include_desc = NA
	
	# Declare names to be used for non-standard evaluation for R CMD check
	include_desc <- NULL
	
	if (!is.SNOMEDcodelist(x)){
		stop('x must be a SNOMEDcodelist')
	}
	if (attr(x, 'Expanded') == FALSE){
		return(x)
	}
	x <- x[!is.na(include_desc)]
	data.table::setattr(x, 'Expanded', FALSE)
	data.table::setkeyv(x, 'conceptId')
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
