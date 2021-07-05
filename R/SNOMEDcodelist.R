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
	SNOMED = getSNOMED()){
	term <- conceptId <- NULL
	
	if (!is.data.frame(x)){
		conceptIds <- SNOMEDconcept(x, SNOMED = SNOMED)
		message(paste0('Converting ', length(conceptIds),
			' concept(s) to a codelist'))
		x <- as.data.frame(conceptIds)
		names(x) <- 'conceptId'
	}
	if (!is.data.frame(x) | !('conceptId' %in% names(x))){
		stop('x must be a data.frame with a column named conceptId')
	}
	out <- as.data.table(x)
	if (!('conceptId' %in% names(out))){
		stop('the SNOMED conceptId must be in a column named conceptId')
	}
	out[, conceptId := as.SNOMEDconcept(conceptId, SNOMED = SNOMED)]
	if ('include_desc' %in% names(out)){
		out[, include_desc := as.logical(include_desc)]
	} else {
		global_include_desc <- include_desc
		out[, include_desc := global_include_desc]
	}
	if (!('term' %in% names(out))){
		# Add SNOMED terms (fully specified names)
		out[, term := description(out$conceptId, SNOMED = SNOMED)$term]
	}
	data.table::setcolorder(out, c('conceptId', 'include_desc', 'term'))
	data.table::setattr(out, 'class', c('SNOMEDcodelist', 'data.table', 'data.frame'))
	data.table::setattr(out, 'Expanded', FALSE)
	data.table::setkeyv(out, 'conceptId')
	out[]
}

#' @rdname SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
as.data.frame.SNOMEDconcept <- function(x, ...){
	class(x) <- 'integer64'
	bit64::as.data.frame.integer64(x, ...)
}

#' @rdname SNOMEDconcept
#' @family SNOMEDconcept functions
#' @export
as.integer64.SNOMEDconcept <- function(x){
	class(x) <- 'integer64'
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
expandSNOMED <- function(x,
	SNOMED = getSNOMED()){
	# Adds descendants of terms marked include_desc = TRUE
	# Terms are added with include_desc = NA, which shows that they
	# were automatically added, and can be removed by contractSNOMED
	
	# Declare names to be used for non-standard evaluation for R CMD check
	include_desc <- NULL
	
	if (!is.SNOMEDcodelist(x)){
		stop('x must be a SNOMEDcodelist')
	}
	if (!is.null(attr(x, 'Expanded'))){
		if (attr(x, 'Expanded') == TRUE){
			return(x)
		}
	}
	# Otherwise perform the expansion
	desc_conceptIds <- descendants(x[include_desc == TRUE]$conceptId,
		SNOMED = SNOMED)
	out <- rbind(x, data.table(conceptId = desc_conceptIds,
		include_desc = as.logical(rep(NA, length(desc_conceptIds))),
		term = description(desc_conceptIds, SNOMED = SNOMED)$term))
	# Restore SNOMEDcodelist class
	data.table::setcolorder(out, c('conceptId', 'include_desc', 'term'))
	data.table::setattr(out, 'class', c('SNOMEDcodelist', 'data.table', 'data.frame'))
	data.table::setattr(out, 'Expanded', TRUE)
	data.table::setkeyv(out, 'conceptId')
	out[]
}

#' @rdname expandSNOMED
#' @family SNOMEDcodelist functions
#' @export
contractSNOMED <- function(x,
	SNOMED = getSNOMED()){
	# Remove terms with include_desc = NA as long as they are a
	# descendant of a term with include_desc = TRUE
	
	# Declare names to be used for non-standard evaluation for R CMD check
	include_desc <- conceptId <- NULL
	out <- copy(x)
	
	if (!is.SNOMEDcodelist(out)){
		stop('x must be a SNOMEDcodelist')
	}
	if (!is.null(attr(out, 'Expanded'))){
		if (attr(out, 'Expanded') == FALSE){
			return(out)
		}
	}
	desclist <- out[include_desc == TRUE]$conceptId
	desclist <- union(desclist, descendants(desclist, SNOMED = SNOMED))
	if (length(desclist) > 0){
		toremove <- out[is.na(include_desc) & conceptId %in% desclist]
		out <- out[!toremove]
	}
	data.table::setcolorder(out, c('conceptId', 'include_desc', 'term'))
	data.table::setattr(out, 'Expanded', FALSE)
	data.table::setkeyv(out, 'conceptId')
	out[]
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
