as.SNOMEDcodelist <- function(x){
	# Creates a SNOMED codelist consisting of conceptId and include_children columns
	# Option to include children allows the creation of more succinct SNOMED codelists
	# Input is a data.frame or data.table with column names 'conceptId' and 'include_children'
	if (!is.data.frame(x)){
		stop('x must be a data.frame')
	}
	x <- as.data.table(x)
	if (!('conceptId' %in% names(x))){
		stop('the SNOMED conceptId must be in a column named conceptId')
	}
	if (class(x$conceptId) == 'character'){
		x[, conceptId := as.integer64(conceptId)]
	} else if (class(x$conceptId) == 'integer64'){
		# correct format
	} else {
		stop('conceptId must be supplied in character or integer64 format; ',
			class(x$conceptId), 'is not acceptable.')
	}
	if ('include_children' %in% names(x)){
		x[, include_children := as.logical(include_children)]
		setattr(x, 'expanded', FALSE)
	} else {
		x[, include_children := as.logical(FALSE)]
		setattr(x, 'expanded', TRUE)
	}
	if (!('term' %in% names(x))){
		# Add SNOMED terms (fully specified names)
		
	}
	setattr(x, 'expanded', TRUE)
	class(x) <- c('data.frame', 'data.table', 'SNOMEDcodelist')
}

expand.SNOMEDcodelist <- function(x){
	# Adds children of terms marked 'include_children'
	if (!is.SNOMEDcodelist(x)){
		stop('x must be a SNOMEDcodelist')
	}
	if (attr(x, 'expanded') == TRUE){
		return(x)
	}
	# Otherwise perform the expansion
	parents <- parents(x[include_children == TRUE]$conceptId)
	x
}

is.SNOMEDcodelist <- function(x){
	if (identical(class(x), c('data.frame', 'data.table', 'SNOMEDcodelist'))){
		TRUE
	}
}

contract.SNOMEDcodelist <- function(x){
	# Checks how many SNOMED terms can be included in parents
	# and includes only additional explicit terms as necessary

	setattr(x, 'expanded', FALSE)
	x	
}
