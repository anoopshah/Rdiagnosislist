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
#'   Ignored if x contains a column 'include_desc'
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
#' form contains all concepts. The output of 'showCodelistHierarchy'
#' includes all hierarchies contained within the codelist in a 
#' format suitable for display.
#'
#' @param x SNOMEDcodelist to expand or contract
#' @param SNOMED environment containing a SNOMED dictionary
#' @param show_excluded_descendants (logical) whether to show excluded
#'   descendants of terms in the codelist, with an 'included' column
#'   stating which terms are included. This can make it easy to see
#'   if a codelist is consistent with the SNOMED CT ontology
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
expandSNOMED <- function(x, SNOMED = getSNOMED()){
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
contractSNOMED <- function(x, SNOMED = getSNOMED()){
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

#' @rdname expandSNOMED
#' @family SNOMEDcodelist functions
#' @export
showCodelistHierarchy <- function(x, SNOMED = getSNOMED(),
	show_excluded_descendants = TRUE){
	# Contract a SNOMED CT codelist, show the terms in a 
	# sensible order and optionally show terms not included
	# that are descendants of included terms 
	
	# For debugging
	debug <- FALSE
	
	# Declare names to be used for R CMD check
	include_desc <- conceptId <- .keep <- NULL
	included <- gen <- parentId <- roworder <- parentrowid <- NULL
	duplicate <- childrowid <- descendantrowid <- NULL
	
	out <- expandSNOMED(data.table::copy(x), SNOMED = SNOMED)
	out[, included := TRUE]
	out <- out[!duplicated(out)] # remove duplicates of entire rows
	
	# Check only one row per concept
	if (any(duplicated(out$conceptId))){
		stop('Codelist must have only one row per concept')
	}
	
	if (show_excluded_descendants == TRUE){
		desc <- as.SNOMEDcodelist(setdiff(
			descendants(out$conceptId, SNOMED = SNOMED),
			out$conceptId), include_desc = FALSE)
		if (nrow(desc) > 0){
			desc[, included := FALSE]
			out <- rbind(out, desc, fill = TRUE)
		}
	}
	
	# Identify top gen
	allchildren <- relatedConcepts(out$conceptId, reverse = TRUE,
		SNOMED = SNOMED)
	out[!(conceptId %in% allchildren), gen := 1]
	
	# Identify parentId of each term
	out[, parentId := list(bit64::integer64(0))]
	for (i in seq_along(out$conceptId)){
		out[i, parentId := list(parents(conceptId, SNOMED = SNOMED))]
	}
	
	# Sort by term
	data.table::setkeyv(out, 'term')
	
	# Add information about hierarchy
	thegen = 1
	maxgen <- 15
	out[, roworder := as.numeric(NA)]
	out[, rowid := 1L:.N]
	out[, parentrowid := NA_integer_] # rowid of parent
	out[gen == thegen, roworder := as.numeric(1:.N)]

	# Loop through max 15 gens
	# Each row has a unique rowid
	# Concepts might need to be duplicated if they are in multiple
	# locations in the hierarchy.
	while (thegen < maxgen){
		if (debug) {cat(thegen, maxgen, '\n')}
		thisgenrowids <- out[gen == thegen]$rowid
		if (length(thisgenrowids) > 0){
			# Looping each row (instance of a concept) in generation
			# Find children of this concept and create
			# appropriate rows for descendant hierarchy
			for (thisrowid in thisgenrowids){
				# Find out which concept
				thisconcept <- out[rowid == thisrowid]$conceptId
				thisroworder <- out[rowid == thisrowid]$roworder
				
				if (debug == TRUE){
					if (length(thisconcept) == 0) {
						cat('\nthisconcept is integer64(0)\n')
						browser()
					}
				}
				
				# For this concept, find all children
				childrows <- sapply(out$parentId, function(x){
					if (length(x) == 0){
						FALSE
					} else {
						thisconcept %in% x
					}
				})

				# Copy children rows if they already have a roworder
				# (i.e. already located in the hierarchy) 
				# as they need to be duplicated
				tocopy <- out[childrows & !is.na(roworder)]

				if (nrow(tocopy) > 0){
					# Keep only one copy of the child row per
					# concept ID, 
					tocopy[, .keep := (rowid == min(rowid)),
						by = conceptId]
					tocopy <- tocopy[.keep == TRUE]
					tocopy[, .keep := NULL]
					maxrowid <- max(out$rowid)
					# Change the rowid for existing rows which have not
					# yet been allocated and have the same rowid
					
					out[rowid %in% tocopy$rowid,
						rowid := (1:.N) + maxrowid]
					# OLD: this would lead to some rowid being
					# removed unnecessarily
					# out[childrows & !is.na(roworder),
					#	rowid := (1:.N) + maxrowid]
					# FIXME: Sometimes there is an error
					# if a rowid is removed, then in the 
					# next loop thisconcept and thisroworder
					# have zero length
				}
				
				out[childrows, roworder := thisroworder +
					(1:sum(childrows)) / (sum(childrows) + 1)]
				out[childrows, parentrowid := thisrowid]
				out[childrows, gen := thegen + 1]
				
				if (nrow(tocopy) > 0){
					out <- rbind(out, tocopy)
				}
				
				data.table::setkeyv(out, c('roworder', 'term'))
				out[!is.na(roworder), roworder := 1:.N]
				
				# Debugging section
				if (debug == TRUE){
					if (thegen == 6){
						cat('\nthisconcept', thisconcept)
						cat(description(thisconcept)$term)
						cat('\n\nthegen', thegen)
						cat('\nthisgenrowids', thisgenrowids)
						cat('\nthisroworder', thisroworder)
						cat('\nthisrowid', thisrowid)
						cat('\ntocopy\n')
						print(tocopy)
						browser()
					}
				}
			}
			thegen <- thegen + 1
		} else {
			thegen <- maxgen
		}
	}

	# Sort by roworder
	data.table::setkeyv(out, 'roworder')

	# Final deduplication of adjacent entries
	out[, duplicate := conceptId == shift(conceptId) &
		gen == shift(gen) & 
		parentrowid == shift(parentrowid)]
	out[1, duplicate := FALSE]
	out <- out[duplicate == FALSE]
	out[, duplicate := NULL]

	# Add information about row grouping so that the
	# html view can have buttons to show/hide rows
	# i.e. - contract all descendants
	#      - expand children (immediate descendants only)
	out[, childrowid := list(integer(0))]
	out[, descendantrowid := list(integer(0))]
	out[, allthisrowid := list(integer(0))]
	out[, alldescendantrowid := list(integer(0))]
	for (i in seq_along(out$rowid)){
		thisrowid <- out[i]$rowid
		thisconcept <- out[i]$conceptId
		out[i, childrowid := list(out[parentrowid == thisrowid]$rowid)]
		out[i, allthisrowid := list(out[conceptId ==
			thisconcept]$rowid)] # all rows containing self
		thisgen <- out[i]$gen
		fin <- min(which(c(out$gen, 0) <= thisgen &
			c(seq_along(out$gen), Inf) > i)) - 1
		if (fin != Inf & fin > i){ 
			out[i, descendantrowid := list(out[(i + 1):fin]$rowid)]
		}
		# self and all descendants
		out[i, alldescendantrowid := list(c(out[conceptId ==
			thisconcept]$rowid, unlist(out[conceptId ==
			thisconcept]$descendantrowid)))]
	}
	# Need to add information about rows of all descendants
	# including those in other hierarchies so that term selection
	# based on trees can be done correctly

	# Sort by roworder
	data.table::setkeyv(out, 'roworder')

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

