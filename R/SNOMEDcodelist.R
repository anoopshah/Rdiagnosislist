#' Convert a data.frame to a SNOMEDcodelist object
#'
#' SNOMEDcodelist is an S3 class for lists of SNOMED CT  concepts.
#' It consists of conceptId and include_desc columns. The 
#' option to include descendants allows the creation of more succinct
#' SNOMED codelists.
#' 
#' Input is a data.frame or data.table with column names 'conceptId'
#' and optionally 'include_desc', which is FALSE by default, but if
#' TRUE then the codelist automatically includes all active descendants
#' of that concept.
#'
#' If the codelist is intended to contain inactive concepts, it can
#' only exist in the 'simple' format. Inactive concepts will be lost if
#' the codelist is converted between formats.
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
#'   are included, recycled if necessary. Default = FALSE.
#'   Ignored if x contains a column 'include_desc'
#' @param format Whether the codelist is expressed as a simple
#'   enumeration of concepts ('simple'), as a set of concept
#'   hierarchies ('tree'), or concept hierarchies showing all
#'   descendant terms ('exptree'). Codelists can be converted
#'   between the formats, but the result of conversion may depend on
#'   the SNOMED CT dictionary being used.
#' @param codelist_name Name of the codelist (character vector of length 1)
#' @param version Version of the codelist (character vector of length 1)
#' @param author Author of the codelist (character vector of length 1)
#' @param date Date attributed to the codelist (character vector of length 1)
#' @param SNOMED environment containing a SNOMED dictionary
#' @param show_excluded_descendants Whether to show excluded
#'   descendants alongside the codes included in the codelist (for
#'   a 'tree' or 'exptree' format codelist).
#' @param ... other arguments to pass to SNOMEDcodelist
#' @return An object of class 'SNOMEDcodelist'
#' @family SNOMEDcodelist functions
#' @seealso htmlCodelistHierarchy
#' @export
#' @examples
#' SNOMED <- sampleSNOMED()
#'
#' my_concepts <- SNOMEDconcept('Heart failure')
#' SNOMEDcodelist(my_concepts)
#' SNOMEDcodelist(data.frame(conceptId = my_concepts))
#' as.SNOMEDcodelist(data.frame(conceptId = my_concepts,
#'   include_desc = TRUE))
SNOMEDcodelist <- function(x, include_desc = FALSE,
	format = c('simple', 'tree', 'exptree'), codelist_name = NULL,
	version = NULL, author = NULL, date = NULL,
	SNOMED = getSNOMED(), show_excluded_descendants = FALSE){

	# Declare variables to be used in data.tables
	metadata <- included <- .dup <- typeId <- active <- NULL
	term <- conceptId <- NULL

	# Use the first element of format as the actual export format
	format <- format[1]

	# 1. GATHER CONCEPTS FOR CODELIST
	# If it is already a codelist, leave as is
	if (is.SNOMEDcodelist(x)){
		out <- data.table::copy(x)
		# Extract metadata if it exists
		if (is.null(codelist_name)) {codelist_name <- attr(out, 'codelist_name')}
		if (is.null(version)) {version <- attr(out, 'version')}
		if (is.null(author)) {author <- attr(out, 'author')}
		if (is.null(date)) {date <- attr(out, 'date')}
	} else {
		# Convert to SNOMEDcodelist
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
		out[, conceptId := as.SNOMEDconcept(conceptId,
			SNOMED = SNOMED)]
	}
	
	# Extract metadata from 'metadata' column if it exists
	if ('metadata' %in% names(out)){
		searchfor <- function(thing, metadata){
			use <- grep(paste0('^', thing, ': '), metadata)
			if (length(use) > 0){
				out <- sub(paste0('^', thing, ': '), '',
					metadata[use[1]])
				out <- sub('[ ]+$', '', out)
				return(out)
			} else {
				return('')
			}
		}
		if (is.null(codelist_name)) {
			codelist_name <- searchfor('codelist_name', out$metadata)
		}
		if (is.null(version)) {
			version <- searchfor('version', out$metadata)
		}
		if (is.null(author)) {
			author <- searchfor('author', out$metadata)
		}
		if (is.null(date)) {
			date <- searchfor('date', out$metadata)
		}
		out[, metadata := NULL]
	}

	# Ensure that the include_desc marker is logical
	if ('include_desc' %in% names(out)){
		out[, include_desc := as.logical(include_desc)]
	} else {
		if (identical(include_desc, TRUE) | identical(include_desc, FALSE)){
			global_include_desc <- include_desc
			out[, include_desc := global_include_desc]
		} else {
			out[, include_desc := FALSE]
		}
	}
	
	# Ensure that the inclusion marker is logical
	if ('included' %in% names(out)){
		out[, included := as.logical(included)]
	} else {
		out[, included := TRUE]
	}
	
	# Expand to show descendants for inclusion and exclusion subsets
	expandDescendants <- function(x, inclusion, SNOMED){
		if (any(x$include_desc == TRUE & x$included == inclusion)){
			desc <- setdiff(descendants(x[include_desc == TRUE &
				included == inclusion]$conceptId, SNOMED = SNOMED),
				x[included == inclusion]$conceptId)
			if (length(desc) > 0){
				return(rbind(x, data.table(conceptId = desc,
				include_desc = NA, included = inclusion), fill = TRUE))
			} else {
				return(x)
			}
		} else {
			return(x)
		}
	}
	
	out <- expandDescendants(out, TRUE, SNOMED = SNOMED)
	out <- expandDescendants(out, FALSE, SNOMED = SNOMED)

	# Reset the descendants marker now that all descendants are listed
	out[, include_desc := FALSE]

	# Remove exclusion terms
	toremove <- out[included == FALSE]$conceptId
	if (length(toremove) > 0){
		out <- out[!(conceptId %in% toremove)]
	}
	
	# Remove duplicate rows
	out[, .dup := duplicated(out[, list(conceptId, include_desc, included)])]
	out <- out[.dup == FALSE]
	out[, .dup := NULL]
	
	# The codelist is now in 'simple' format
	# i.e. one row per concept, included concepts only
	
	# 2. CONVERT TO DESIRED FORMAT
	if (show_excluded_descendants == TRUE &
		format %in% c('tree', 'exptree')){
		# Add non-included active descendants (for tree format codelists)
		desc <- setdiff(descendants(out$conceptId, SNOMED = SNOMED),
			out$conceptId)
		# Add these concepts to the inclusion and exclusion lists
		# (so they get excluded)
		if (length(desc) > 0){
			out <- rbind(out,
				data.table(conceptId = desc, included = TRUE,
				include_desc = FALSE), 
				data.table(conceptId = desc, included = FALSE,
				include_desc = FALSE), fill = TRUE)
		}
	}
	
	simpleToTree <- function(conceptIds,
		include_desc = logical(length(conceptIds)), SNOMED = SNOMED){
		# Returns include_desc (TRUE, FALSE, NA) for a set of conceptIds
		# If include_desc is supplied, NA entries are ignored because
		# it is assumed that these concepts are already included as
		# descendants of other concepts
		if (length(conceptIds) > 0){
			for (i in seq_along(conceptIds)){
				if (!is.na(include_desc[i])){
					desc <- descendants(conceptIds[i], SNOMED = SNOMED)
					if (length(desc) > 0){
						if (all(desc %in% conceptIds)){
							include_desc[conceptIds %in% desc] <- NA
							include_desc[i] <- TRUE
						}
					}
				}
			}
			return(include_desc)
		} else {
			return(logical(0))
		}
	}
	
	# 2. Identify trees in inclusion and exclusion sets
	if (format %in% c('tree', 'exptree')){
		out[included == TRUE, include_desc := simpleToTree(conceptId,
			include_desc, SNOMED = SNOMED)]
		if (any(out$included == FALSE)){
			out[included == FALSE, include_desc :=
				simpleToTree(conceptId, include_desc, SNOMED = SNOMED)]
		}
	}
	
	if (format == 'tree'){
		# Do not show descendants
		out <- out[!is.na(include_desc)]
	} 

	# Add term descriptions if not included
	if (!('term' %in% names(out))){
		out[, term := NA_character_]
	}
	# Add missing terms
	# Add SNOMED terms (fully specified names)
	# choose active term description if there is a choice
	# Ensure only one term per conceptId
	if (any(is.na(out$term))){
		TERMS <- SNOMED$DESCRIPTION[typeId ==
			as.integer64('900000000000003001')][
			out[is.na(term)], on = 'conceptId'][
			order(conceptId, active)][,
			list(term = term[.N]), by = conceptId]
		out[is.na(term), term := TERMS[out[is.na(term)],
			on = 'conceptId']$term]
	}

	if (format == 'simple'){
		# include_desc and included must not be included
		out[, include_desc := NULL]
		out[, included := NULL]
		data.table::setkeyv(out, 'conceptId')
		data.table::setcolorder(out, c('conceptId', 'term'))
	} else {
		data.table::setkeyv(out, c('included', 'conceptId'))
		data.table::setcolorder(out, c('conceptId',
			'include_desc', 'included', 'term'))
	}

	data.table::setattr(out, 'class', c('SNOMEDcodelist',
		'data.table', 'data.frame'))
	
	# 3. SET METADATA
	data.table::setattr(out, 'codelist_name', as.character(codelist_name))
	data.table::setattr(out, 'version', as.character(version))
	data.table::setattr(out, 'author', as.character(author))
	data.table::setattr(out, 'date', as.character(date))
	data.table::setattr(out, 'timestamp', Sys.time())
	data.table::setattr(out, 'sct_version', SNOMED$metadata$version)
	data.table::setattr(out, 'format', format)
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
as.integer64.SNOMEDconcept <- function(x, ...){
	class(x) <- 'integer64'
	bit64::as.integer64(x, ...)
}

#' @rdname SNOMEDcodelist
#' @family SNOMEDcodelist functions
#' @export
as.SNOMEDcodelist <- function(x, ...){
	if (is.SNOMEDcodelist(x, ...)){
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
#' @param x SNOMEDcodelist to expand or contract. If x is not a
#'   SNOMEDcodelist, it is coerced to one by as.SNOMEDcodelist
#' @param SNOMED environment containing a SNOMED dictionary
#' @param max_excluded_descendants (integer) whether to show excluded
#'   descendants as long as they do not exceed this number (a
#'   limit is suggested to avoid the program crashing if there
#'   are too many descendants). If this number is exceeded, the
#'   program will initially try to include children only, and if
#'   there are still too many, it will ignore all descendants.
#'   An 'included' column is added to the codelist
#'   showing which terms are included. This can make it easy to see
#'   if a codelist is consistent with the SNOMED CT ontology.
#' @param ... other arguments to pass to as.SNOMEDcodelist
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
expandSNOMED <- function(x, SNOMED = getSNOMED(), ...){
	x <- as.SNOMEDcodelist(x, ...)
	SNOMEDcodelist(x, format = 'exptree', SNOMED = SNOMED)
}

#' @rdname expandSNOMED
#' @family SNOMEDcodelist functions
#' @export
contractSNOMED <- function(x, SNOMED = getSNOMED(), ...){
	x <- as.SNOMEDcodelist(x, ...)
	SNOMEDcodelist(x, format = 'tree', SNOMED = SNOMED)
}

#' @rdname expandSNOMED
#' @family SNOMEDcodelist functions
#' @export
showCodelistHierarchy <- function(x, SNOMED = getSNOMED(),
	max_excluded_descendants = 200, ...){
	# Contract a SNOMED CT codelist, show the terms in a 
	# sensible order and optionally show terms not included
	# that are descendants of included terms 
	
	# For debugging
	debug <- FALSE
	
	# Declare names to be used for R CMD check
	include_desc <- conceptId <- .keep <- NULL
	included <- gen <- parentId <- roworder <- parentrowid <- NULL
	duplicate <- childrowid <- descendantrowid <- NULL
	allthisrowid <- alldescendantrowid <- NULL
	
	# Ensure that x is a SNOMEDcodelist
	x <- as.SNOMEDcodelist(x, SNOMED = SNOMED, ...)
	x <- data.table::copy(x)
	
	# Convert any 'list' columns to character
	for (i in colnames(x)){
		if (is.list(x[[i]])){
			x[, (i) := sapply(x[[i]],
				function(z) paste(z, collapse = ','))]
		}
	}
	
	out <- expandSNOMED(x, SNOMED = SNOMED)
	out[, included := TRUE]
	out <- out[!duplicated(out)] # remove duplicates of entire rows
	
	# Check only one row per concept
	if (any(duplicated(out$conceptId))){
		stop('Codelist must have only one row per concept')
	}
	
	if (max_excluded_descendants > 0){
		desc <- SNOMEDcodelist(setdiff(
			descendants(out$conceptId, SNOMED = SNOMED),
			out$conceptId), include_desc = FALSE)
		if (nrow(desc) > 0){
			if (nrow(desc) <= max_excluded_descendants){
				desc[, included := FALSE]
				out <- rbind(out, desc, fill = TRUE)
			} else {
				desc <- SNOMEDcodelist(setdiff(
					children(out$conceptId, SNOMED = SNOMED),
					out$conceptId), include_desc = FALSE)
				if (nrow(desc) <= max_excluded_descendants){
					desc[, included := FALSE]
					out <- rbind(out, desc, fill = TRUE)
					message('Adding children only as too many descendants')
				} else {
					message('Not adding descendants as too many')
				}
			}
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
	
	# Return the output as 'codelistHierarchy' class so that
	# it can be correctly handled by htmlCodelistHierarchy
	data.table::setattr(out, 'class', c('codelistHierarchy',
		'SNOMEDcodelist', 'data.table', 'data.frame'))
	out[]
}

#' Check if an object is a SNOMEDcodelist
#'
#' SNOMEDcodelist is an S3 class for lists of SNOMED codes.
#' This function checks whether the object has the class
#' SNOMEDcodelist, and whether the specified attributes are
#' as per the arguments (if the arguments are left as NULL, as per
#' default, they are not checked).
#' The function does not check if the codelist contains valid data.
#'
#' @param x object to check
#' @param format Whether the codelist is expressed as a simple
#'   enumeration of concepts ('simple'), as a set of concept
#'   hierarchies ('tree') or as a set of hierarchies showing all
#'   concepts ('exptree'). Codelists can be converted between
#'   the formats, but the result of conversion may depend on the 
#'   SNOMED CT dictionary being used.
#' @param codelist_name Name of the codelist (character vector of length 1)
#' @param version Version of the codelist (character vector of length 1)
#' @param author Author of the codelist (character vector of length 1)
#' @param date Date assigned to the codelist (character vector of length 1)
#' @param SNOMED Dummy argument to ensure that this function works with
#'   as.SNOMEDcodelist
#' @return a logical vector of length one: TRUE or FALSE
#' @family SNOMEDcodelist functions
#' @export
is.SNOMEDcodelist <- function(x, format = NULL, codelist_name = NULL,
	version = NULL, author = NULL, date = NULL, SNOMED = NULL){
	if ('SNOMEDcodelist' %in% class(x) & is.data.table(x)){
		if ((identical(attr(x, 'format'), 'tree') |
			identical(attr(x, 'format'), 'exptree') |
			identical(attr(x, 'format'), 'simple')) &
			(is.null(format) | identical(attr(x, 'format'), format)) &
			(is.null(date) | identical(attr(x, 'date'), date)) &
			(is.null(codelist_name) | identical(attr(x, 'codelist_name'),
				codelist_name)) &
			(is.null(version) | identical(attr(x, 'version'), version)) &
			(is.null(author) | identical(attr(x, 'author'), author))){
			TRUE
		} else {
			FALSE
		}
	} else {
		FALSE
	}
}

#' Export a SNOMEDcodelist
#'
#' Writes a SNOMEDcodelist to file. If the filename is NULL,
#' a filename is created from the 'codelist_name' attribute.
#'
#' @param x SNOMEDcodelist object to export to file
#' @param filename character vector of length 1 for the file
#'   to write to. If NULL, a filename is generated from the
#'   codelist filename.
#' @param metadata whether metadata is exported (default = TRUE)
#' @param ... not used
#' @return invisibly returns the exported codelist
#' @family SNOMEDcodelist functions
#' @export
export <- function(x, ...){
	# S3 generic function definition
	UseMethod("export")
}

#' @rdname export
#' @family SNOMEDcodelist functions
#' @export
export.SNOMEDcodelist <- function(x, filename = NULL, 
	metadata = TRUE, ...){
	# Exports a codelist to file.
	# All metadata must be stored in the codelist.

	if (is.null(filename)){
		filename <- makeCodelistFilename(x)
	} else if (grepl('[\\\\/]$', filename)){
		# filename is a directory; so create a filename
		filename <- paste0(filename, makeCodelistFilename(x))
	}
	
	message(paste0('Exporting ', attr(x, 'codelist_name'),
		'\nSNOMEDcodelist to ', filename))

	# Prepare output file
	out <- copy(x)

	if (metadata == TRUE) {	
		# Create the metadata character vector
		metadata <- encodeMetadata(x)
		metadataWidth <- max(nchar(metadata)) + 1
		metadata <- padTo(metadata, metadataWidth)
		
		# Output to file
		# Export to text file (CSV or tab separated)
		# Bind metadata column onto codelist
		if (length(metadata) < nrow(out)){
			metadata <- c(metadata, rep(padTo('', metadataWidth),
				nrow(out) - length(metadata)))
		} else if (length(metadata) > nrow(out)) {
			out <- copy(out[1:length(metadata)])
		}

		# Bind them all together in a custom CSV
		out[, metadata:= metadata]

		# Put the metadata column first
		setcolorder(out, c('metadata', setdiff(colnames(out),
			c('metadata'))))
	}

	 if (grepl('.csv$', tolower(filename))){
		data.table::fwrite(out, file = filename)
	} else {
		# tab delimited 
		data.table::fwrite(out, file = filename, sep='\t')
	}
	return(invisible(x))
}

#' Display a SNOMEDcodelist on screen
#'
#' Displays a SNOMEDcodelist on screen, including metadata.
#' Truncates term descriptions in order to fit within the line width.
#'
#' @param x SNOMEDcodelist object to print to screen
#' @param ... not used
#' @return invisibly returns the codelist
#' @family SNOMEDcodelist functions
#' @export
print.SNOMEDcodelist <- function(x, ...){
	cat('SNOMED CT codelist: \n')
	cat(paste(encodeMetadata(x), collapse = '\n'), '\n')
	printTerms(x)
	invisible(x)
}

#' Add inactive concepts to a SNOMEDcodelist or SNOMEDconcept vector
#'
#' Adds SNOMED concepts linked by the QUERY or HISTORY tables
#' that are mapped to or descendants of concepts in a SNOMEDcodelist
#' or a SNOMEDconcept vector. If a SNOMEDcodelist, it is automatically
#' converted to the 'simple' format (all items enumerated).
#'
#' It is recommended to use this function to convert a reference
#' into a codelist for running a query against an electronic health
#' record database which might contain historic SNOMED CT concepts.
#'
#' @param x SNOMEDcodelist or SNOMEDconcept object
#' @param provenance vector of provenance values to use
#' @param SNOMED SNOMED environment containing HISTORY and QUERY tables
#' @return SNOMEDcodelist or SNOMEDconcept with linked inactive concepts included
#' @family SNOMEDcodelist functions
#' @export
addInactiveConcepts <- function(x, provenance = 0:3, SNOMED = getSNOMED()){
	supertypeId <- NEWCONCEPTID <- NULL

	if (is.SNOMEDconcept(x)){
		if ('QUERY' %in% names(SNOMED)){
			x <- union(x, as.SNOMEDconcept(SNOMED$QUERY[supertypeId %in% x &
				provenance %in% provenance]$subtypeId))
		} else {
			warning('SNOMED does not contain a query table')
		}
		if ('HISTORY' %in% names(SNOMED)){
			x <- union(x, as.SNOMEDconcept(SNOMED$HISTORY[
				NEWCONCEPTID %in% x]$OLDCONCEPTID))
		} else {
			warning('SNOMED does not contain a history table')
		}
		return(x)
	} else if (is.SNOMEDcodelist(x)){
		x <- as.SNOMEDcodelist(x, format = 'simple')
		extra_concepts <- setdiff(addInactiveConcepts(
			as.SNOMEDconcept(x$conceptId),
			provenance = provenance, SNOMED = getSNOMED()), x$conceptId)
		out <- rbind(x, data.table(conceptId = extra_concepts,
			term = description(extra_concepts)$term), fill = TRUE)
		# Ensure metadata is not lost in the conversion
		data.table::setattr(out, 'codelist_name', attr(x, 'codelist_name'))
		data.table::setattr(out, 'version', attr(x, 'version'))
		data.table::setattr(out, 'author', attr(x, 'author'))
		data.table::setattr(out, 'date', attr(x, 'date'))
		data.table::setattr(out, 'timestamp', Sys.time())
		data.table::setattr(out, 'sct_version', SNOMED$metadata$version)
		data.table::setattr(out, 'format', 'simple')
		data.table::setkeyv(out, 'conceptId')
		return(out)
	} else {
		warning('x is not a SNOMEDconcept or SNOMEDcodelist')
		return(x)
	}
}



# Internal function
padTo <- function(string, length){
	# Returns a character vector with strings padded to a particular length
	# Arguments: string - string to pad out with additional spaces
	#            length - final length of string
	spaces <- paste(rep(' ', length), collapse = '')
	substr(paste0(string, spaces), 1, length)
}

# Internal function
printTerms <- function(x){
	# Prints the table portion of a codelist, using the maximum
	# available width
	term <- show <- NULL

	if ('term' %in% colnames(x)){
		x2 <- data.table::copy(x)
		# Rename 'include_desc' and 'included' columns
		if ('include_desc' %in% colnames(x2)){
			setnames(x2, 'include_desc', 'inc_d')
		}
		if ('included' %in% colnames(x2)){
			setnames(x2, 'included', 'inclu')
		}
		# Calculate the width without term column
		temp <- lapply(x2[, setdiff(colnames(x2), 'term'), with = FALSE],
			function(z){max(nchar(as.character(z)), na.rm = TRUE)})
		# Calculate the total width without term column
		# (Minumum allowed width = 20)
		termwidth <- max(getOption('width') - 6 - 
			sum(pmax(nchar(names(temp)), unlist(temp))) - length(temp), 20)
		x2[, term := truncateChar(term, termwidth)]
	} else {
		x2 <- data.table::copy(x)
	}
	data.table::setattr(x2, 'class', c('data.table', 'data.frame'))
	methods::show(x2)
}

# Internal function
truncateChar <- function(x, maxchar){
	# Truncates a character vector so that each element does not have more
	# than a specified number of characters, adding ... to the end of 
	# truncated terms
	# Arguments: x - character string to truncate
	#            maxchar - length to truncate to
	convert <- nchar(ifelse(is.na(x), '', x)) > maxchar
	x[convert] <- paste0(substr(x[convert], 1, maxchar - 3), '...')
	x
}

# Internal function
encodeMetadata <- function(x){
	# Returns a character string with formatted metadata of the
	# codelist x
	c(paste0('codelist_name: ', attr(x, 'codelist_name')),
		paste0('version: ', attr(x, 'version')),
		paste0('author: ', attr(x, 'author')),
		paste0('date: ', attr(x, 'date')),
		paste0('timestamp: ', attr(x, 'timestamp')),
		paste0('sct_version: ', attr(x, 'sct_version')),
		paste0('format: ', attr(x, 'format')))
}

# Internal function
makeCodelistFilename <- function(x){
	paste0(attr(x, 'codelist_name'), '.SNOMEDcodelist.',
		as.character(floor(as.numeric(attr(x, 'version')))), '.csv')
}
