#' WORK IN PROGRESS
#'
#' Extracts SNOMED CT concepts from appropriate places in the 
#' hierarchy to create a set of CDB files in an environment.
#' Uses WordNet and manual synonyms if available.
#'
#' @param SNOMED environment containing a SNOMED dictionary
#' @param WN WordNet data.table as returned by downloadWordnet
#'   containing WordNet data from appropriate
#'   categories, in the format: cat (character), wordnetId (integer64),
#'   synonyms (list), parents (list), adj (list)
#' @param MANUAL_SYNONYMS 
#' @return environment containing the following data tables: FINDINGS,
#'   QUAL, CAUSES, BODY, FINDINGS, OTHERSUB, OVERLAP
#' @seealso [addWordnet()]
#' @references \url{https://wordnet.princeton.edu/}
#' @examples
#' # Not run
#' # WORDNET <- downloadWordnet()
decompose <- function(the_conceptId, diagnosis_text,
	SNOMED = getSNOMED(), noisy = FALSE, use_spacy = FALSE){
	# decomposes the diagnosis_text (e.g. from a SNOMED CT description)
	# conceptId <- SNOMEDconcept('Chronic systolic heart failure')
	# diagnosis_text <- 'chronic systolic heart failure'
	stripped_text <- gsub(' +', ' ', gsub('^ +| +$', '',
		gsub('-|,|\\(|\\)', ' ', tolower(diagnosis_text))))
	text <- paste0(' ', stripped_text, ' ')
	
		latConcepts <- s(c('Left', 'Right', 'Bilateral'))
	setattr(latConcepts, 'names', c('Left', 'Right', 'Bilateral'))

	SCT_assoc <- s('Associated with')
	SCT_cause <- s('Causative agent')
	SCT_after <- s('After')
	SCT_dueto <- s('Due to')
	SCT_findingsite <- s('Finding site')
	
	stopwords <- c('the', 'of', 'by', 'with', 'to', 'into', 'and', 'or',
		'both', 'at', 'as', 'and/or')
	
	if (noisy){
		message(paste0('\nDecomposing:', text))
	}
	
	BLANK_OUTPUT <- function() {
		return(data.table(parttext = character(0),
			rootId = bit64::integer64(0),
			with = bit64::integer64(0),
			due_to = bit64::integer64(0),
			after = bit64::integer64(0),
			without = bit64::integer64(0),
			body_site = bit64::integer64(0),
			severity = bit64::integer64(0),
			stage = bit64::integer64(0),
			laterality = bit64::integer64(0),
			other_attr = character(0)))
	}
	
	RELEVANT <- attrConcept(the_conceptId)
	relevant_conceptId <- c(RELEVANT$sourceId, RELEVANT$destinationId)
	relevant_conceptId <- union(union(children(
		setdiff(ancestors(relevant_conceptId),
		c('Disorder', 'Clinical finding')), include_self = TRUE),
		descendants(relevant_conceptId, include_self = TRUE)),
		QUAL$conceptId)
	
	#### SPLIT INTO PARTS
	do_splitparts <- function(conceptId, text){
		# text must be lower case with space before and after
		splitpart <- function(OLD, text, split_var, split_text,
			the_typeId = NULL, SOURCE = FINDINGS, reverse = FALSE){
			linked <- NULL
			# If split has already taken place successfully (i.e.
			# rootId is different from original conceptId, stop here
			if (nrow(OLD) == 1){
				if (OLD$rootId != conceptId){
					return(OLD)
				}
			}
			# the_typeId must be a single SNOMED concept
			if (text %like% paste0(' (', split_text, ') ')){
				if (reverse){
					linked <- as.SNOMEDconcept(unique(SOURCE[term ==
						sub(paste0('^ ([^ ].*) (', split_text,
						') (.*[^ ]) $'), ' \\1 ', text)]$conceptId))
				} else {
					linked <- as.SNOMEDconcept(unique(SOURCE[term ==
						sub(paste0('^ ([^ ].*) (', split_text,
						') (.*[^ ]) $'), ' \\3 ', text)]$conceptId))
				}
				if (!is.null(the_typeId)){
					# allow only one related concept, and it must match
					# the SNOMED relationshpi specified in the_typeId
					linked_text <- linked
					linked <- NULL
					linked_sct <- as.SNOMEDconcept(character(0))
					for (x in seq_along(the_typeId)){
						linked_sct <- c(linked_sct,
							relatedConcepts(conceptId,
							typeId = the_typeId[x], SNOMED = SNOMED))
					}
					# if there are multiple of linked or linked_sct,
					# restrict to those that match up
					if (length(linked) > 0){
						linkmatch <- intersect(linked_text, linked_sct)
						# if any overlap in text linked and SCT linked
						# terms, use only the overlap
						if (length(linkmatch) > 0){
							linked <- linked_sct <- linkmatch
						} else if (length(linked_sct) > 0) {
							# if no overlap, check that linked and
							# sct_linked have an ancestor-
							# descendant relationship
							linked <- intersect(linked_text,
								c(ancestors(linked_sct), linked_sct,
								descendants(linked_sct)))
						}
					} else {
						linked <- linked_sct
					}
					
					# check it is a valid concept to link to, and only
					# allow a single linked concept
					if (!(is.null(linked))){
						linked <- linked[linked %in% c(FINDINGS$conceptId,
							CAUSES$conceptId, BODY$conceptId)]
						if (length(linked) == 0){
							linked <- NULL
						} else {
							linked <- linked[1]
						}
					}
				} else {
					# error checking for linked
					if (length(linked) == 0){
						linked <- NULL
					} else {
						linked <- linked[1]
					}
				}
				if (!is.null(linked)){
					if (reverse){
						text <- sub(paste0('^ ([^ ].*) (', split_text,
						') (.*[^ ]) $'), ' \\3 ', text)
					} else {
						text <- sub(paste0('^ ([^ ].*) (', split_text,
						') (.*[^ ]) $'), ' \\1 ', text)
					}
				}
			}
			if (is.null(linked)){
				# unable to split
				return(OLD)
			} else {
				# finding the new rootID. If not found, keep as is.
				if (noisy){
					message(paste('Splitting',
						paste(split_var, collapse = ' '),
						'at', paste(split_text, collapse = ' ')))
				}
				the_rootId <- FINDINGS[term == text]$conceptId
				if (length(the_rootId) == 0){
					the_rootId <- conceptId
				}
				NEW <- data.table(text = text, rootId = the_rootId[1],
					temp = linked, other_attr = repl_(text))
				setnames(NEW, 'temp', split_var)
				return(rbind(OLD, NEW, fill = TRUE))
			}
		}
		
		OUT <- BLANK_OUTPUT()
		x <- text
		
		# Change to just doing one split, not trying others if first
		# attempt is successful
		
		# DUE TO
		OUT <- splitpart(OUT, x, 'due_to',
			'with|co-occurrent and due to|resulting from',
			SCT_dueto)
		OUT <- splitpart(OUT, x, 'due_to', 'due to|caused by',
			c(SCT_cause, SCT_dueto), SOURCE = CAUSES)
		OUT <- splitpart(OUT, x, 'due_to', 'causing|resulting in|complicated by',
			SCT_cause, SOURCE = CAUSES, reverse = TRUE)
		OUT <- splitpart(OUT, x, 'due_to', 'by',
			SCT_cause, SOURCE = ORGSUB)
		OUT <- splitpart(OUT, x, 'due_to', 'causing|resulting in',
			reverse = TRUE)
		OUT <- splitpart(OUT, x, 'due_to', 'induced',
			SCT_cause, SOURCE = ORGSUB, reverse = TRUE)
		OUT <- splitpart(OUT, x, 'due_to', 'due to|caused by')

		# WITH
		OUT <- splitpart(OUT, x, 'with', 'associated with', SCT_assoc)
		OUT <- splitpart(OUT, x, 'with', 'co-occurrent with|and')
		OUT <- splitpart(OUT, x, 'with', 'with|in')
		OUT <- splitpart(OUT, x, 'with', 'associated',
			SOURCE = CAUSES, reverse = TRUE)

		# AFTER
		OUT <- splitpart(OUT, x, 'after',
			'due to and following|as a sequela of|as a late effect of',
			SCT_after, SOURCE = CAUSES)
		OUT <- splitpart(OUT, x, 'after', 'following|after',
			SCT_after, SOURCE = CAUSES)
		OUT <- splitpart(OUT, x, 'after', 'following|after')

		
		# WITHOUT
		OUT <- splitpart(OUT, x, 'without', 'without|but without')
	
		OUT
	}
	
	C <- do_splitparts(the_conceptId, text)
	# Now C is a data.table with lines - one for each split version.
	# If there is no data in C, include the original term description
	if (nrow(C) == 0){
		C <- rbind(BLANK_OUTPUT(), data.table(rootId = the_conceptId,
			text = text, other_attr = repl_(text)),
			fill = TRUE)
	}
	
	if (noisy){
		message('\nAfter splitting by parts')
		print(C)
	}
	
	#### ADD SPACY DECOMPOSITION
	if (use_spacy){
		C[, spacy := list(as.data.table(spacy_parse(sub('^ ', '', text),
			entity = TRUE, remove_punct = TRUE, dependency = TRUE,
			pos = TRUE, output = 'data.frame'))), by = .I]
	}
	C[, partId := rootId]
	C[, parttext := text]
	
	#### EXPAND LINES USING ANCESTORS

	# Candidates for finding ancestor match
	A <- FINDINGS[conceptId %in% ancestors(the_conceptId)]

	e_ancestors <- function(DATALINE){
		# Returns a data.table containing the original DATALINE and
		# optionally additional data lines with decompositions by
		# searching for different ancestors
		ANC <- A[sapply(A$term, function(x){DATALINE$text %like% x})]
		if (nrow(ANC) > 0){
			return(rbindlist(lapply(1:nrow(ANC), function(x){
				new_text <- sub(ANC[x]$term, repl_(ANC[x]$term), DATALINE$text)
				OUTPUT <- rbind(DATALINE, DATALINE)
				OUTPUT[1, other_attr := new_text]
				OUTPUT[1, text := ANC[x]$term]
				OUTPUT[1, rootId := ANC[x]$conceptId]
			})))
		} else {
			# no valid ancestors - no decomposition performed
			return(DATALINE)
		}
	}
	
	if (nrow(A) > 0){
		TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_ancestors(C[x]))) 
		C <- TEMP	
		if (noisy){
			message('\nFinding ancestors')
			print(C)
		}
	} else {
		if (noisy){
			message('\nNo ancestors found')
		}
	}
	
	#### BODY SITE
	
	e_body <- function(DATALINE){
		# Returns a data.table containing the original DATALINE and
		# optionally additional data lines with decompositions by
		# searching for body parts.
		# Add both the stated finding site (as per SNOMED CT model) and
		# the concept suggested by decomposing the text, in case they
		# are different (e.g. ankle region / entire ankle) as both are
		# correct and should be available for potential matches.
		# However it is essential that laterality is correct - either
		# as part of concept or as an additional attribute
		if (nrow(B) > 0){
			# Subset of body sites that match with this term
			BOD <- B[sapply(B$term, function(x){
				DATALINE$other_attr %like% x
			})]
			if (nrow(BOD) > 0){
				return(rbindlist(lapply(1:nrow(BOD), function(x){
					# Is laterality included in the concept or
					# is it to be identified separately in the text?
					# Is laterality status the same as in the stated
					# body site?
					if (BOD[x]$required_laterality %in%
						c('Included', 'No laterality')){
						new_text <- sub(BOD[x]$term, repl_(BOD[x]$term),
							DATALINE$other_attr)
					} else if (BOD[x]$required_laterality %in%
						c('Left', 'Right', 'Bilateral')){
						# Get SNOMED laterality concepts from
						# LATERALITY lookup
						LATSELECT <- LATERALITY[conceptId ==
							latConcepts[BOD[x]$required_laterality]]
						LATSELECT <- LATSELECT[sapply(term, function(x){
							DATALINE$other_attr %like% x
							})]
						if (nrow(LATSELECT) > 0){
							# Only permit one row for simplicity
							new_text <- sub(LATSELECT[1]$term,
								repl_(LATSELECT[1]$term),
								sub(BOD[x]$term, repl_(BOD[x]$term),
								DATALINE$other_attr))
						} else {
							# invalid laterality - no decomposition performed
							return(DATALINE)
						}
					}
					# If laterality in the body site found
					# is the same as in the SNOMED modelled
					# body site, add this as well.
					if (BOD[x]$required_laterality ==
						B[exact == TRUE]$required_laterality[1]){
						if (length(the_body_siteId) == 1){
							OUTPUT <- rbind(DATALINE, DATALINE)
						} else {
							OUTPUT <- rbind(DATALINE, DATALINE, DATALINE)
							the_body_siteId <- the_body_siteId[1:2]
						}
						# allow for up to 2 SNOMED stated body sites
						OUTPUT[, body_site := c(the_body_siteId,
							BOD[x]$conceptId)]
					} else {
						OUTPUT <- copy(DATALINE)
						OUTPUT[, body_site := BOD[x]$conceptId]
					}
					OUTPUT[, other_attr := new_text]
					if (BOD[x]$required_laterality %in%
						c('Left', 'Right', 'Bilateral')){
						OUTPUT[, laterality :=
						latConcepts[BOD[x]$required_laterality]]
					}
					OUTPUT
				})))
			} else {
				# no valid body site - no decomposition performed
				return(DATALINE)
			}
		} else {
			return(DATALINE)
		}
	}

	# Candidates for body site match (must have same laterality as
	# base concept)
	the_body_siteId <- relatedConcepts(the_conceptId,
		typeId = SCT_findingsite, SNOMED = SNOMED)
	# the_body_siteId should only be one but a few concepts have
	# multiple body sites, so allow multiple
	
	if (length(the_body_siteId) > 0){
		the_laterality <- BODY_LATERALITY[
			conceptId %in% the_body_siteId]$laterality[1]
		# keep only the first the_laterality so that it has cardinality 1
		B <- BODY[conceptId %in% c(ancestors(the_body_siteId),
			descendants(the_body_siteId, include_self = TRUE))]
		B[, concept_laterality := BODY_LATERALITY[B,
			on = 'conceptId']$laterality]
		B[, required_laterality := ifelse(
			concept_laterality == the_laterality,
			'Included', the_laterality)]
		# Remove concepts with inconsistent laterality
		B <- B[required_laterality == 'Included' |
			(required_laterality == the_laterality &
			concept_laterality == 'Lateralisable')]
		B[, exact := conceptId %in% the_body_siteId]

		TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_body(C[x])))
		C <- TEMP
		
		if (noisy){
			message('\nFinding body sites')
			print(C)
		}
	} else {
		if (noisy){
			message('\nBody site not relevant for this disorder')
		}
	}
	
	#### CAUSES
	
	e_cause <- function(DATALINE){
		# Returns a data.table containing the original DATALINE and
		# optionally additional data lines with decompositions by
		# searching for causes.
		the_causeId <- relatedConcepts(DATALINE$partId,
			typeId = SCT_cause, SNOMED = SNOMED)
		if (length(the_causeId) > 0){
			CAU <- CAUSES[conceptId %in% the_causeId]
			CAU <- CAU[sapply(CAU$term, function(x){
				DATALINE$other_attr %like% x
			})]
			if (nrow(CAU) > 0){
				return(rbindlist(lapply(1:nrow(CAU), function(x){
					new_text <- sub(CAU[x]$term, repl_(CAU[x]$term),
						DATALINE$other_attr)
					OUTPUT <- copy(DATALINE)
					OUTPUT[, other_attr := new_text]
					OUTPUT[, due_to := the_causeId]
				})))
			} else {
				# no valid cause - no decomposition performed
				return(DATALINE)
			}
		} else {
			return(DATALINE)
		}
	}
	
	TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_cause(C[x])))
	C <- TEMP

	if (noisy){
		message('\nFinding causes')
		print(C)
	}

	#### SEVERITY

	e_severity <- function(DATALINE){
		# Returns a data.table containing the original DATALINE and
		# with severity information extracted. 
		SEV <- SEVERITY[sapply(SEVERITY$term, function(x){
			DATALINE$other_attr %like% x
		})]
		if (nrow(SEV) > 0){
			return(rbindlist(lapply(1:nrow(SEV), function(x){
				new_text <- sub(SEV[x]$term, repl_(SEV[x]$term),
					DATALINE$other_attr)
				OUTPUT <- copy(DATALINE)
				OUTPUT[, other_attr := new_text]
				OUTPUT[, severity := SEV[x]$conceptId]
			})))
		} else {
			# no valid severity - no decomposition performed
			return(DATALINE)
		}
	}

	TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_severity(C[x])))
	C <- TEMP
	
	if (noisy){
		message('\nFinding severity')
		print(C)
	}

	#### STAGE

	e_stage <- function(DATALINE){
		# Returns a data.table containing the original DATALINE and
		# with stage information extracted.
		STA <- STAGE[sapply(STAGE$term, function(x){
			DATALINE$other_attr %like% x
		})]
		if (nrow(STA) > 0){
			return(rbindlist(lapply(1:nrow(STA), function(x){
				new_text <- sub(STA[x]$term, repl_(STA[x]$term),
					DATALINE$other_attr)
				OUTPUT <- copy(DATALINE)
				OUTPUT[, other_attr := new_text]
				OUTPUT[, stage := STA[x]$conceptId]
			})))
		} else {
			# no valid stage - no decomposition performed
			return(DATALINE)
		}
	}

	TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_stage(C[x])))
	C <- TEMP
	
	if (noisy){
		message('\nFinding stage')
		print(C)
	}
	
	# Now decompose other_attr
	# Assume that all remaining elements are descriptors
	# Search for digrams that are SNOMED concepts, then other digrams
	# that are compound nouns according to Spacy 
	# Look for SNOMED qualifiers and Wordnet adjectives

	e_other_attr <- function(DATALINE){
		# Look for and extract other attributes
		# Loop through words remaining in other_attr
		the_other_attr <- c(strsplit(sub('^ ', '',
			DATALINE$other_attr), ' ')[[1]], '@')
		DATALINE[, other_conceptId := '']
		i <- 1
		
		# Loop until i reaches the end of other_attr
		while (i < length(the_other_attr)){
			# Advance to first word
			while (i < length(the_other_attr) & the_other_attr[i] == '@'){
				i <- i + 1
			}
			j <- i
			# Locate end of phrase
			while (j < length(the_other_attr) & the_other_attr[j] != '@'){
				j <- j + 1
			}
			# Try to match sequence to a SNOMED or WORDNET concept
			match <- FALSE
			while (!match & j > i){
				j <- j - 1
				to_match <- paste0(' ', paste(the_other_attr[i:j],
					collapse = ' '), ' ')
				if (to_match %in% OTHERSEARCH[conceptId %in%
					relevant_conceptId]$term){
					match <- TRUE
					DATALINE[, other_conceptId := paste(other_conceptId,
						paste(
						unique(OTHERSEARCH[term == to_match &
						conceptId %in% relevant_conceptId]$conceptId),
						collapse = '|'))]
					i <- j
				}
			}
			if (use_spacy){
				if (match == FALSE & (i + 2) < length(the_other_attr)){
					if (the_other_attr[i + 1] != '@' &
						DATALINE$spacy[[1]]$dep_rel[i] == 'compound' &
						DATALINE$spacy[[1]]$head_token_id[i] == i + 1 &
						DATALINE$spacy[[1]]$dep_rel[i + 1] == 'compound' &
						DATALINE$spacy[[1]]$head_token_id[i + 1] == i + 2){
						DATALINE[, other_conceptId := paste(other_conceptId,
							paste(the_other_attr[i:(i + 2)],
							collapse = '_'))]
						match <- TRUE
						i <- i + 2
					}
				}
				if (match == FALSE & (i + 1) < length(the_other_attr)){
					if (the_other_attr[i + 1] != '@' &
						(DATALINE$spacy[[1]]$dep_rel[i] == 'compound' &
						DATALINE$spacy[[1]]$head_token_id[i] == i + 1) |
						(DATALINE$spacy[[1]]$dep_rel[i + 1] == 'nummod' &
						DATALINE$spacy[[1]]$head_token_id[i + 1] == i)){
						DATALINE[, other_conceptId := paste(other_conceptId,
							paste(the_other_attr[i:(i + 1)],
							collapse = '_'))]
						match <- TRUE
						i <- i + 1
					}
				}
			}
			if (match == FALSE){
				if (the_other_attr[i] %in% stopwords){
					# pass
				} else {
					# keep as a single word
					DATALINE[, other_conceptId := paste(other_conceptId,
						the_other_attr[i])]
				}
			}
			i <- i + 1
		}
		return(DATALINE)
	}

	TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_other_attr(C[x])))
	C <- TEMP
	C[, other_conceptId := gsub(' +', ' ', gsub('^ | $', '',
		gsub('@', '', other_conceptId)))]
	
	if (noisy){
		message('\nFinding other attributes')
		print(C)
	}

	# Remove spacy table to enable de-duplication
	if (use_spacy){
		C[, spacy := NULL]
	}
	C <- C[!duplicated(C)]

	# Add original conceptId
	C[, origId := the_conceptId]
	
	# Rename text to roottext
	setnames(C, 'text', 'roottext')
	
	# Remove any entries where rootId == origId
	C <- C[!(rootId == origId)]
	
	C
}




matchpos <- function(bigvector, smallvector){
	# returns position at which smallvector starts within bigvector
	matched <- FALSE
	i <- 0
	while (!matched & (i < length(bigvector))){
		if (all(smallvector == bigvector[i + 1:length(smallvector)])){
			matched <- TRUE
		} else {
			i <- i + 1
		}
	}
	if (i < length(bigvector)){
		return(i + 1:length(smallvector))
	} else {
		return(NULL)
	}
}

repl_ <- function(x){
	# replaces every word in x with @
	i <- nchar(x)
	while(x %like% '[[:alnum:]]' & i > 0){
		x <- sub('^([^\\@ ]+) | ([^\\@ ]+) | ([^\\@ ]+)$', ' @ ', x)
		i <- i - 1 # counter to prevent infinite loops
	}
	x
}

expand_other_conceptId <- function(DATALINES){
	# Adds additional lines for other conceptId
	# This also works in vectorised form for the entire dataset
	done <- FALSE
	while (!done){
		DATA1 <- copy(DATALINES)
		DATA2 <- copy(DATALINES)
		DATA1[, other_conceptId := sub(' ([^ ]+)\\|([^ ]+) ', ' \\1 ',
			paste0(' ', other_conceptId, ' '))]
		DATA2[, other_conceptId := sub(' ([^ ]+)\\|([^ ]+) ', ' \\2 ',
			paste0(' ', other_conceptId, ' '))]
		if (all(DATA1$other_conceptId == DATA2$other_conceptId)){
			done <- TRUE
		}
		DATALINES <- rbind(DATA1, DATA2)
		DATALINES <- DATALINES[!duplicated(DATALINES)]
	}
	DATALINES[, other_conceptId := gsub('^ +| +$', '', other_conceptId)]
	DATALINES
}

test_decompose <- function(x, diagnosis_text = NULL, noisy = FALSE){
	the_conceptId <- as.SNOMEDconcept(x)
	if (length(the_conceptId) == 0){
		stop('No SNOMED concept')
	}
	texts <- description(the_conceptId, include_synonyms = TRUE)
	texts <- texts[type == 'Synonym']$term
	if (is.null(diagnosis_text)){
		diagnosis_text <- texts[1]
	} else {
		texts <- diagnosis_text
	}
	
	cat('\nDecomposition of', print(the_conceptId), ':\n')
	for (diagnosis_text in texts){
		cat('\n\n', diagnosis_text, '\n')
		print_decomposition(expand_other_conceptId(decompose(
			the_conceptId, diagnosis_text, SNOMED = getSNOMED(),
			noisy = noisy)))
	}
}

print_decomposition <- function(D, SNOMED = getSNOMED()){
	if (nrow(D) == 0){
		return(NULL)
	}
	# Prints a SNOMED CT decomposition
	show_concept <- function(x, offset = 16){
		x <- as.SNOMEDconcept(x, SNOMED = SNOMED)
		if (x %in% SNOMED$DESCRIPTION$conceptId){
			output <- paste0(x, " | ", description(x, SNOMED = SNOMED)$term)
		} else if (x %in% WORDNET$conceptId) {
			output <- paste0('Wordnet', paste(WORDNET[conceptId == x]$term,
				collapse = ', '))
		} else {
			output <- as.character(x)
		}
		truncateChar <- function(x, maxchar) {
			convert <- nchar(x) > maxchar
			x[convert] <- paste0(substr(x[convert], 1, maxchar - 
				3), "...")
			x
		}
		truncateChar(output, getOption("width") - offset)
	}
	for (i in 1:nrow(D)){
		cat(paste(c('\n', rep('-', getOption('width'))), collapse = ''))
		cat(paste0('\n', show_concept(D[i]$origId, 0)))
		cat(paste(c('\n', rep('-', getOption('width'))), collapse = ''))
		cat('\nRoot: ', show_concept(D[i]$rootId, 6))  
		if (!is.na(D[i]$with)){
			cat('\n- With: ', show_concept(D[i]$with, 8))
		}
		if (!is.na(D[i]$due_to)){
			cat('\n- Due to: ', show_concept(D[i]$due_to, 10))
		}
		if (!is.na(D[i]$after)){
			cat('\n- After: ', show_concept(D[i]$after, 9))
		}
		if (!is.na(D[i]$without)){
			cat('\n- Without: ', show_concept(D[i]$without, 11))
		}
		if (!is.na(D[i]$body_site)){
			cat('\n- Body site: ', show_concept(D[i]$body_site, 13))
		}
		if (!is.na(D[i]$severity)){
			cat('\n- Severity: ', show_concept(D[i]$severity, 12))
		}
		if (!is.na(D[i]$stage)){
			cat('\n- Stage: ', show_concept(D[i]$stage, 9))
		}
		if (!is.na(D[i]$laterality)){
			cat('\n- Laterality: ', show_concept(D[i]$laterality, 14))
		}
		if (!(D[i]$other_conceptId == '' | is.na(D[i]$other_conceptId))){
			cat('\n- Other attributes: ')
			for (theconcept in strsplit(D[i]$other_conceptId, ' ')[[1]]){
				if (theconcept %like% '^[[:digit:]]+$'){
					cat('\n  - ', show_concept(theconcept, 4))
				} else {
					cat('\n  - ', theconcept)
				}
			}
		}
		cat('\n\n')
	}
	invisible(TRUE)
}
