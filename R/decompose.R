#' Decomposition of meaning of a finding or disorder SNOMED CT concept 
#'
#' Decomposes a SNOMED CT term into separate components according
#' to the SNOMED CT information model and text parsing. Each term
#' may have a number of possible decompositions. Requires a CDB
#' environment created by createCDB.
#'
#' @param conceptIds vector of SNOMED CT concepts to decompose
#' @param diagnosis_text vector of SNOMED CT terms (or in theory any
#'   text that has the same meaning as the SNOMED CT concept). If NULL,
#'   decompositions are created for all SNOMED CT synonyms of the
#'   concepts.
#' @param CDB an environment containing CDB files, as created by
#'   createCDB
#' @param SNOMED an environment containing the SNOMED CT dictionary
#' @param noisy whether to output messages (for debugging)
#' @param omit_unmatched whether to omit rows in which some attributes
#'   could not be matched to SNOMED CT concepts
#' @return a SNOMEDfinding objects, which is a data.table with columns
#'   rootId (integer64), with (integer64), due_to (integer64),
#'   after (integer64), without (integer64), body_site (integer64),
#'   severity (integer64), stage (integer64), laterality (integer64),
#'   roottext (character), partId (integer64),
#'   other_conceptId (character, a string of SNOMED CT concept IDs
#'   separated by spaces), origId (integer64)
#' @export
#' @examples
#' \dontrun{
#' miniCDB <- createCDB(SNOMED = sampleSNOMED())
#' D <- decompose(as.SNOMEDconcept('Cor pulmonale',
#'   SNOMED = sampleSNOMED()), CDB = miniCDB, SNOMED = sampleSNOMED())
#' }
#'
#' # -------------------------------------------------------
#' # 83291003 | Cor pulmonale (disorder)
#' # -------------------------------------------------------
#' # Root : 367363000 | Right ventricular failure (disorder)
#' # - Due to : 19829001 | Disorder of lung (disorder)
decompose <- function(conceptIds, diagnosis_text = NULL, CDB,
	SNOMED = getSNOMED(), noisy = FALSE, omit_unmatched = TRUE){
	# decomposes the diagnosis_text (e.g. from a SNOMED CT description)
	# conceptId <- SNOMEDconcept('Chronic systolic heart failure')
	# diagnosis_text <- 'chronic systolic heart failure'
	
	# Declare symbols to avoid R check error
	type <- term <- partId <- rootId <- parttext <- conceptId <- NULL
	conceptId <- use <- other_attr <- rootId <- origId <- NULL
	required_laterality <- exact <- body_site <- laterality <- NULL
	due_to <- severity <- stage <- other_conceptId <- without <- NULL
	sourceId <- destinationId <- typeId <- active <- after <- NULL
	
	conceptIds <- as.character(as.SNOMEDconcept(conceptIds,
		SNOMED = SNOMED))
	
	if (length(conceptIds) > 1){
		if (is.null(diagnosis_text)){
			C <- rbindlist(lapply(conceptIds, function(x){
				decompose(x, diagnosis_text = NULL, CDB,
					SNOMED = SNOMED, noisy = noisy,
					omit_unmatched = omit_unmatched)
			}))
			C <- C[!duplicated(C)]
			setattr(C, 'class', c('SNOMEDfindings', 'data.table', 'data.frame'))
			return(C)
		} else if (length(diagnosis_text) == length(conceptIds)){
			C <- rbindlist(mapply(function(x, y){
				decompose(x, diagnosis_text = y, CDB,
					SNOMED = SNOMED, noisy = noisy,
					omit_unmatched = omit_unmatched)
			}, x = conceptIds, y = diagnosis_text))
			C <- C[!duplicated(C)]
			setattr(C, 'class', c('SNOMEDfindings', 'data.table', 'data.frame'))
			return(C)
		} else {
			stop('diagnosis_text must be NULL or the same length as conceptIds')
		}
	}
	
	if (length(conceptIds) > 1){
		stop('This part of the function can only analyse a single conceptId')
	} else {
		the_conceptId <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
	}
	
	if (is.null(diagnosis_text)){
		diagnosis_text <- description(the_conceptId, SNOMED = SNOMED,
			include_synonyms = TRUE)[type == 'Synonym']$term
		C <- rbindlist(lapply(diagnosis_text, function(y){
			decompose(the_conceptId, diagnosis_text = y, CDB,
				SNOMED = SNOMED, noisy = noisy,
				omit_unmatched = omit_unmatched)
		}))
		C <- C[!duplicated(C)]
		setattr(C, 'class', c('SNOMEDfindings', 'data.table', 'data.frame'))
		return(C)
	}

	if (length(diagnosis_text) > 1){
		stop('This part of the function can only analyse a single diagnosis_text')
	}
	
	text <- std_term(diagnosis_text)
	
	#### LOCAL FUNCTIONS ####
	repl_ <- function(x){
		# replaces every word in x with @
		i <- nchar(x)
		while(x %like% '[[:alnum:]]' & i > 0){
			x <- sub('^([^\\@ ]+) | ([^\\@ ]+) | ([^\\@ ]+)$', ' @ ', x)
			i <- i - 1 # counter to prevent infinite loops
		}
		x
	}
	desc <- function(x, ...){
		descendants(x, SNOMED = SNOMED, TRANSITIVE = CDB$TRANSITIVE, ...)
	}
	anc <- function(x, ...){
		ancestors(x, SNOMED = SNOMED, TRANSITIVE = CDB$TRANSITIVE, ...)
	}
	addPlural <- function(X){
		X_PLURAL <- copy(X)
		#X_PLURAL <- X[nchar(gsub('[^ ]', '', term)) == 2]
		if (nrow(X_PLURAL) > 0){
			X_PLURAL[, term := sub('([a-rt-z]) $', '\\1s ', term)]
			X <- rbind(X, X_PLURAL)
			X[!duplicated(X)]
		} else {
			X
		}
	}

	#### INITIALISE OUTPUT ####
	if (noisy){
		message(paste0('\nDecomposing ', the_conceptId, ': ', text))
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
	
	# RELEVANT <- attrConcept(the_conceptId, SNOMED = SNOMED)
	# relevant_conceptId <- c(RELEVANT$sourceId, RELEVANT$destinationId)
	# attrConcept is slow, to rewrite
	relevant_conceptId <- c(SNOMED$RELATIONSHIP[sourceId %in%
		the_conceptId]$destinationId,
		SNOMED$RELATIONSHIP[destinationId %in% the_conceptId]$sourceId)
	relevant_conceptId <- union(children(
		setdiff(anc(relevant_conceptId), c(CDB$SCT_disorder,
		CDB$SCT_finding)), SNOMED = SNOMED, include_self = TRUE),
		desc(relevant_conceptId, include_self = TRUE))
	
	#### SPLIT INTO PARTS
	do_splitparts <- function(conceptId, text){
		
		# text must be lower case with space before and after
		splitpart <- function(OLD, split_var, split_text,
			the_typeId = NULL, SOURCE = CDB$FINDINGS, reverse = FALSE){
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
				myregex <- paste0('^ ([^ ].*) (', split_text,
					') (.*[^ ]) $')
				if (reverse){
					linked <- as.SNOMEDconcept(unique(SOURCE[term ==
						sub(myregex, ' \\1 ', text)]$conceptId),
						SNOMED = SNOMED)
				} else {
					linked <- as.SNOMEDconcept(unique(SOURCE[term ==
						sub(myregex, ' \\3 ', text)]$conceptId),
						SNOMED = SNOMED)
				}
				if (noisy){
					message(paste('Attempting to split by',
						paste(split_var, collapse = ' '),
						'at', paste(split_text, collapse = ' ')))
					if (length(linked) == 0){
						message('Unsucessful - no text match') 
					} else { 
						message(paste0('Linked: ',
							description(linked, SNOMED = SNOMED)$term))
					}
				}
				if (!is.null(the_typeId)){
					# allow only one related concept, and it must match
					# the SNOMED relationship specified in the_typeId
					linked_text <- linked
					linked <- NULL
					linked_sct <- as.SNOMEDconcept(character(0),
						SNOMED = SNOMED)
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
								c(anc(linked_sct), linked_sct,
								desc(linked_sct)))
						}
					} else {
						# return NULL if no text match, to avoid errors
						# in case there are multiple SCT matches
						linked <- NULL
						# linked <- linked_sct
					}
					
					# check it is a valid concept to link to, and only
					# allow a single linked concept
					if (!(is.null(linked))){
						if (length(linked) == 0){
							linked <- NULL
						} else {
							linked <- linked[linked %in%
								c(CDB$FINDINGS$conceptId,
								CDB$CAUSES$conceptId, CDB$BODY$conceptId)]
							if (length(linked) == 0){
								linked <- NULL
							} else {
								linked <- linked[1]
							}
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
						text <- sub(myregex, ' \\3 ', text)
					} else {
						text <- sub(myregex, ' \\1 ', text)
					}
				}
			}
			if (is.null(linked)){
				# unable to split
				return(OLD)
			} else {
				# finding the new rootID. If not found, keep as is.
				the_rootId <- CDB$FINDINGS[term == text]$conceptId
				if (length(the_rootId) == 0){
					the_rootId <- conceptId
				} else {
					if (noisy){
						message(paste('Successful split by',
							paste(split_var, collapse = ' '),
							'at', paste(split_text, collapse = ' '),
							'with SNOMED type as',
							paste(description(the_typeId,
							SNOMED = SNOMED)$term, collapse = ', ')))
						message(paste0('Linked: ',
							description(linked, SNOMED = SNOMED)$term))
						message(paste0('Remaining text:', text, '\n'))
					}
				}
				NEW <- data.table(text = text, rootId = the_rootId[1],
					temp = linked, other_attr = repl_(text))
				setnames(NEW, 'temp', split_var)
				return(rbind(OLD, NEW, fill = TRUE))
			}
		}
		
		OUT <- BLANK_OUTPUT()
		
		# Attempt up to one split in the following order:
		
		# DUE TO
		OUT <- splitpart(OUT, 'due_to',
			'with|cooccurrent and due to|co-occurrent and due to|resulting from',
			CDB$SCT_dueto)
		OUT <- splitpart(OUT, 'due_to', 'due to|caused by',
			c(CDB$SCT_cause, CDB$SCT_dueto), SOURCE = CDB$CAUSES)
		OUT <- splitpart(OUT, 'due_to', 'causing|resulting in|complicated by',
			CDB$SCT_cause, SOURCE = CDB$CAUSES, reverse = TRUE)
		# Caused by a non-finding (e.g. an organism or substance)
		OUT <- splitpart(OUT, 'due_to', 'by',
			CDB$SCT_cause, SOURCE = CDB$OTHERCAUSE)
		OUT <- splitpart(OUT, 'due_to', 'causing|resulting in',
			reverse = TRUE)
		OUT <- splitpart(OUT, 'due_to', 'induced',
			CDB$SCT_cause, SOURCE = CDB$OTHERCAUSE, reverse = TRUE)
		OUT <- splitpart(OUT, 'due_to', 'due to|caused by')

		# WITH
		OUT <- splitpart(OUT, 'with', 'associated with', CDB$SCT_assoc)
		OUT <- splitpart(OUT, 'with',
			'cooccurrent with|co-occurrent with|and')
		OUT <- splitpart(OUT, 'with', 'with|in')
		OUT <- splitpart(OUT, 'with', 'associated',
			SOURCE = CDB$CAUSES, reverse = TRUE)

		# AFTER
		OUT <- splitpart(OUT, 'after',
			'due to and following|as a sequela of|as a late effect of',
			CDB$SCT_after, SOURCE = CDB$CAUSES)
		OUT <- splitpart(OUT, 'after', 'following|after',
			CDB$SCT_after, SOURCE = CDB$CAUSES)
		OUT <- splitpart(OUT, 'after', 'following|after')

		
		# WITHOUT
		OUT <- splitpart(OUT, 'without', 'without|but without')
	
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
	C[, partId := rootId]
	C[, parttext := text]
	
	#### EXPAND LINES USING ANCESTORS ####

	# Candidates for finding ancestor match
	# Add pluralised single-word concepts (e.g. fracture --> fractures)
	A <- addPlural(CDB$FINDINGS[conceptId %in% anc(the_conceptId)])

	e_ancestors <- function(DATALINE){
		# Returns a data.table containing the original DATALINE and
		# optionally additional data lines with decompositions by
		# searching for different ancestors
		use <- sapply(A$term, function(x) DATALINE$text %like% x)
		ANC <- A[use == TRUE, list(conceptId, term)]
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
			if (noisy){
				message('\nNo valid ancestors, trying morphologies')
			}
			if (nrow(CDB$MORPH) > 0){
				use <- sapply(CDB$MORPH$term, function(x){
					DATALINE$text %like% x
				})
				ANC <- CDB$MORPH[use == TRUE, list(conceptId, term)]
			}
			if (nrow(ANC) > 0){
				return(rbindlist(lapply(1:nrow(ANC), function(x){
					new_text <- sub(ANC[x]$term, repl_(ANC[x]$term),
						DATALINE$text)
					OUTPUT <- rbind(DATALINE, DATALINE)
					OUTPUT[1, other_attr := new_text]
					OUTPUT[1, text := ANC[x]$term]
					OUTPUT[1, rootId := ANC[x]$conceptId]
				})))
			} else {
				if (noisy){
					message('\nNo valid morphologies')
				}
				return(DATALINE)
			}
		}
	}
	
	if (nrow(A) > 0){
		TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_ancestors(C[x]))) 
		C <- TEMP
		if (noisy){
			message('\nFinding ancestors or morphologies')
			print(C)
		}
	} else {
		if (noisy){
			message('\nNo ancestors or morphologies found')
		}
	}
	
	#### BODY SITE
	
	e_body <- function(DATALINE, B, the_body_siteId, inc_modelled){
		# Arguments:
		# DATALINE = partial decomposition line
		# B = portion of body table
		# the_body_siteId = modelled body site ID
		# inc_modelled = whether to include modelled body site concept
		#   as well as the concept matched by text matching. 
		#   Set to FALSE if the original SNOMED concept has more than
		#   one body site, as the wrong one may be matched.
		#   Currently switched off as it is sometimes incorrect
		# 
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
			use <- sapply(B$term, function(x){
				DATALINE$other_attr %like% x
			})
			BOD <- B[use == TRUE, list(conceptId, term, required_laterality)]
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
						LATSELECT <- CDB$LATERALITY[conceptId ==
							CDB$latConcepts[
							BOD[x]$required_laterality]]
						if (nrow(LATSELECT) > 0){
							use <- sapply(LATSELECT$term, function(x){
								DATALINE$other_attr %like% x
							})
							LATSELECT <- LATSELECT[use == TRUE,
								list(conceptId, term)]
						}
						if (nrow(LATSELECT) > 0){
							# Only permit one row for simplicity
							# Remove laterality word from other_attr
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
					# body site and inc_modelled is TRUE, add as well.
					if (BOD[x]$required_laterality ==
						B[exact == TRUE]$required_laterality[1] &
						inc_modelled){
						OUTPUT <- rbind(DATALINE, DATALINE)
						OUTPUT[, body_site := c(the_body_siteId,
							BOD[x]$conceptId)]
					} else {
						# Otherwise just add the body site as found
						OUTPUT <- copy(DATALINE)
						OUTPUT[, body_site := BOD[x]$conceptId]
					}
					OUTPUT[, other_attr := new_text]
					if (BOD[x]$required_laterality %in%
						c('Left', 'Right', 'Bilateral')){
						OUTPUT[, laterality :=
						CDB$latConcepts[BOD[x]$required_laterality]]
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
	body_siteIds <- setdiff(relatedConcepts(the_conceptId,
		typeId = CDB$SCT_findingsite, SNOMED = SNOMED),
		SNOMEDconcept(c('91689009', '278195005', '714488006',
		'91690000'), SNOMED = SNOMED))
	# Remove vague body sites:
	# 91689009 | Body system structure (body structure)
	# 278195005 | Entire body system (body structure)
	# 714488006 | Structure of subdivision of organ system (body structure)
	# 91690000 | Entire subdivision of organ system (body structure)
	
	# the_body_siteId should only be one but a few concepts have
	# multiple body sites, so allow multiple
	body_siteTerms <- paste0(' ',
		tolower(SNOMED$DESCRIPTION[conceptId %in% body_siteIds &
		typeId == bit64::as.integer64('900000000000003001') &
		active == TRUE]$term))

	if (length(body_siteIds) > 0){
		if (length(body_siteIds) == 2 &
			(sub(' left ', '', body_siteTerms[1]) ==
			sub(' right ', '', body_siteTerms[2]) | 
			sub(' left ', '', body_siteTerms[2]) ==
			sub(' right ', '', body_siteTerms[1]))){
			the_laterality <- 'Bilateral'
		} else {
			the_laterality <- CDB$BODY[
				conceptId %in% body_siteIds]$laterality[1]
		}
		# keep only the first the_laterality so that it has cardinality 1
		B <- CDB$BODY[conceptId %in% c(anc(body_siteIds),
			desc(body_siteIds, include_self = TRUE))]
		B[, required_laterality := ifelse(
			laterality == the_laterality |
			(laterality == 'No laterality' &
			the_laterality == 'Lateralisable'),
			'Included', the_laterality)]
		# Remove concepts with inconsistent laterality
		B <- B[required_laterality == 'Included' |
			(required_laterality == the_laterality &
			laterality %in% c('Lateralisable', 'No laterality'))]
		if (nrow(B) > 0){
			B[, exact := conceptId %in% body_siteIds]
			# Add pluralised single-word concepts (e.g. rib --> ribs)
			B <- addPlural(B)

			TEMP <- rbindlist(lapply(1:nrow(C), function(x){
				if (length(body_siteIds) == 1){
					# e_body(C[x], B, body_siteIds, inc_modelled = TRUE)
					# inc_modelled is switched off as the modelled
					# SNOMED CT body location is sometimes incorrect
					# e.g. 'Pain in female pelvis' finding site = 
					# '609617007 | Structure of pelvic segment of trunk'
					e_body(C[x], B, body_siteIds, inc_modelled = FALSE)
				} else {
					rbindlist(lapply(body_siteIds, function(body_siteId){
						e_body(C[x], B, body_siteId, inc_modelled = FALSE)
					}))
				}
			}))
			C <- TEMP
			
			if (noisy){
				message('\nFinding body sites')
				print(C)
			}
		} else {
			if (noisy){
				message('\nUnable to extract body site for this disorder')
			}
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
		the_causeIds <- relatedConcepts(DATALINE$partId,
			typeId = CDB$SCT_cause, SNOMED = SNOMED)
		if (length(the_causeIds) > 0){
			for (i in seq_along(the_causeIds)){
				CAU <- CDB$CAUSES[conceptId %in% the_causeIds[i]]
				if (nrow(CAU) > 0){
					use <- sapply(CAU$term, function(x){
						DATALINE$other_attr %like% x
					})
					CAU <- CAU[use == TRUE, list(conceptId, term)]
				}
				if (nrow(CAU) > 0){
					DATALINE <- rbindlist(lapply(1:nrow(CAU), function(x){
						new_text <- sub(CAU[x]$term, repl_(CAU[x]$term),
							DATALINE$other_attr)
						OUTPUT <- copy(DATALINE)
						OUTPUT[, other_attr := new_text]
						OUTPUT[, due_to := the_causeIds[i]]
						OUTPUT
					}))
				}
			}
		}
		DATALINE
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
		use <- sapply(CDB$SEVERITY$term, function(x){
			DATALINE$other_attr %like% x
		})
		SEV <- CDB$SEVERITY[use == TRUE, list(conceptId, term)]
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

	if (nrow(CDB$SEVERITY) > 0){
		TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_severity(C[x])))
		C <- TEMP
		if (noisy){
			message('\nFinding severity')
			print(C)
		}
	}

	#### STAGE

	e_stage <- function(DATALINE){
		# Returns a data.table containing the original DATALINE and
		# with stage information extracted.
		use <- sapply(CDB$STAGE$term, function(x){
			DATALINE$other_attr %like% x
		})
		STA <- CDB$STAGE[use == TRUE, list(conceptId, term)]
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

	if (nrow(CDB$STAGE) > 0){
		TEMP <- rbindlist(lapply(1:nrow(C), function(x) e_stage(C[x])))
		C <- TEMP
		if (noisy){
			message('\nFinding stage')
			print(C)
		}
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
			while (i < length(the_other_attr) &
				the_other_attr[i] == '@'){
				i <- i + 1
			}
			j <- i
			# Locate end of phrase
			while (j < length(the_other_attr) &
				the_other_attr[j] != '@'){
				j <- j + 1
			}
			# Try to match sequence to a SNOMED concept
			match <- FALSE
			while (!match & j > i){
				j <- j - 1
				to_match <- paste0(' ', paste(the_other_attr[i:j],
					collapse = ' '), ' ')
				OTHER <- rbind(
					CDB$QUAL[term %in% to_match, list(conceptId, term)],
					CDB$FINDINGS[term %in% to_match &
						conceptId %in% relevant_conceptId &
						!(conceptId %in% c(DATALINE$rootId,
						DATALINE$partId)), list(conceptId, term)],
					CDB$CAUSES[term %in% to_match &
						conceptId %in% relevant_conceptId &
						!(conceptId %in% c(DATALINE$rootId,
						DATALINE$partId)), list(conceptId, term)],
					CDB$BODY[term %in% to_match &
						conceptId %in% relevant_conceptId &
						!(conceptId %in% DATALINE$body_site),
						list(conceptId, term)])
				if (nrow(OTHER) > 0){
					match <- TRUE
					DATALINE[, other_conceptId :=
						paste(other_conceptId,
							paste(unique(OTHER$conceptId),
							collapse = '|'))]
					i <- j
				}
			}
			if (match == FALSE){
				if (the_other_attr[i] %in% CDB$stopwords){
					# pass
				} else {
					# keep as a single word for now - eventually will
					# exclude
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

	C <- C[!duplicated(C)]

	# Add original conceptId
	C[, origId := the_conceptId]
	
	# Rename text to roottext
	setnames(C, 'text', 'roottext')
	
	# Remove any entries where rootId == origId
	C <- C[!(rootId == origId)]
	
	# Remove any entries where other_conceptId is not matched to a
	# SNOMED CT concept or if there are no attributes, or if the concept
	# itself it included as an attribute
	if (omit_unmatched){
		C <- C[!(other_conceptId %like% '[A-Za-z]')]
		C <- C[!is.na(with) | !is.na(due_to) | !is.na(after) |
			!is.na(without) | !is.na(body_site) | !is.na(severity) |
			!is.na(stage) | !is.na(laterality) |
			other_conceptId %like% '[0-9]']
	}
	
	# Now expand where there are alternative entries
	done <- FALSE
	while (!done){
		DATA1 <- copy(C)
		DATA2 <- copy(C)
		DATA1[, other_conceptId := sub(' ([^ ]+)\\|([^ ]+) ', ' \\1 ',
			paste0(' ', other_conceptId, ' '))]
		DATA2[, other_conceptId := sub(' ([^ ]+)\\|([^ ]+) ', ' \\2 ',
			paste0(' ', other_conceptId, ' '))]
		if (all(DATA1$other_conceptId == DATA2$other_conceptId)){
			done <- TRUE
		}
		C <- rbind(DATA1, DATA2)
		C <- C[!duplicated(C)]
	}
	C[, other_conceptId := gsub('^ +| +$', '', other_conceptId)]
	C[, other_attr := NULL]
	C[, parttext := NULL]
	C <- C[!duplicated(C)]
	setattr(C, 'class', c('SNOMEDfindings', 'data.table', 'data.frame'))
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

#' Print method for output of 'decompose' function
#'
#' @param x SNOMEDfindings object to display
#' @param ... not used
#' @return TRUE
#' 
#' @export
print.SNOMEDfindings <- function(x, ...){
	SNOMED <- NULL
	try(SNOMED <- getSNOMED(), silent = TRUE)
	D <- x
	if (nrow(D) == 0){
		cat('No rows in SNOMED finding object.\n')
		return(NULL)
	}
	# Prints a SNOMED CT finding object
	show_concept <- function(x, offset = 16){
		x <- as.SNOMEDconcept(x, SNOMED = SNOMED)
		if (length(x) == 1){
			if (x %in% SNOMED$DESCRIPTION$conceptId){
				output <- paste0(x, " | ", description(x,
					SNOMED = SNOMED)$term)
			} else {
				output <- as.character(x)
			}
		} else {
			output <- ''
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
		
		if ('origId' %in% names(D)){
			cat(paste0('\n', show_concept(D[i]$origId, 0)))
			cat(paste(c('\n', rep('-', getOption('width'))), collapse = ''))
		}
		if ('rootId' %in% names(D)){
			cat('\nRoot :', show_concept(D[i]$rootId, 6))
		}
		if ('conceptId' %in% names(D)){
			cat('\nConcept :', show_concept(D[i]$conceptId, 6))
		}
		if (all(c('onset_range_start', 'onset_range_end') %in% names(D))){
			if (!is.na(D[i]$onset_range_start) &
				!is.na(D[i]$onset_range_end)){
				cat('\n- Onset date:', simplify_date_range(
					D[i]$onset_range_start, D[i]$onset_range_end))
			}
		}
		show <- function(attr_name){
			if (attr_name %in% names(D)){
				if (!is.na(D[i][[attr_name]])){
					attr_displayname <- paste0(
						toupper(substr(attr_name, 1, 1)),
						substr(gsub('_', ' ', attr_name), 2, 100))
					if (bit64::is.integer64(D[i][[attr_name]])){
						cat('\n-', attr_displayname, ':',
							show_concept(D[i][[attr_name]],
							nchar(attr_name) + 3))
					} else if (is.list(D[i][[attr_name]])){
						if (length(D[i][[attr_name]][[1]]) > 0){
							cat('\n-', attr_displayname, ':')
							for (j in 1:length(D[i][[attr_name]][[1]])){
								cat('\n  -',
									show_concept(D[i][[attr_name]][[1]][j], 4))
							}
						}
					}
				}
			}
		}
		show('body_site')
		show('severity')
		show('stage')
		show('laterality')
		show('with')
		show('due_to')
		show('causing')
		show('after')
		show('without')
		show('attributes')
		if ('other_conceptId' %in% names(D)){
			# other conceptIds as a character vector 
			# (used for CSV file import / export)
			if (!is.na(D[i]$other_conceptId)){
				if (D[i]$other_conceptId != ''){
					cat('\n- Other attributes :')
					attributes <- strsplit(D[i]$other_conceptId, ' ')[[1]]
					for (j in 1:length(attributes)){
						cat('\n  -', show_concept(attributes[j], 4))
					}
				}
			}
		}
		cat('\n')
	}
	invisible(TRUE)
}


simplify_date_range <- function(start, end){
	# returns a simple text representation of the range between
	# two dates
	if (start == end){
		as.character(end, format = '%d %b %Y')
	} else if (month(start) == 1 & month(end) == 12 &
		mday(start) == 1 & mday(end) == 31){
		if (year(start) == year(end)){
			as.character(end, format = '%Y')
		} else {
			paste(as.character(start, format = '%Y'), '-',
				as.character(end, format = '%Y'))
		}
	} else if (year(start) == year(end) & month(start) == month(end) &
		mday(start) == 1 & mday(end + 1) == 1){
		as.character(start, format = '%b %Y')
	} else {
		paste(as.character(start, format = '%d %b %Y'), '-',
			as.character(end, format = '%d %b %Y'))
	}
}
