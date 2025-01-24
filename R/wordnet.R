#' Download WordNet to assist concept database creation
#'
#' Downloads the WordNet thesaurus and converts it into a format to be
#' used by addWordnet to add extra synonyms to a concept database.
#'
#' @param wordnet_url URL or filepath to WordNet tar.gz file
#' @param wn_categories WordNet categories from which to extract data
#' @return data.table containing WordNet data from appropriate
#'   categories, in the format: cat (character), wordnetId (integer64),
#'   synonyms (list), parents (list), adj (list)
#' @export
#' @seealso addWordnet
#' @family CDB functions
#' @references \url{https://wordnet.princeton.edu/}
#' @examples
#' # Not run
#' # WORDNET <- downloadWordnet()
downloadWordnet <- function(
	wordnet_url = 'https://wordnetcode.princeton.edu/wn3.1.dict.tar.gz',
	wn_categories = c('noun.body', 'noun.state', 'noun.process',
		'noun.animal', 'noun.plant', 'noun.phenomenon')){
			
	# Declare symbols to avoid R check error
	wordnetId <- synonyms <- adj <- NULL
			
	# Download Wordnet files and returns a data.table
	wordnet_filename <- paste0(tempdir(), '/wn3.1.dict.tar.gz')
	download.file(wordnet_url, wordnet_filename)
	untar(wordnet_filename, exdir = paste0(tempdir(), '/WordNet/'))
	wordnet_filepath <- paste0(tempdir(), '/WordNet/dict/dbfiles/')
	
	# Load Wordnet adj.pert and noun.body, noun.state, noun.process
	#
	# noun files format:
	# { synonym, another_synonym, synonym1, ..., hypernym,@ part_of,#p
	# (definition) }
	# synonym1 = sense 2
	# [ tibia, adj.pert:tibial,+ ] shinbone, ...
	# square brckets around things which go together, ,+ for pert

	WN <- rbindlist(
		lapply(wn_categories, function(x){
			wn <- scan(paste0(wordnet_filepath, x), what = 'character',
				sep = '\n')	
			# Generate synsets, and add adjectives 
			# Strip everything before { and after )
			# Keep nouns and pertaining adjectives
			wn <- wn[wn %like% '^\\{']
			syn <- gsub('(\\{|\\[|\\])', '',
				sub('^([^\\(]+)\\(.+$', '\\1', wn))
			def <- gsub('(\\{|\\[|\\])', '',
				sub('^([^\\(]+)\\((.+)\\) *\\} *$', '\\2', wn))
			data.table(cat = x, raw = syn, def = def)
		})
	)

	# Generate synsets wach with a unique ID
	# For our purposes we are using parts of and hypernyms interchangeably
	# Also including adjective pertainyms in the same synset
	WN[, wordnetId := bit64::as.integer64(1:length(raw))]

	extract_synonyms <- function(x){
		x <- unlist(x)
		synonym <- x[x %like% ',$|^adj.pert.*,\\+$']
		synonym <- sub('adj.pert:', '', synonym)
		synonym <- gsub('_', ' ', synonym)
		sub(',$|,\\+$', '', synonym)
	}
	extract_adj <- function(x){
		x <- unlist(x)
		adj <- x[x %like% '^adj.pert.*,\\+$']
		adj <- sub('adj.pert:', '', adj)
		adj <- gsub('_', ' ', adj)
		sub(',$|,\\+$', '', adj)
	}
	extract_parents <- function(x){
		x <- unlist(x)
		parents <- x[x %like% ',@$|,\\#p$']
		# only parents from the same class / type
		parents <- parents[!(parents %like% ':')]
		parents <- gsub('_', ' ', parents)
		sub(',@$|,\\#p$', '', parents)
	}

	WN[, synonyms := lapply(strsplit(raw, ' '), extract_synonyms)]
	WN[, parents := lapply(strsplit(raw, ' '), extract_parents)]
	WN[, adj := lapply(strsplit(raw, ' '), extract_adj)]

	# Outdegrees e.g. for body part noun

	#~    linkid                   link recurses
	#~ 1       1               hypernym        1 = supertype
	#~ 2       2                hyponym        1
	#~ 3       3      instance hypernym        1
	#~ 4       4       instance hyponym        1
	#~ 5      11           part holonym        1 = parts of X
	#~ 6      12           part meronym        1 = X is part of
	#~ 7      13         member holonym        1
	#~ 8      14         member meronym        1
	#~ 9      15      substance holonym        1
	#~ 10     16      substance meronym        1
	#~ 11     21                 entail        1
	#~ 12     23                  cause        1
	#~ 13     30                antonym        0
	#~ 14     40                similar        0
	#~ 15     50                   also        0
	#~ 16     60              attribute        0
	#~ 17     70             verb group        0
	#~ 18     71             participle        0
	#~ 19     80              pertainym        0
	#~ 20     81             derivation        0 = adj pertaining to X
	#~ 21     91        domain category        0
	#~ 22     92 domain member category        0
	#~ 23     93          domain region        0
	#~ 24     94   domain member region        0
	#~ 25     95           domain usage        0
	#~ 26     96    domain member usage        0
	#~ 27     97                 domain        0
	#~ 28     98                 member        0
	
	# Clean up temporary files
	unlink(wordnet_filename)
	unlink(paste0(tempdir(), '/WordNet/'), recursive = TRUE)
	WN
}

#' Use WordNet to assist concept database creation
#'
#' Adds terms from a WordNet thesaurus to a concept database, matching
#' on term. It is recommended to restrict the wordnet categories to
#' ensure that words with multiple meanings are not linked to the wrong
#' synonym. This function also corrects some known errors in WordNet to
#' avoid them being passed on to the CDB; currently this applies to 
#' 'allergy = allergic reaction', 'cuneiform bone = triquetral'
#' and 'trauma' = 'injury', but more corrections can be done if needed.
#'
#' @param CDB_TABLE data.frame or data.table with columns
#'   conceptId (integer64) and term (character, with space before
#'   and after) containing existing descriptions in the CDB
#' @param wn_categories WordNet categories to use
#' @param WN WordNet data.table as returned by downloadWordnet
#' @param CHECK_TABLE other table in the same format as CDB_TABLE
#'   to check for WordNet synonyms that link to another unrelated
#'   concept, where this synonym will be excluded because of the risk
#'   of errors
#' @param errors_to_remove list of character vectors of length two
#'   containing synonym pairs to be removed. The first entry of the
#'   pair will be removed from the WordNet file before it is used for
#'   adding to CDB
#' @param noisy whether to output test comments
#' @return CDB_TABLE with extra rows for Wordnet synonyms
#' @export
#' @seealso downloadWordnet
#' @family CDB functions
#' @references \url{https://wordnet.princeton.edu/}
#' @examples
#' WORDNET <- data.table::data.table(cat = c('noun.body', 'noun.state'),
#'   wordnetId = bit64::as.integer64('1', '2'),
#'   synonyms = list(c('heart', 'pump', 'ticker'),
#'   c('infection', 'infectious')),
#'   parents = list('cardiovascular system',
#'   'pathologic process'), 
#'   adj = list('cardiac', 'infectious'))
#' # Add Wordnet synonyms to a concept database table
#' SNOMED <- sampleSNOMED()
#' CDB_TABLE <- description(c('Heart', 'Infection'),
#'   include_synonyms = TRUE)[type == 'Synonym',
#'   list(conceptId, term = paste0(' ', tolower(term), ' '))]
#' addWordnet(CDB_TABLE, 'noun.state', WORDNET)
addWordnet <- function(CDB_TABLE, wn_categories, WN,
	CHECK_TABLE = NULL, errors_to_remove = list(
	c('allergy', 'allergic reaction'),
	c('allergic', 'allergic reaction'),
	c('trauma', 'injury'),
	c('traumatic', 'injury'),
	c('skinny', 'skin'),
	c('cuneiform bone', 'triquetral bone'),
	c('upset', 'disorder'),
	c('disorderliness', 'disorder')), noisy = TRUE){
		
	# Define symbols for R check
	term <- synonyms <- cat <- conceptId <- wordnetId <- NULL
	OK <- conceptIds_WN <- conceptIds <- NULL
	
	if (noisy) message('Adding categories for ', paste0(wn_categories,
		collapse = ', '))

	D <- as.data.table(CDB_TABLE)
	WNLONG <- WN[cat %in% wn_categories, list(term = synonyms[1][[1]]),
		by = wordnetId]
	WNLONG[, term := sub('[1-9]$', '', term)]
	WNLONG[, term := paste0(' ', gsub('_', ' ', term), ' ')]
	
	# Correct known errors in Wordnet
	remove <- function(WNLONG, word_to_remove, base_word){
		word_to_remove <- paste0(' ', gsub('^ +| $', '',
			word_to_remove), ' ')
		base_word <- paste0(' ', gsub('^ +| $', '', base_word), ' ')
		TOREMOVE <- WNLONG[wordnetId %in%
			WNLONG[term %in% base_word]$wordnetId][
			term %in% word_to_remove]
		if (nrow(TOREMOVE) > 0){
			message('Removing ', word_to_remove, ' from ',
				nrow(TOREMOVE), ' synonym relations.')
			WNLONG[!(wordnetId %in% TOREMOVE$wordnetId &
				term %in% word_to_remove)]
		} else {
			WNLONG
		}
	}
	
	for (i in 1:length(errors_to_remove)){
		WNLONG <- remove(WNLONG, errors_to_remove[[i]][1],
			errors_to_remove[[i]][2])
	}
	
	# Match up WNLONG with SNOMED CT concepts by term
	MERGELINK <- merge(WNLONG, D[!duplicated(D)], by = 'term')
	MERGELINK <- MERGELINK[!duplicated(MERGELINK)]
	setindexv(MERGELINK, 'term')
	
	# Filter to WordNet ID groups that contain at least one member not
	# linked to an existing SNOMED CT description
	if (is.null(CHECK_TABLE)){
		CHECK_TABLE <- D
	} else {
		CHECK_TABLE <- as.data.table(CHECK_TABLE)[,
			list(conceptId, term)]
	}
	setindexv(CHECK_TABLE, c('term', 'conceptId'))

	# Omit synonym groups that link to two or more distinct SNOMED
	# concepts that do not already share a synonym
	EXISTING_SYNGROUPS <- CHECK_TABLE[, list(conceptId), by = term]
	EXISTING_SYNGROUPS[, conceptId := as.character(conceptId)]
	setkeyv(EXISTING_SYNGROUPS, c('term', 'conceptId'))
	EXISTING_SYNGROUPS <- EXISTING_SYNGROUPS[
		!duplicated(EXISTING_SYNGROUPS)]
	EXISTING_SYNGROUPS <- EXISTING_SYNGROUPS[,
		list(conceptIds = list(conceptId)), by = term]
	MERGELINK_SYNGROUPS <- MERGELINK[, list(conceptIds_WN = list(
		sort(unique(as.character(conceptId))))),
		by = term]
	MERGELINK_SYNGROUPS <- merge(MERGELINK_SYNGROUPS,
		EXISTING_SYNGROUPS, by = 'term')
	MERGELINK_SYNGROUPS[, OK := length(setdiff(conceptIds_WN[[1]],
		conceptIds[[1]])) == 0, by = term]

	# Remove any synonyms groups from Wordnet that contain more
	# SNOMED CT concepts than the synonym sets in SNOMED CT itself
	# i.e. avoid introducing new erroneous links
	MERGELINK <- MERGELINK[!(as.character(conceptId) %in% 
		unlist(lapply(MERGELINK_SYNGROUPS[OK == FALSE]$conceptIds_WN,
		function(x) as.character(x))))]
	
	# Add WN synonyms
	D <- rbind(D, MERGELINK[conceptId %in% D$conceptId,
		list(conceptId, term)], fill = TRUE)
	D[!duplicated(D)]
}
