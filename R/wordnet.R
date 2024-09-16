#' Download WordNet to assist concept database creation
#'
#' Downloads the WordNet thesaurus and converts it into a format to be
#' used by addWordNet to add extra synonyms to a concept database.
#'
#' @param wordnet_url URL or filepath to WordNet tar.gz file
#' @param wn_categories WordNet categories from which to extract data
#' @return data.table containing WordNet data from appropriate
#'   categories, in the format: cat (character), wordnetId (integer64),
#'   synonyms (list), parents (list), adj (list)
#' @export
#' @seealso [addWordNet()]
#' @references \url{https://wordnet.princeton.edu/}
#' @examples
#' # Not run
#' # WORDNET <- downloadWordNet()
downloadWordNet <- function(
	wordnet_url = 'https://wordnetcode.princeton.edu/wn3.1.dict.tar.gz',
	wn_categories = c('noun.body', 'noun.state', 'noun.process',
		'noun.animal', 'noun.plant', 'noun.phenomenon')){
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
#' synonym.
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
#' @return CDB_TABLE with extra rows for Wordnet synonyms
#' @export
#' @seealso [downloadWordNet()]
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
#'   .(conceptId, term = paste0(' ', tolower(term), ' '))]
#' addWordNet(CDB_TABLE, 'noun.state', WORDNET)
addWordNet <- function(CDB_TABLE, wn_categories, WN,
	CHECK_TABLE = NULL){
	term <- synonyms <- cat <- conceptId <- wordnetId <- NULL
	D <- as.data.table(CDB_TABLE)
	WNLONG <- WN[cat %in% wn_categories, .(term = synonyms[1][[1]]),
		by = wordnetId]
	WNLONG[, term := sub('[1-9]$', '', term)]
	WNLONG[, term := paste0(' ', gsub('_', ' ', term), ' ')]
	
	# Match up WNLONG with SNOMED CT concepts by term
	MERGED <- merge(WNLONG, D[!duplicated(D)], by = 'term')
	MERGED <- MERGED[!duplicated(MERGED)]
	
	# Filter to WordNet ID groups that contain at least one member not
	# linked to an existing SNOMED CT description
	if (is.null(CHECK_TABLE)) CHECK_TABLE <- D[0]
	CHECK <- merge(WNLONG, rbind(D[!duplicated(D)],
		CHECK_TABLE[!duplicated(CHECK_TABLE)]), by = 'term',
		all.x = TRUE, allow.cartesian = TRUE)
	CHECK[, already := all(!is.na(conceptId)), by = wordnetId]
	
	# Do not use WordNet matches where all terms already in SNOMED CT
	MERGELINK <- MERGED[!(wordnetId %in% CHECK[already == TRUE]$wordnetId),
		.(wordnetId, conceptId)]
	MERGELINK <- MERGELINK[!duplicated(MERGELINK)]
	
	# Replace WNLONG concept IDs with SNOMED concept IDs
	MERGELINK <- merge(MERGELINK, WNLONG)[,
		.(conceptId, term)]
	
	# Add WN synonyms
	D <- rbind(D, MERGELINK[conceptId %in% D$conceptId])
	return(D[!duplicated(D)])
}
