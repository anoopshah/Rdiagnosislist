#' Download Orphanet to assist with adding synonyms for rare diseases
#'
#' Downloads the Orphanet nomenclature pack and converts it into a
#' format to be appended to MANUAL_SYNONYMS and used in concept
#' database creation.
#'
#' @param orphanet_url URL or filepath to Orphanet zip file
#' @param masterfile_name name of xls file containing Orphanet synonyms.
#'   If omitted, it is assumed to be the only file with 'MasterFile'
#'   in its name, and the program will search for it in the Orphanet
#'   zip file.
#' @param SNOMED environment containing SNOMED CT dictionary
#' @return data.table containing Orphanet synonyms in the format
#' @importFrom readxl read_excel
#'   
#' @export
#' @family CDB functions
#' @seealso downloadWordnet, MANUAL_SYNONYMS
#' @references \url{https://www.orphadata.com/pack-nomenclature/}
#' @examples
#' # Not run
#' # ORPHANET <- downloadOrphanet()
downloadOrphanet <- function(
	orphanet_url = 'https://www.orphadata.com/data/nomenclature/packs/Orphanet_Nomenclature_Pack_EN.zip',
	masterfile_name = NULL, SNOMED = getSNOMED()){

	# Declare R symbols to avoid R check error
	ORPHAcode <- PreferredTerm <- Synonyms <- term <- NULL
	conceptId <- synonym <- typeId <- NULL

	# Download Orphanet files and returns a data.table
	orphanet_filename <- paste0(tempdir(), '/orphanet.zip')
	download.file(orphanet_url, orphanet_filename)
	orphanet_filepath <- paste0(tempdir(), '/orphanet/')
	unzip(orphanet_filename, exdir = orphanet_filepath,
		junkpaths = TRUE)
	
	if (is.null(masterfile_name)){
		x <- dir(orphanet_filepath)
		x <- x[x %like% 'MasterFile'][1]
	}
	A <- as.data.table(readxl::read_excel(paste0(orphanet_filepath, x)))
	B <- rbind(A[, list(ORPHAcode, term = PreferredTerm)],
		A[, list(ORPHAcode, term = Synonyms)])
	B <- B[!duplicated(B)]
	B <- B[!is.na(term)]
	C <- merge(B, SNOMED$DESCRIPTION, by = 'term')[,
		list(ORPHAcode, conceptId)]
	C <- C[!duplicated(C)]
	OUT <- merge(B[, list(ORPHAcode, synonym = term)], C,
		by = 'ORPHAcode')[, list(conceptId, synonym)]
	# merge with preferred terms in SNOMED to generate new synonym pairs
	OUT <- merge(OUT, SNOMED$DESCRIPTION[typeId %in%
		bit64::as.integer64('900000000000003001'),
		list(conceptId, term = sub(' \\([^\\)]+\\)$', '', term))],
		by = 'conceptId')
	OUT <- OUT[, list(synonym = gsub('^ +| +$', '', std_term(synonym,
		hyphens_to_space = TRUE)),
		snomed = gsub('^ +| +$', '', std_term(term,
		hyphens_to_space = TRUE)),
		bidirectional = FALSE)][!is.na(synonym)]
	OUT[!duplicated(OUT)]
}
