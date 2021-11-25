 #' Sample mappings from Read to SNOMED CT
#'
#' A sample of a mapping table derived from NHS Digital maps. 
#' Contains concepts in Read Clinical Terms Version 2 and 
#' Clinical Terms Version 3 that map to a set of SNOMED CT
#' concepts, according to a supplied mapping file. 
#' The source data are available from the NHS Digital Technology
#' Reference data Update Distribution
#' \url{https://isd.digital.nhs.uk/trud/user/guest/group/0/pack/9/subpack/9/releases}.
#' 
#' @name READMAPS
#' @importFrom utils data
#' @docType data
#' @usage data(READMAPS)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \itemize{
#'   \item{conceptId}{ integer64: SNOMED CT conceptId (primary key)} 
#'   \item{read2_code}{ list: character list of 7-character Read V2 codes}
#'   \item{read2_term}{ list: character list of Read V2 terms}
#'   \item{ctv3_concept}{ list: character list of CTV3 concept codes}
#'   \item{ctv3_termid}{ list: character list of CTV3 term description codes}
#' }
#' 
#' @seealso loadREADMAPS, getMaps
#' @examples
#' # Show properties of the READMAPS table
#' data(READMAPS)
#' str(READMAPS)
"READMAPS"
