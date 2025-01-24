#' Sample concept table from SNOMED CT dictionary
#'
#' A sample of the SNOMED CT concept table. 
#' 
#' @name SNOMED_CONCEPT
#' @aliases CONCEPT
#' @importFrom utils data
#' @docType data
#' @usage data(CONCEPT)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \describe{
#'   \item{id}{ integer64: SNOMED CT conceptId (primary key)} 
#'   \item{moduleId}{ integer64: class of SNOMED CT concept (whether it is used for recording information or is a metadata concept)}
#'   \item{definitionStatusId}{ integer64: 900000000000074008 = primitive concept, 900000000000073002 = defined by conditions}
#'   \item{effectiveTime}{ IDate: when the concept became active}
#'   \item{active}{ logical: whether this concept is currently active}
#' }
#' 
#' @family sampleSNOMED
#' @examples
#' # Show properties of the CONCEPT table
#' data('CONCEPT')
#' str(CONCEPT)
"CONCEPT"
