#' Sample manual synonym table to assist in creation of concept database
#' 
#' @name MANUAL_SYNONYMS
#' @docType data
#' @importFrom utils data
#' @usage data(MANUAL_SYNONYMS)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \describe{
#'   \item{synonym}{ character: Synonym, in lower case if case is unimportant} 
#'   \item{snomed}{ character: SNOMED CT term, in lower case if case is unimportant}
#'   \item{bidirectional}{ boolean: whether synonym can only ever mean
#'     snomed. Not to be used for abbreviations or acronyms.}
#' }
#' 
#' @seealso [addManual()]
#' @examples
#' data(MANUAL_SYNONYMS)
#' str(MANUAL_SYNONYMS)
"MANUAL_SYNONYMS"
