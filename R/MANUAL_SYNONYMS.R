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
#' \itemize{
#'   \item{term1}{ character: First term, in lower case if case is unimportant} 
#'   \item{term2}{ character: Second term, in lower case if case is unimportant}
#' }
#' 
#' @seealso [addManual()]
#' @examples
#' data(MANUAL_SYNONYMS)
#' str(MANUAL_SYNONYMS)
"MANUAL_SYNONYMS"
