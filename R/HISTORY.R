#' Sample history substitution table from SNOMED CT dictionary
#'
#' Sample of the SNOMED CT table showing current equivalents for inactive concepts.
#' 
#' @name HISTORY
#' @docType data
#' @importFrom utils data
#' @usage data(HISTORY)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#' @family SNOMEDsample
#'
#' @details
#' \describe{
#'   \item{OLDCONCEPTID}{ integer64: concept ID of the inactive concepts} 
#'   \item{OLDCONCEPTSTATUS}{ integer: status of the old concept} 
#'   \item{NEWCONCEPTID}{ integer64: concept ID of the new concept}
#'   \item{NEWCONCEPTSTATUS}{ integer: status of the new concept} 
#'   \item{PATH}{ character: path} 
#'   \item{ISAMBIGUOUS }{ integer: whether ambiguous} 
#'   \item{ITERATIONS}{ integer: number of iterations} 
#'   \item{OLDCONCEPTFSN}{ character: old concept Fully Specified Name}
#'   \item{OLDCONCEPTFSN_TAGCOUNT}{ integer: number of tags for old concept}
#'   \item{NEWCONCEPTFSN}{ integer: new concept Fully Specified Name}
#'   \item{NEWCONCEPTFSN_STATUS}{ integer: new concept Fully Specified Name status}
#'   \item{TLH_IDENTICALFLAG}{ integer: whether TLH identical}
#'   \item{FSN_TAGLESSIDENTICALFLAG}{ integer: whether Fully Specified Names are identical ignoring the tags}
#'   \item{FSN_TAGIDENTICALFLAG}{ integer: whether Fully Specified Names tags are identical}
#' }
#' 
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- new.env()
#' data(CONCEPT, envir = TEST)
#' data(DESCRIPTION, envir = TEST)
#' data(RELATIONSHIP, envir = TEST)
#' data(STATEDRELATIONSHIP, envir = TEST)
#' data(HISTORY, envir = TEST)
#' 
#' # Show properties of the history table
#' str(TEST$HISTORY)
"HISTORY"
