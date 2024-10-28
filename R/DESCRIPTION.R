#' Sample description table from SNOMED CT dictionary
#'
#' A sample of the SNOMED CT description table. Each concept may has
#' a fully specified name and may have any number of synonyms.
#' 
#' @name SNOMED_DESCRIPTION
#' @aliases DESCRIPTION
#' @docType data
#' @importFrom utils data
#' @usage data(DESCRIPTION)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \describe{
#'   \item{id}{ integer64: description ID}
#'   \item{moduleId}{ integer64: class of SNOMED CT concept (whether it
#'     is used for recording information or is a metadata concept)}
#'   \item{conceptId}{ integer64: SNOMED CT concept ID}
#'   \item{languageCode}{ character: 'en' = English}
#'   \item{typeId}{ integer64: 900000000000013009 = Synonym,
#'     900000000000003001 = Fully Specified Name}
#'   \item{term}{ character: term description}
#'   \item{caseSignificanceId}{ integer64:
#'     900000000000020002 = Initial character case sensitive,
#'     900000000000017005 = Whole term case sensitive,
#'     900000000000448009 = Whole term case insensitive}
#'   \item{effectiveTime}{ IDate: when the concept became active}
#'   \item{active}{ logical: whether this concept is currently active}
#' }
#' 
#' @examples
#' # Show properties of the DESCRIPTION table
#' data('DESCRIPTION')
#' str(DESCRIPTION)
"DESCRIPTION"
