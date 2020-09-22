#' Sample description table from SNOMED CT dictionary
#'
#' A sample of the SNOMED CT description table. Each concept may has
#' a fully specified name and may have any number of synonyms.
#' 
#' @name DESCRIPTION
#' @docType data
#' @usage data(DESCRIPTION)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \itemize{
#'   \item{id}{integer64 - description ID}
#'   \item{moduleId}{integer64 - class of SNOMED CT concept (whether it is used for recording information or is a metadata concept)}
#'   \item{conceptId}{integer64 - SNOMED CT concept ID}
#'   \item{languageCode}{character - 'en' = English}
#'   \item{typeId}{integer64 - 900000000000013009 synonym, 900000000000003001 fully specified name}
#'   \item{term}{character - term description}
#'   \item{caseSignificanceId}{integer64 - 900000000000020002 initial character case sensitive, 900000000000017005 whole term case sensitive, 900000000000448009 whole term case insensitive}
#'   \item{effectiveTime}{IDate - when the concept became active}
#'   \item{active}{integer - whether this concept is currently active}
#' }
#' 
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- new.env()
#' data(CONCEPT, envir = TEST)
#' data(DESCRIPTION, envir = TEST)
#' data(RELATIONSHIP, envir = TEST)
#' data(STATEDRELATIONSHIP, envir = TEST) 
#' 
#' Show properties of the DESCRIPTION table
#' str(TEST$DESCRIPTION)
"DESCRIPTION"