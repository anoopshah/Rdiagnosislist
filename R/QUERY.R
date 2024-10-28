#' Sample query table from SNOMED CT dictionary
#'
#' Sample of the SNOMED CT table of ancestor / descendant relationships
#' for inactive concepts.
#' 
#' @name QUERY
#' @docType data
#' @importFrom utils data
#' @usage data(QUERY)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#' @family SNOMEDsample
#'
#' @details
#' \describe{
#'   \item{supertypeId}{ integer64: concept ID of the ancestor (active) concept} 
#'   \item{subtypeId}{ integer64: concept ID of the descendant (inactive) concept} 
#'   \item{provenance}{ integer: provenance of relationship. Provenance = 0 means subsumption is always true. Provenance = 1 means subsumption is usually true (but there is a theoretical risk of false positives). Provenance = 2 means both ancestors and descendents are only approximately known. Provenance = 3 means original code had at least two distinct meanings and all are being returned}
#' }
#' 
#' @examples
#' # Create a TEST environment and load the sample dictionaries
#' TEST <- new.env()
#' data(CONCEPT, envir = TEST)
#' data(DESCRIPTION, envir = TEST)
#' data(RELATIONSHIP, envir = TEST)
#' data(STATEDRELATIONSHIP, envir = TEST)
#' data(QUERY, envir = TEST)
#' 
#' # Show properties of the query table
#' str(TEST$QUERY)
"QUERY"
