#' Sample relationship tables from SNOMED CT dictionary
#'
#' Samples of the SNOMED CT tables of stated relationships (RELATIONSHIP)
#'   and inferred relationships (RELATIONSHIP).
#' 
#' @name SNOMED_RELATIONSHIP
#' @aliases RELATIONSHIP
#' @aliases STATEDRELATIONSHIP
#' @aliases SNOMED_STATEDRELATIONSHIP
#' @docType data
#' @importFrom utils data
#' @usage data(RELATIONSHIP); data(STATEDRELATIONSHIP)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#' @family SNOMEDsample
#'
#' @details
#' \itemize{
#'   \item{id}{ integer64: ID of the relationship record (primary key)} 
#'   \item{active}{ logical: whether this concept is currently active}
#'   \item{moduleId}{ integer64: class of SNOMED CT concept (whether it
#'     is used for recording information or is a metadata concept)}
#'   \item{sourceId}{ integer64: source SNOMED CT concept for the relationship}
#'   \item{destinationId}{ integer64: destination SNOMED CT concept for the relationship}
#'   \item{relationshipGroup}{ integer: group ID for relationships that are grouped}
#'   \item{characteristicTypeId}{ integer64: 900000000000011006 = Inferred relationship}
#'   \item{modifierId}{ integer64: 900000000000451002 = Existential restriction modifier}
#'   \item{effectiveTime}{ IDate: when the concept became active}
#'   \item{typeId}{ integer64: type of relationship, e.g.
#'     116680003 = Is a, 42752001 = Due to, 246090004 = Associated finding,
#'     363698007 = Finding site, 363702006 = Has focus}
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
#' # Show properties of the relationship tables
#' str(TEST$RELATIONSHIP)
#' str(TEST$STATEDRELATIONSHIP)
"RELATIONSHIP"

#' @rdname SNOMED_RELATIONSHIP
"STATEDRELATIONSHIP"
