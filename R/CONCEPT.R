#' Sample concept table from SNOMED CT dictionary
#'
#' A sample of the SNOMED CT concept table. 
#' 
#' @name CONCEPT
#' @docType data
#' @usage data(CONCEPT)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \itemize{
#'   \item{id}{integer64 - conceptId} 
#'   \item{moduleId}{integer64 - class of SNOMED CT concept (whether it is used for recording information or is a metadata concept)}
#'   \item{definitionStatusId}{integer64 - 900000000000074008 primitive concept, 900000000000073002 defined by conditions}
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
#' Show properties of the CONCEPT table
#' str(TEST$CONCEPT)
"CONCEPT"