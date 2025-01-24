#' Sample refset table from SNOMED CT dictionary
#'
#' A sample of the SNOMED CT refset table. This contains SNOMED CT
#' codelists that are used for partiular operational or clinical
#' purposes, and are curated by SNOMED CT. The id column of the
#' refset table is not included, in order to save space.
#' 
#' @name SNOMED_REFSET
#' @aliases REFSET
#' @importFrom utils data
#' @docType data
#' @usage data(REFSET)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \describe{
#'   \item{moduleId}{ integer64: SNOMED CT core metadata concept, stating whether the refset is from the SNOMED CT core module or the UK extension.}
#'   \item{refsetId}{ integer64: SNOMED CT conceptId of the refset. These concepts have semantic type 'foundation metadata concept'} 
#'   \item{referencedComponentId}{ integer64: SNOMED CT conceptId of the member of the refset} 
#'   \item{effectiveTime}{ IDate: when the concept became active}
#'   \item{active}{ logical: whether this concept is currently active}
#' }
#' 
#' @family sampleSNOMED
#' @examples
#' # Load the dataset and show its properties
#' data('REFSET')
#' str(REFSET)
#'
#' # This REFSET table is part of the sample SNOMED CT dictionary
#' # Hence this should show the same properties as above
#' str(sampleSNOMED()$REFSET)
"REFSET"
