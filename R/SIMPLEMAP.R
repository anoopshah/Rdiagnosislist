#' Sample SIMPLE map table from SNOMED CT dictionary
#'
#' A sample of the SNOMED CT SIMPLE map table, containing maps to ICD-10 and OPCS4. 
#' 
#' @name SNOMED_SIMPLEMAP
#' @aliases SIMPLEMAP
#' @importFrom utils data
#' @docType data
#' @usage data(SIMPLEMAP)
#' @format An object of class \code{"data.table"}
#' @keywords datasets
#'
#' @details
#' \describe{
#'   \item{moduleId}{ integer64: core metadata concept: 900000000000207008 = SNOMED CT core module, 999000021000000109 = SNOMED CT United Kingdom clinical extension reference set module, 999000031000000106 = SNOMED CT United Kingdom Edition reference set module}
#'   \item{refsetId}{ integer64: foundation metadata concept: 900000000000497000 = CTV3 simple map reference set, 446608001 = ICD-O simple map reference set, 1323081000000108 = Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 test result communication to general practice concept simple map reference set, 1323091000000105 = Coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 test result communication to general practice description simple map reference set, 82551000000107 = National Health Service England National Genomic Test Directory whole genome sequencing test simple map reference set}
#'   \item{referencedComponentId}{ integer64: SNOMED CT conceptId of the concept mapped}
#'   \item{mapTarget}{ character: target ICD-O or CTV3 code}
#'   \item{effectiveTime}{ IDate: when the concept became active}
#'   \item{active}{ logical: whether this concept is currently active}
#' }
#' 
#' @family sampleSNOMED
#' @examples
#' # Load the dataset and show its properties
#' data('SIMPLEMAP')
#' str(SIMPLEMAP)
#'
#' # This SIMPLEMAP table is part of the sample SNOMED CT dictionary
#' # Hence this should show the same properties as above
#' str(sampleSNOMED()$SIMPLEMAP)
"SIMPLEMAP"
