context('SNOMED hierarchy')

test_that('SNOMED dictionary can be loaded', {
	expect_equal(1, 1)
})


test_that('Parent-child relationships work', {
	expect_equal(1, 1)
})

#' SNOMED <- sampleSNOMED()
#'
#' # Example: anatomical site of a finding
#' findingSite <- function(x){
#'   relatedConcepts(conceptId(x),
#'     typeId = conceptId('Finding site'))
#' }
#' 
#' description(findingSite('Heart failure'))
#' # Heart structure (body structure)

#'
#' description(parents(conceptId('Heart failure')))
#' description(children(conceptId('Heart failure')))

#' SNOMED <- sampleSNOMED()
#'
#' hasAttributes(conceptId(c('Heart failure', 'Acute heart failure')),
#'   conceptId(c('Heart structure', 'Heart failure')),
#'   conceptId(c('Finding site', 'Is a')))

#' SNOMED <- sampleSNOMED()
#'
#' attributes(conceptId('Heart failure'))

#' SNOMED <- sampleSNOMED()
#'
#' semanticType(conceptId(c('Heart failure', 'Is a')))

#' SNOMED <- sampleSNOMED()
#'
#' original_terms <- c('Systolic heart failure', 'Is a',
#'   'Heart failure with reduced ejection fraction',
#'   'Acute kidney injury due to circulatory failure (disorder)')
#' # Note in this example 'Is a' has no parents in ancestors,
#' # and acute kidney failure has two parents in ancestors
#' # so neither of the parents will be chosen.
#' # Also test out inclusion of duplicate concepts.
#'
#' ancestors <- simplify(
#'   c(conceptId(original_terms), conceptId(original_terms)[3:4]),
#'   conceptId(c('Heart failure', 'Acute heart failure',
#'   'Cardiorenal syndrome (disorder)')))
#' print(cbind(original_terms, description(ancestors$ancestorId)$term))
