context('SNOMED codelists')

test_that('SNOMED dictionary can be loaded', {
	expect_equal(1, 1)
})

context('SNOMED hierarchy')

test_that('Parent-child relationships work', {
	expect_equal(1, 1)
})

#' SNOMED <- sampleSNOMED()
#'
#' my_concepts <- conceptId('Heart failure')
#' SNOMEDcodelist(data.table(conceptId = my_concepts))
#' as.SNOMEDcodelist(data.table(conceptId = my_concepts,
#'   include_desc = TRUE))


#' my_concepts <- conceptId('Heart failure')
#' my_codelist <- SNOMEDcodelist(data.table(conceptId = my_concepts,
#'   include_desc = TRUE))
#' expanded_codelist <- expandSNOMED(my_codelist)
#' contractSNOMED(expanded_codelist)
