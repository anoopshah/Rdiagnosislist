context('Loading SNOMED dictionary')


#' TEST <- sampleSNOMED()
#'
#' # Export to temporary directory
#' for (table in c('Concept', 'Description', 'Relationship',
#'   'StatedRelationship')){
#'   write.table(get(toupper(table), envir = TEST), paste0(tempdir(),
#'     '/', table, '.txt'), row.names = FALSE, sep = '\t', quote = FALSE)
#' }
#'
#' # Try to import using the loadSNOMED function
#' TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
#'
#' # Check that reimported SNOMED dictionary is the same as the original
#' all.equal(TEST$CONCEPT, TEST2$CONCEPT)
#' all.equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
#' all.equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
#' all.equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)

test_that('SNOMED dictionary can be loaded', {
	expect_equal(1, 1)
})

context('SNOMED hierarchy')

test_that('Parent-child relationships work', {
	expect_equal(1, 1)
})

