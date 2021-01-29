require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Loading SNOMED dictionary')

test_that('Test exporting and reloading sample SNOMED dictionary', {
	TEST <-sampleSNOMED()
	# Export to temporary directory
	for (table in c('Concept', 'Description', 'Relationship',
		'StatedRelationship')){
		write.table(get(toupper(table), envir = TEST), paste0(tempdir(),
		'/', table, '.txt'), row.names = FALSE, sep = '\t', quote = FALSE)
	}

	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
	expect_equal(TEST$CONCEPT, TEST2$CONCEPT)
	expect_equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
	expect_equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
	expect_equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
})
