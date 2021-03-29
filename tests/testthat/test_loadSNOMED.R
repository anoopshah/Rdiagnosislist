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
		write.table(get(toupper(table), envir = TEST, inherits = FALSE),
			paste0(tempdir(), '/sct_', table, '_test.txt'),
			row.names = FALSE, sep = '\t', quote = FALSE)
	}

	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
	expect_equal(TEST$CONCEPT, TEST2$CONCEPT)
	expect_equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
	expect_equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
	expect_equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
	
	# Clean up
	for (table in c('Concept', 'Description', 'Relationship',
		'StatedRelationship')){
		file.remove(paste0(tempdir(), '/sct_', table, '_test.txt'))
	}
})

test_that('Test exporting and reloading multiple files', {
	TEST <- sampleSNOMED()
	# Create temporary directories
	dir.create(paste0(tempdir(), '/1'))
	dir.create(paste0(tempdir(), '/2'))
	# Export to temporary directories
	for (table in c('Concept', 'Description', 'StatedRelationship')){
		write.table(get(toupper(table), envir = TEST, inherits = FALSE),
			paste0(tempdir(), '/1/sct_', table, '_test.txt'),
			row.names = FALSE, sep = '\t', quote = FALSE)
	}
	for (table in c('Concept', 'Description', 'Relationship')){
		write.table(get(toupper(table), envir = TEST, inherits = FALSE),
			paste0(tempdir(), '/2/sct_', table, '_test.txt'),
			row.names = FALSE, sep = '\t', quote = FALSE)
	}

	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(paste0(tempdir(), c('/1', '/2')),
		active_only = FALSE)
	expect_equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
	expect_equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
	
	# Try with one file completely missing
	file.remove(paste0(tempdir(), '/2/sct_Relationship_test.txt'))
	expect_error(TEST3 <- loadSNOMED(paste0(tempdir(), c('/1', '/2')),
		active_only = FALSE))
	
	# Clean up
	for (table in c('Concept', 'Description', 'StatedRelationship')){
		file.remove(paste0(tempdir(), '/1/sct_', table, '_test.txt'))
		}
	file.remove(paste0(tempdir(), '/1'))
	for (table in c('Concept', 'Description')){
		file.remove(paste0(tempdir(), '/2/sct_', table, '_test.txt'))
	}
	file.remove(paste0(tempdir(), '/2'))
})
