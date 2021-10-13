require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Loading SNOMED dictionary')

test_that('Test exporting and reloading sample SNOMED dictionary', {
	TEST <-sampleSNOMED()
	# Export to temporary directory
	write.table(get('CONCEPT', envir = TEST, inherits = FALSE),
		paste0(tempdir(), '/_Concept_Snapshot.txt'),
		row.names = FALSE, sep = '\t', quote = FALSE)
	write.table(get('DESCRIPTION', envir = TEST, inherits = FALSE),
		paste0(tempdir(), '/_Description_Snapshot.txt'),
		row.names = FALSE, sep = '\t', quote = FALSE)
	write.table(get('RELATIONSHIP', envir = TEST, inherits = FALSE),
		paste0(tempdir(), '/_Relationship_Snapshot.txt'),
		row.names = FALSE, sep = '\t', quote = FALSE)
	write.table(get('STATEDRELATIONSHIP', envir = TEST, inherits = FALSE),
		paste0(tempdir(), '/_StatedRelationship_Snapshot.txt'),
		row.names = FALSE, sep = '\t', quote = FALSE)
	write.table(get('REFSET', envir = TEST, inherits = FALSE),
		paste0(tempdir(), '/Refset_SimpleSnapshot.txt'),
		row.names = FALSE, sep = '\t', quote = FALSE)
	write.table(get('SIMPLEMAP', envir = TEST, inherits = FALSE),
		paste0(tempdir(), '/Refset_SimpleMapSnapshot.txt'),
		row.names = FALSE, sep = '\t', quote = FALSE)
	write.table(get('EXTENDEDMAP', envir = TEST, inherits = FALSE),
		paste0(tempdir(), '/Refset_ExtendedMapSnapshot.txt'),
		row.names = FALSE, sep = '\t', quote = FALSE)

	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
	expect_equal(TEST$CONCEPT, TEST2$CONCEPT)
	expect_equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
	expect_equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
	expect_equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
	expect_equal(TEST$REFSET, TEST2$REFSET)
	expect_equal(TEST$SIMPLEMAP, TEST2$SIMPLEMAP)
	expect_equal(TEST$EXTENDEDMAP, TEST2$EXTENDEDMAP)

	# Clean up
	for (table in c('_Concept_Snapshot', '_Description_Snapshot',
		'_StatedRelationship_Snapshot', '_Relationship_Snapshot',
		'Refset_SimpleMapSnapshot', 'Refset_ExtendedMapSnapshot',
		'Refset_SimpleSnapshot')){
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
			paste0(tempdir(), '/1/_', table, '_Snapshot.txt'),
			row.names = FALSE, sep = '\t', quote = FALSE)
	}
	for (table in c('Concept', 'Description', 'Relationship')){
		write.table(get(toupper(table), envir = TEST, inherits = FALSE),
			paste0(tempdir(), '/2/_', table, '_Snapshot.txt'),
			row.names = FALSE, sep = '\t', quote = FALSE)
	}

	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(paste0(tempdir(), c('/1', '/2')),
		active_only = FALSE)
	expect_equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
	expect_equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
	
	# Try with one file completely missing
	file.remove(paste0(tempdir(), '/2/_Relationship_Snapshot.txt'))
	expect_error(TEST3 <- loadSNOMED(paste0(tempdir(), c('/1', '/2')),
		active_only = FALSE))
	
	# Clean up
	for (table in c('Concept', 'Description', 'StatedRelationship')){
		file.remove(paste0(tempdir(), '/1/_', table, '_Snapshot.txt'))
		}
	file.remove(paste0(tempdir(), '/1'))
	for (table in c('Concept', 'Description')){
		file.remove(paste0(tempdir(), '/2/_', table, '_Snapshot.txt'))
	}
	file.remove(paste0(tempdir(), '/2'))
})
