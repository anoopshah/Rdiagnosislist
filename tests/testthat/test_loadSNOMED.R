require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Loading SNOMED dictionary')

test_that('Test exporting and reloading sample SNOMED dictionary', {
	TEST <-sampleSNOMED()
	# Export to temporary directory
	exportSNOMEDenvir(TEST, tempdir())
	
	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
	expect_equal(TEST$CONCEPT, TEST2$CONCEPT)
	expect_equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
	expect_equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
	expect_equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
	expect_equal(TEST$REFSET, TEST2$REFSET)
	expect_equal(TEST$SIMPLEMAP, TEST2$SIMPLEMAP)
	expect_equal(TEST$HISTORY, TEST2$HISTORY)
	expect_equal(TEST$QUERY, TEST2$QUERY)
	# expect_equal(TEST$EXTENDEDMAP, TEST2$EXTENDEDMAP)
	# Comments fields in EXTENDEDMAP may have different
	# handling of missing data

	# Clean up
	for (table in c('_Concept_', '_Description_',
		'_StatedRelationship_', '_Relationship_',
		'Refset_SimpleMap', 'Refset_ExtendedMap',
		'Refset_Simple')){
		file.remove(paste0(tempdir(), '/', table, 'Snapshot.txt'))
	}
})

test_that('Test exporting and reloading multiple files', {
	TEST <- sampleSNOMED()
	# Create temporary directories
	dir.create(paste0(tempdir(), '/1'))
	dir.create(paste0(tempdir(), '/2'))
	# Export to temporary directories
	exportSNOMEDenvir(TEST, paste0(tempdir(), '/1/'))
	# Move some files to the other directory
	for (table in c('_Concept_', '_Relationship_',
		'Refset_SimpleMap', 'Refset_Simple')){
		file.rename(paste0(tempdir(), '/1/', table, 'Snapshot.txt'),
			paste0(tempdir(), '/2/', table, 'Snapshot.txt'))
	}

	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(paste0(tempdir(), c('/1', '/2')),
		active_only = FALSE)
	expect_equal(TEST$CONCEPT, TEST2$CONCEPT)
	expect_equal(TEST$DESCRIPTION, TEST2$DESCRIPTION)
	expect_equal(TEST$RELATIONSHIP, TEST2$RELATIONSHIP)
	expect_equal(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP)
	expect_equal(TEST$REFSET, TEST2$REFSET)
	expect_equal(TEST$SIMPLEMAP, TEST2$SIMPLEMAP)
	expect_equal(TEST$QUERY, TEST2$QUERY)
	expect_equal(TEST$HISTORY, TEST2$HISTORY)
	# expect_equal(TEST$EXTENDEDMAP, TEST2$EXTENDEDMAP)
	
	# Try with one file completely missing
	file.remove(paste0(tempdir(), '/2/_Relationship_Snapshot.txt'))
	expect_error(TEST3 <- loadSNOMED(paste0(tempdir(), c('/1', '/2')),
		active_only = FALSE))
	
	# Clean up
	for (table in c('_Description_', '_StatedRelationship_', 
		'Refset_ExtendedMap')){
		file.remove(paste0(tempdir(), '/1/', table, 'Snapshot.txt'))
	}
	file.remove(paste0(tempdir(),
		'/1/HistorySubstitutionTable_Concepts.txt'))
	file.remove(paste0(tempdir(),
		'/1/SNOMEDQueryTable.txt'))
	file.remove(paste0(tempdir(), '/1'))
	for (table in c('_Concept_', 'Refset_SimpleMap', 'Refset_Simple')){
		file.remove(paste0(tempdir(), '/2/', table, 'Snapshot.txt'))
	}
	file.remove(paste0(tempdir(), '/2'))
})
