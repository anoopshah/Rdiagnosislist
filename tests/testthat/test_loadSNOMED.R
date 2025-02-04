require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Loading SNOMED dictionary')

test_that('Test exporting and reloading sample SNOMED dictionary', {
	setDTthreads(threads = 1)
	TEST <-sampleSNOMED()
	# Export to temporary directory
	exportSNOMEDenvir(TEST, tempdir())
	
	# Try to import using the loadSNOMED function
	TEST2 <- loadSNOMED(tempdir(), active_only = FALSE)
	
	# Data table comparison ignoring attributes
	dtcomp <- function(dt1, dt2){
		data.table:::all.equal.data.table(dt1, dt2,
			trim.levels = TRUE, check.attributes = FALSE,
			ignore.col.order = TRUE, ignore.row.order = TRUE)
	}
	expect_true(dtcomp(TEST$CONCEPT, TEST2$CONCEPT))
	expect_true(dtcomp(TEST$DESCRIPTION, TEST2$DESCRIPTION))
	expect_true(dtcomp(TEST$RELATIONSHIP, TEST2$RELATIONSHIP))
	expect_true(dtcomp(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP))
	expect_true(dtcomp(TEST$REFSET, TEST2$REFSET))
	expect_true(dtcomp(TEST$SIMPLEMAP, TEST2$SIMPLEMAP))
	expect_true(dtcomp(TEST$HISTORY, TEST2$HISTORY))
	expect_true(dtcomp(TEST$QUERY, TEST2$QUERY))

	# Clean up
	for (table in c('_Concept_', '_Description_',
		'_StatedRelationship_', '_Relationship_',
		'Refset_SimpleMap', 'Refset_ExtendedMap',
		'Refset_Simple')){
		file.remove(paste0(tempdir(), '/', table, 'Snapshot.txt'))
	}
})

test_that('Test exporting and reloading multiple files', {
	setDTthreads(threads = 1)
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

	# Data table comparison ignoring attributes
	dtcomp <- function(dt1, dt2){
		data.table:::all.equal.data.table(dt1, dt2,
			trim.levels = TRUE, check.attributes = FALSE,
			ignore.col.order = TRUE, ignore.row.order = TRUE)
	}
	expect_true(dtcomp(TEST$CONCEPT, TEST2$CONCEPT))
	expect_true(dtcomp(TEST$DESCRIPTION, TEST2$DESCRIPTION))
	expect_true(dtcomp(TEST$RELATIONSHIP, TEST2$RELATIONSHIP))
	expect_true(dtcomp(TEST$STATEDRELATIONSHIP, TEST2$STATEDRELATIONSHIP))
	expect_true(dtcomp(TEST$REFSET, TEST2$REFSET))
	expect_true(dtcomp(TEST$SIMPLEMAP, TEST2$SIMPLEMAP))
	expect_true(dtcomp(TEST$HISTORY, TEST2$HISTORY))
	expect_true(dtcomp(TEST$QUERY, TEST2$QUERY))
	
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
