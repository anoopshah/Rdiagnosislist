require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Mapping table')

test_that('Test Read 2 to SNOMED CT mappings - multiple rows', {
	data(MAPS)
	SNOMED <- sampleSNOMED()
	read2hfmaps <- getMaps(SNOMEDconcept('Heart failure'),
		mappingtable = MAPS, to = 'read2',
		single_row_per_concept = FALSE)
	mappingrow <- MAPS[conceptId == as.integer64(84114007)]
	expect_equal(sort(unique(unlist(mappingrow$read2_code))),
		sort(unique(read2hfmaps$read2_code)))
	expect_equal(sort(unique(unlist(mappingrow$read2_term))),
		sort(unique(read2hfmaps$read2_term)))
})

test_that('Test CTV3 to SNOMED CT mappings - multiple rows', {
	data(MAPS)
	SNOMED <- sampleSNOMED()
	ctv3hfmaps <- getMaps(SNOMEDconcept('Heart failure'),
		mappingtable = MAPS, to = 'ctv3',
		single_row_per_concept = FALSE)
	mappingrow <- MAPS[conceptId == as.integer64(84114007)]
	expect_equal(sort(unique(unlist(mappingrow$ctv3_concept))),
		sort(unique(ctv3hfmaps$ctv3_concept)))
	expect_equal(sort(unique(unlist(mappingrow$ctv3_termid))),
		sort(unique(ctv3hfmaps$ctv3_termid)))
})

test_that('Test Read 2 / CTV3 to SNOMED CT mappings - single row', {
	data(MAPS)
	SNOMED <- sampleSNOMED()
	allmaps <- getMaps(SNOMEDconcept('Heart failure'),
		mappingtable = MAPS, to = c('ctv3', 'read2'),
		single_row_per_concept = TRUE)
	mappingrow <- MAPS[conceptId == as.integer64(84114007)]
	expect_equal(nrow(allmaps), 1)
	expect_equal(mappingrow$read2_code, allmaps$read2_code)
	expect_equal(mappingrow$ctv3_concept, allmaps$ctv3_concept)
})
