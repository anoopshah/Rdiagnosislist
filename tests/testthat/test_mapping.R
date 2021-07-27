require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Mapping table')

test_that('Test Read 2 to SNOMED CT mappings', {
	data(MAPS)
	read2hfmaps <- getMaps(SNOMEDconcept('Heart failure'),
		mappingtable = MAPS, to = 'read2')
	mappingrow <- MAPS[conceptId == as.integer64(84114007)]
	expect_equal(sort(unique(unlist(mappingrow$read2_code))),
		sort(unique(read2hfmaps$read2_code)))
	expect_equal(sort(unique(unlist(mappingrow$read2_term))),
		sort(unique(read2hfmaps$read2_term)))
})

test_that('Test CTV3 to SNOMED CT mappings', {
	data(MAPS)
	ctv3hfmaps <- getMaps(SNOMEDconcept('Heart failure'),
		mappingtable = MAPS, to = 'ctv3')
	mappingrow <- MAPS[conceptId == as.integer64(84114007)]
	expect_equal(sort(unique(unlist(mappingrow$ctv3_concept))),
		sort(unique(ctv3hfmaps$ctv3_concept)))
	expect_equal(sort(unique(unlist(mappingrow$ctv3_termid))),
		sort(unique(ctv3hfmaps$ctv3_termid)))
})
