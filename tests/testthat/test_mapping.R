require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Mapping table')

test_that('Test Read 2 to SNOMED CT mappings - multiple rows', {
	setDTthreads(threads = 1)
	data(READMAPS)
	read2hfmaps <- getMaps(SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		mappingtable = READMAPS, to = 'read2',
		single_row_per_concept = FALSE)
	mappingrow <- READMAPS[conceptId == bit64::as.integer64('84114007')]
	expect_equal(sort(unique(unlist(mappingrow$read2_code))),
		sort(unique(read2hfmaps$read2_code)))
	expect_equal(sort(unique(unlist(mappingrow$read2_term))),
		sort(unique(read2hfmaps$read2_term)))
})

test_that('Test CTV3 to SNOMED CT mappings - multiple rows', {
	setDTthreads(threads = 1)
	data(READMAPS)
	ctv3hfmaps <- getMaps(SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		mappingtable = READMAPS, to = 'ctv3',
		single_row_per_concept = FALSE)
	mappingrow <- READMAPS[conceptId == bit64::as.integer64('84114007')]
	expect_equal(sort(unique(unlist(mappingrow$ctv3_concept))),
		sort(unique(ctv3hfmaps$ctv3_concept)))
	expect_equal(sort(unique(unlist(mappingrow$ctv3_termid))),
		sort(unique(ctv3hfmaps$ctv3_termid)))
})

test_that('Test Read 2 / CTV3 to SNOMED CT mappings - single row', {
	setDTthreads(threads = 1)
	data(READMAPS)
	allmaps <- getMaps(SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		mappingtable = READMAPS, to = c('ctv3', 'read2'),
		single_row_per_concept = TRUE)
	mappingrow <- READMAPS[conceptId == bit64::as.integer64('84114007')]
	expect_equal(nrow(allmaps), 1)
	expect_equal(mappingrow$read2_code, allmaps$read2_code)
	expect_equal(mappingrow$ctv3_concept, allmaps$ctv3_concept)
})

test_that('Test CTV3 to SNOMED CT mappings without READMAPS - single row', {
	setDTthreads(threads = 1)
	allmaps <- getMaps(SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		to = 'ctv3simple', single_row_per_concept = TRUE)
	mappingrow <- sampleSNOMED()$SIMPLEMAP[referencedComponentId ==
		bit64::as.integer64('84114007')]
	expect_equal(nrow(allmaps), 1)
	expect_equal(mappingrow$mapTarget, allmaps$ctv3_simple)
})


test_that('Test SNOMED CT to ICD-10 mappings - single row', {
	setDTthreads(threads = 1)
	icd10map <- getMaps(SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		to = 'icd10', single_row_per_concept = TRUE)
	expect_equal(nrow(icd10map), 1)
	expect_equal(unlist(icd10map$icd10_code), 'I509')
})

test_that('Test SNOMED CT to ICD-10 mappings - multiple rows', {
	setDTthreads(threads = 1)
	icd10map <- getMaps(SNOMEDconcept(c('Heart failure',
		paste0('Heart failure with reduced ejection fraction ',
		'due to coronary artery disease')),
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		to = 'icd10', single_row_per_concept = FALSE)
	expect_equal(nrow(icd10map), 3)
	expect_equal(sort(unique(unlist(icd10map$icd10_code))),
		c('I251', 'I509'))
})

test_that('Test SNOMED CT to OPCS mappings - single row', {
	setDTthreads(threads = 1)
	opcs4map <- getMaps(SNOMEDconcept(paste0('Implantation of ',
		'permanent cardiac pacemaker using fluoroscopic guidance'),
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		to = 'opcs4', single_row_per_concept = TRUE)
	expect_equal(nrow(opcs4map), 1)
	expect_equal(sort(unique(unlist(opcs4map$opcs4_code))),
		c('K601', 'Y534'))
})

test_that('Test SNOMED CT to OPCS mappings - multiple rows', {
	setDTthreads(threads = 1)
	opcs4map <- getMaps(SNOMEDconcept(paste0('Implantation of ',
		'permanent cardiac pacemaker using fluoroscopic guidance'),
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED(),
		to = 'opcs4', single_row_per_concept = FALSE)
	expect_equal(nrow(opcs4map), 2)
	expect_equal(sort(unique(unlist(opcs4map$opcs4_code))),
		c('K601', 'Y534'))
})
