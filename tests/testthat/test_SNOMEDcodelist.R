require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('SNOMED codelists')

test_that('Creating codelists from concept IDs or tables', {
	myconcepts <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
	concept_codelist <- SNOMEDcodelist(myconcepts, SNOMED = sampleSNOMED())
	table_codelist <- SNOMEDcodelist(data.frame(conceptId = myconcepts,
		include_desc = TRUE), SNOMED = sampleSNOMED())
	expect_equal(all.equal(concept_codelist, table_codelist), TRUE)
})

test_that('Codelist with missing descriptions', {
	my_codelist <- SNOMEDcodelist('1234', SNOMED = sampleSNOMED())
	expect_equal(nrow(my_codelist), 1)
	expect_equal(my_codelist$conceptId, as.SNOMEDconcept('1234'))
	expect_true(my_codelist$include_desc)
	expect_equal(my_codelist$term, as.character(NA))
})

test_that('Expand and contract codelists', {
	my_concepts <- as.SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED())
	my_codelist <- SNOMEDcodelist(data.frame(conceptId = my_concepts,
		include_desc = TRUE), SNOMED = sampleSNOMED())
	expanded_codelist <- expandSNOMED(my_codelist, SNOMED = sampleSNOMED())
	roundtrip_codelist <- contractSNOMED(expanded_codelist)
	setindex(my_codelist, NULL)
	setindex(roundtrip_codelist, NULL)
	expect_equal(all.equal(my_codelist, roundtrip_codelist), TRUE)
})
