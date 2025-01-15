require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('History and query table')

test_that('Test the use of history and query table', {
	setDTthreads(threads = 1)
	TEST <-sampleSNOMED()
	HFpEF <- SNOMEDconcept('Heart failure with normal ejection fraction',
		SNOMED = TEST)
	with_inactive <- sort(addInactiveConcepts(HFpEF, SNOMED = TEST))
	expect_equal(with_inactive, as.SNOMEDconcept(c(
		'446221000', '749821000000101', '757101000000106')))
})

