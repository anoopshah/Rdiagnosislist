require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Retrieving refsets')

test_that('Retrieving refsets', {
	setDTthreads(threads = 1)
	myrefset <- sort(getRefset('Renal clinical finding simple reference set',
		SNOMED = sampleSNOMED()))
	check <- sort(as.SNOMEDconcept(sampleSNOMED()$REFSET[refsetId ==
		bit64::as.integer64('999001061000000106'), referencedComponentId],
		SNOMED = sampleSNOMED()))
	expect_equal(myrefset, check)
	# No refset ID specified
	expect_equal(getRefset(bit64::integer64(0), SNOMED = sampleSNOMED()),
		as.SNOMEDconcept(bit64::integer64(0), SNOMED = sampleSNOMED()))
})
