require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Mapping table')

test_that('Test SNOMED / Read mappings', {
	expect_true(inactiveIncluded(sampleSNOMED()))
})
