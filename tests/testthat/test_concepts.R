require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('SNOMED concepts')

test_that('Test SNOMED dictionary includes inactive terms', {
	expect_true(inactiveIncluded(sampleSNOMED()))
})

test_that('inactiveIncluded checks metadata of SNOMED dictionary', {
	TEST <- sampleSNOMED()
	assign('metadata', list(active_only = TRUE), envir = TEST)
	expect_false(inactiveIncluded(TEST))
})

context('Identifying concepts in test SNOMED dictionary')

test_that('Single term matching', {
	expect_equal(as.SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED()),
		as.SNOMEDconcept('84114007'))
})

test_that('Duplicates', {
	expect_equal(as.SNOMEDconcept(c('Heart failure', 'Weak heart'),
		SNOMED = sampleSNOMED()), as.SNOMEDconcept('84114007'))
	expect_equal(as.SNOMEDconcept(c('Heart failure', 'Systolic heart failure',
		'Weak heart', 'Acute heart failure'), unique = FALSE,
		SNOMED = sampleSNOMED()), as.SNOMEDconcept(c('84114007',
		'417996009', '84114007', '56675007')))
})

test_that('Regular expressions', {
	expect_equal(as.SNOMEDconcept('hfnef|HFNEF', exact = FALSE,
		SNOMED = sampleSNOMED()), as.SNOMEDconcept('446221000'))
})

test_that('Pattern matching', {
	expect_equal(as.SNOMEDconcept('Systolic heart', exact = FALSE,
		SNOMED = sampleSNOMED()), as.SNOMEDconcept('417996009'))
})

test_that('Match not found', {
	expect_equal(as.SNOMEDconcept('Angina',
		SNOMED = sampleSNOMED()), as.SNOMEDconcept(0))
})

test_that('Check concept ID', {
	expect_equal(as.SNOMEDconcept('900000000000003001'),
		as.SNOMEDconcept(as.integer64('900000000000003001')))
	expect_error(as.SNOMEDconcept(12345))
	expect_equal(as.SNOMEDconcept('Heart'), NA)
	expect_error(as.SNOMEDconcept(list(as.integer64('1234'))))
})
