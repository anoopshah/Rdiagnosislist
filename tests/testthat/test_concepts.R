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
	SNOMED <- sampleSNOMED()
	expect_equal(conceptId('Heart failure'), as.integer64('84114007'))
})

test_that('Duplicates', {
	SNOMED <- sampleSNOMED()
	expect_equal(conceptId(c('Heart failure', 'Weak heart')),
		as.integer64('84114007'))
	expect_equal(conceptId(c('Heart failure', 'Systolic heart failure',
		'Weak heart', 'Acute heart failure')),
		as.integer64(c('84114007', '417996009', '84114007', '56675007')))
})

test_that('Regular expressions', {
	SNOMED <- sampleSNOMED()
	expect_equal(conceptId('hfnef|HFNEF', exact = FALSE),
		as.integer64('446221000'))
})

test_that('Pattern matching', {
	SNOMED <- sampleSNOMED()
	expect_equal(conceptId('Systolic heart', exact = FALSE),
		as.integer64('417996009'))
})

test_that('Match not found', {
	SNOMED <- sampleSNOMED()
	expect_equal(conceptId('Angina', integer64(0)))
})

test_that('Check concept ID', {
	SNOMED <- sampleSNOMED()
	expect_equal(checkConcepts('900000000000003001'),
		checkConcepts(as.integer64('900000000000003001')))
	expect_error(checkConcepts(12345))
	expect_equal(checkConcepts('Heart'), NA)
	expect_error(checkConcepts(list(as.integer64('1234'))))
})
