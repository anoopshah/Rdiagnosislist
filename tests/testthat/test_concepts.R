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
	expect_equal(SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED()),
		SNOMEDconcept('84114007'))
})

test_that('Ensure that only concepts in the concept table are returned', {
	TEST <- sampleSNOMED()
	# Create an alternative (deprecated) concept for heart failure
	TEST$DESCRIPTION[term == 'Heart failure (disorder)',
		conceptId := as.integer64('155374007')]
	TEST$DESCRIPTION[term == 'Heart failure (disorder)',
		term := 'Heart failure']
	TEST$metadata$active_only <- TRUE
	# Add a description without a concept in the concept table
	expect_equal(SNOMEDconcept('Heart failure', SNOMED = TEST),
		SNOMEDconcept('84114007'))
})

test_that('Duplicates', {
	expect_equal(SNOMEDconcept(c('Heart failure', 'Weak heart'),
		SNOMED = sampleSNOMED()), SNOMEDconcept('84114007'))
	expect_equal(SNOMEDconcept(c('Heart failure', 'Systolic heart failure',
		'Weak heart', 'Acute heart failure'), unique = FALSE,
		SNOMED = sampleSNOMED()), SNOMEDconcept(c('84114007',
		'417996009', '84114007', '56675007')))
})

test_that('Descriptions with duplicates', {
	expect_equal(description(rep(SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), 2), SNOMED = sampleSNOMED())$term,
		rep('Heart failure (disorder)', 2))
	expect_equal(description(rep(SNOMEDconcept('History of heart failure',
		SNOMED = sampleSNOMED()), 2), SNOMED = sampleSNOMED())$term,
		rep('History of heart failure (situation)', 2))
})


test_that('Regular expressions', {
	expect_equal(SNOMEDconcept('hfnef|HFNEF', exact = FALSE,
		SNOMED = sampleSNOMED()), SNOMEDconcept('446221000'))
})

test_that('Semantic types', {
	expect_equal(semanticType(c('Heart failure', 'Is a'),
		SNOMED = sampleSNOMED()), c('disorder', 'attribute'))
})

test_that('Pattern matching', {
	expect_equal(SNOMEDconcept('Systolic heart', exact = FALSE,
		SNOMED = sampleSNOMED()), SNOMEDconcept(c('417996009',
		'120851000119104', '120861000119102', '15629741000119102')))
})

test_that('Match not found', {
	empty_concept <- bit64::integer64(0)
	setattr(empty_concept, 'class', c('SNOMEDconcept', 'integer64'))
	expect_equal(SNOMEDconcept('Angina',
		SNOMED = sampleSNOMED()), empty_concept)
})

test_that('Check concept ID', {
	# Convert character to integer64
	expect_equal(SNOMEDconcept('900000000000003001'),
		SNOMEDconcept(bit64::as.integer64('900000000000003001')))
	# Do not allow numeric input for SNOMEDconcept in case it is
	# incorrect (inadequate precision)
	expect_error(SNOMEDconcept(84114007))
	# Allow integer input, which is converted to integer64
	expect_equal(SNOMEDconcept(as.integer(84114007)),
		SNOMEDconcept(bit64::as.integer64('84114007')))
	# Do not allow lists
	expect_error(SNOMEDconcept(list(bit64::as.integer64('1234'))))
})

test_that('Acronyms', {
	expect_equal(acronyms(as.SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED())$term, 'HF')
	expect_equal(acronyms(as.SNOMEDconcept('Right ventricular abnormality',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED())$term,
		'RV abnormality')
})

context('SNOMEDconcept set functions')

test_that('Generic set functions for strings', {
	# These tests are essential to ensure that the new
	# generic functions work just like the base functions
	# for data other than SNOMEDconcept objects
	sys_acute <- c('sys', 'acute')
	acute_left_right <- c('acute', 'left', 'right')
	expect_equal(union(sys_acute, acute_left_right),
		c('sys', 'acute', 'left', 'right'))
	expect_equal(intersect(sys_acute, acute_left_right), 'acute')
	expect_equal(setdiff(sys_acute, acute_left_right), 'sys')
})

test_that('Generic set functions for numbers', {
	# These tests are essential to ensure that the new
	# generic functions work just like the base functions
	# for data other than SNOMEDconcept objects
	sys_acute <- c(1, 2, 3, 4, 4)
	acute_left_right <- c(3, 5, 6, 7, 7, 8)
	expect_equal(union(sys_acute, acute_left_right), 1:8)
	expect_equal(union(sys_acute, acute_left_right),
		base::union(sys_acute, acute_left_right))
	expect_equal(intersect(sys_acute, acute_left_right), 3)
	expect_equal(intersect(sys_acute, acute_left_right),
		base::intersect(sys_acute, acute_left_right))
	expect_equal(setdiff(sys_acute, acute_left_right), c(1, 2, 4))
	expect_equal(setdiff(sys_acute, acute_left_right),
		base::setdiff(sys_acute, acute_left_right))
})

test_that('SNOMEDconcept set functions', {
	sys_acute <- SNOMEDconcept(c('Systolic heart failure',
		'Acute heart failure'), SNOMED = sampleSNOMED())
	acute_left_right <- SNOMEDconcept(c('Acute heart failure',
		'Left heart failure', 'Right heart failure'),
		SNOMED = sampleSNOMED())
	expect_equal(union(sys_acute, acute_left_right),
		SNOMEDconcept(c('Systolic heart failure', 'Acute heart failure',
		'Left heart failure', 'Right heart failure'),
		SNOMED = sampleSNOMED()))
	expect_equal(intersect(sys_acute, acute_left_right),
		SNOMEDconcept('Acute heart failure', SNOMED = sampleSNOMED()))
	expect_equal(setdiff(sys_acute, acute_left_right),
		SNOMEDconcept('Systolic heart failure', SNOMED = sampleSNOMED()))
})

test_that('SNOMEDconcept set functions with empty sets', {
	sys_acute <- SNOMEDconcept(c('Systolic heart failure',
		'Acute heart failure'), SNOMED = sampleSNOMED())
	empty <- SNOMEDconcept(bit64::integer64(0))
	expect_equal(union(sys_acute, empty), sys_acute)
	expect_equal(union(empty, sys_acute), sys_acute)
	expect_equal(union(empty, empty), empty)
	expect_equal(intersect(sys_acute, empty), empty)
	expect_equal(intersect(empty, sys_acute), empty)
	expect_equal(intersect(empty, empty), empty)
	expect_equal(setdiff(sys_acute, empty), sys_acute)
	expect_equal(setdiff(empty, sys_acute), empty)
	expect_equal(setdiff(empty, empty), empty)
})

test_that('Concatenate SNOMEDconcept objects', {
	hf <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
	hf2 <- rep(bit64::as.integer64('84114007'), 2)
	class(hf2) <- c('SNOMEDconcept', 'integer64')
	expect_equal(unique(c(hf, hf)), hf)
	expect_equal(c(hf, hf), hf2)
})

test_that('Description for empty SNOMEDconcept object', {
	empty <- SNOMEDconcept(integer64(0), SNOMED = sampleSNOMED())
	emptytable <- data.table(id = integer64(0), conceptId = integer64(0),
		term = character(0))
	expect_equal(description(empty), emptytable)
})
