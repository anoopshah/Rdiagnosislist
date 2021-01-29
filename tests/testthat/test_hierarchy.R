require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('SNOMED hierarchy')

test_that('Related concepts', {
	expect_equal(relatedConcepts(
		conceptId('Heart failure', SNOMED = sampleSNOMED()),
		typeId = conceptId('Finding site', SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED()),
		conceptId('Heart structure', SNOMED = sampleSNOMED()))
})

test_that('Attributes', {
	expect_equal(hasAttributes(
		conceptId(c('Heart failure', 'Acute heart failure'),
		SNOMED = sampleSNOMED()),
		conceptId(c('Heart structure', 'Heart failure'),
		SNOMED = sampleSNOMED()),
		conceptId(c('Finding site', 'Is a'), SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED()), c(TRUE, TRUE))
})

test_that('Ancestors', {
	expect_equal(ancestors(conceptId(c('Heart failure',
		'Acute heart failure'), SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED()), as.integer64('105981003'))
})

test_that('Semantic types', {
	expect_equal(semanticType(conceptId('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED()), 'disorder')
	expect_equal(semanticType(conceptId('Suspected heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED()), 'situation')
	expect_equal(semanticType(conceptId('Is a', SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED()), 'attribute')
})

test_that('Simplify - find closest single ancestor', {
	possible_ancestors <- c('Heart failure', 'Acute heart failure',
		'Cardiorenal syndrome')
	original_terms <- c('Systolic heart failure',
		'Is a',
		'Heart failure with reduced ejection fraction',
		'Acute kidney injury due to circulatory failure')
	expected_ancestors <- c('Heart failure',
		'Is a',
		'Heart failure',
		'Acute kidney injury due to circulatory failure') 
	# Note in this example 'Is a' has no parents in ancestors,
	# and acute kidney failure has two parents in ancestors
	# so neither of the parents will be chosen.
	# Also test out inclusion of duplicate concepts.
	found_ancestors <- simplify(conceptId(original_terms,
		unique = FALSE, SNOMED = sampleSNOMED())[c(1:4, 3:4)],
		conceptId(possible_ancestors, SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED())
	expect_equal(found_ancestors$ancestorId,
		conceptId(expected_ancestors, unique = FALSE,
		SNOMED = sampleSNOMED())[c(1:4, 3:4)])
})

