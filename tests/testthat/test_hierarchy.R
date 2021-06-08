require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('SNOMED hierarchy')

test_that('Related concepts', {
	expect_equal(relatedConcepts(
		as.SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED()),
		typeId = as.SNOMEDconcept('Finding site', SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED()),
		as.SNOMEDconcept('Heart structure', SNOMED = sampleSNOMED()))
})

test_that('hasAttributes', {
	expect_equal(hasAttributes(
		as.SNOMEDconcept(c('Heart failure', 'Acute heart failure'),
		SNOMED = sampleSNOMED()),
		as.SNOMEDconcept(c('Heart structure', 'Heart failure'),
		SNOMED = sampleSNOMED()),
		as.SNOMEDconcept(c('Finding site', 'Is a'), SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED()), c(TRUE, TRUE))
})

test_that('attrConcept', {
	attrTable <- attrConcept(as.SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED())
	expect_true(attrTable[,
		all(sourceId == as.SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()) |
		destinationId == as.SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()))])
})

test_that('Ancestors', {
	expect_equal(ancestors(as.SNOMEDconcept(c('Heart failure',
		'Acute heart failure'), SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED()), as.SNOMEDconcept('105981003'))
})

test_that('Semantic types', {
	expect_equal(semanticType(as.SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED()), 'disorder')
	expect_equal(semanticType(as.SNOMEDconcept('Suspected heart failure',
		SNOMED = sampleSNOMED()), SNOMED = sampleSNOMED()), 'situation')
	expect_equal(semanticType(as.SNOMEDconcept('Is a', SNOMED = sampleSNOMED()),
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
	found_ancestors <- simplify(as.SNOMEDconcept(original_terms,
		unique = FALSE, SNOMED = sampleSNOMED())[c(1:4, 3:4)],
		as.SNOMEDconcept(possible_ancestors, SNOMED = sampleSNOMED()),
		SNOMED = sampleSNOMED())
	expect_equal(found_ancestors$ancestorId,
		as.SNOMEDconcept(expected_ancestors, unique = FALSE,
		SNOMED = sampleSNOMED())[c(1:4, 3:4)])
})

