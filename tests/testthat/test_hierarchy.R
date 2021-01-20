context('SNOMED hierarchy')

test_that('Related concepts', {
	SNOMED <- sampleSNOMED()
	expect_equal(relatedConcepts(conceptId('Heart failure'),
		typeId = conceptId('Finding site')),
		conceptId('Heart structure'))
})

test_that('Attributes', {
	SNOMED <- sampleSNOMED()
	expect_equal(hasAttributes(
		conceptId(c('Heart failure', 'Acute heart failure')),
		conceptId(c('Heart structure', 'Heart failure')),
		conceptId(c('Finding site', 'Is a'))), c(TRUE, TRUE))
})

test_that('Ancestors and descendants', {
	SNOMED <- sampleSNOMED()
	expect_equal(ancestors(conceptId(c('Heart failure',
		'Acute heart failure'))), as.integer64('105981003'))
	expect_equal(ancestors(conceptId(c('Heart failure',
		'Acute heart failure'))))
})

test_that('Semantic types', {
	expect_equal(semanticType(conceptId('Heart failure')), 'disorder')
	expect_equal(semanticType(conceptId('Suspected heart failure')),
		'situation')
	expect_equal(semanticType(conceptId('Is a')), 'attribute')
})

test_that('Simplify - find closest single ancestor', {
	SNOMED <- sampleSNOMED()
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
	found_ancestors <- simplify(
		conceptId(original_terms, unique = FALSE)[c(1:4, 3:4)],
		conceptId(possible_ancestors))
	expect_equal(found_ancestors$ancestorId,
		conceptId(expected_ancestors, unique = FALSE)[c(1:4, 3:4)])
})

