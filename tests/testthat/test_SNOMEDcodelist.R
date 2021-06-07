require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('SNOMED codelists')

test_that('Checking conversion of SNOMED concepts in data.table and data.frame', {
	myconcepts <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
	
	# data.frame
	frame <- data.frame(conceptId = myconcepts)
	expect_equal(names(frame), 'conceptId')
	expect_equal(class(frame[[1]]), 'integer64')

	# data.frame with as.integer64
	frame_int64 <- data.frame(conceptId = bit64::as.integer64(myconcepts))
	expect_equal(names(frame_int64), 'conceptId')
	expect_equal(class(frame_int64[[1]]), 'integer64')
	
	# data.frame with SNOMEDconcept
	frame_concept <- data.frame(conceptId = as.SNOMEDconcept(myconcepts))
	expect_equal(names(frame_concept), 'conceptId')
	expect_equal(class(frame_concept[[1]]), 'integer64')
	
	# data.table (should retain SNOMEDconcept class)
	table <- data.table(conceptId = myconcepts)
	expect_equal(names(table), 'conceptId')
	expect_equal(class(table[[1]]), c('SNOMEDconcept', 'integer64'))
	
	# data.table with as.integer64
	table_int64 <- data.table(conceptId = bit64::as.integer64(myconcepts))
	expect_equal(names(table_int64), 'conceptId')
	expect_equal(class(table_int64[[1]]), 'integer64')
	
	# data.table with SNOMEDconcept (should retain SNOMEDconcept class)
	table_concept <- data.table(conceptId = as.SNOMEDconcept(myconcepts))
	expect_equal(names(table_concept), 'conceptId')
	expect_equal(class(table_concept[[1]]), c('SNOMEDconcept', 'integer64'))
})

test_that('Creating codelists from concept IDs or tables', {
	myconcepts <- SNOMEDconcept('Heart failure', SNOMED = sampleSNOMED())
	# Converting to SNOMEDcodelist from a vector
	concept_codelist <- SNOMEDcodelist(myconcepts, SNOMED = sampleSNOMED())
	# Converting from a data.frame
	table_codelist <- as.SNOMEDcodelist(data.frame(conceptId = myconcepts,
		include_desc = TRUE), SNOMED = sampleSNOMED())
	# Converting from a data.frame without 'include_desc' (default = TRUE)
	table_codelist2 <- as.SNOMEDcodelist(data.frame(conceptId = myconcepts),
		SNOMED = sampleSNOMED())
	# Converting from a data.table
	table_codelist3 <- as.SNOMEDcodelist(data.table(conceptId = myconcepts,
		include_desc = TRUE), SNOMED = sampleSNOMED())
	# Converting from a data.table with extra columns
	table_codelist4 <- as.SNOMEDcodelist(data.table(conceptId = myconcepts,
		nice = 1, include_desc = TRUE), SNOMED = sampleSNOMED())
	# Comparisions
	expect_equal(all.equal(concept_codelist, table_codelist), TRUE)
	expect_equal(all.equal(concept_codelist, table_codelist2), TRUE)
	expect_equal(all.equal(concept_codelist, table_codelist3), TRUE)
	# Test for euality ignoring attributes (which are dropped by subsetting)
	expect_equal(all.equal(
		concept_codelist[, .(conceptId, include_desc, term)],
		table_codelist4[, .(conceptId, include_desc, term)]), TRUE)
	# Expect an error if the column is not named conceptId
	expect_error(table_codelist <- as.SNOMEDcodelist(
		data.frame(x = myconcepts, include_desc = TRUE),
		SNOMED = sampleSNOMED()))
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
	roundtrip_codelist <- contractSNOMED(expanded_codelist, SNOMED = sampleSNOMED())
	data.table::setindex(my_codelist, NULL)
	data.table::setindex(roundtrip_codelist, NULL)
	expect_equal(all.equal(my_codelist, roundtrip_codelist), TRUE)
})

test_that('Related concepts for a NULL list', {
	my_concepts <- as.SNOMEDconcept('0')[-1]
	related_concepts <- relatedConcepts(my_concepts,
		SNOMED = sampleSNOMED())
	expect_equal(my_concepts, related_concepts)
	expect_equal(my_concepts, parents(related_concepts))
	expect_equal(my_concepts, children(related_concepts))
	expect_equal(my_concepts, ancestors(related_concepts))
	expect_equal(my_concepts, descendants(related_concepts))
})

test_that('Expand codelist with nothing to expand', {
	my_concepts <- as.SNOMEDconcept(
		c('Heart failure', 'Acute heart failure'),
		SNOMED = sampleSNOMED())
	my_codelist <- SNOMEDcodelist(data.frame(conceptId = my_concepts,
		include_desc = FALSE), SNOMED = sampleSNOMED())
	expanded_codelist <- expandSNOMED(my_codelist,
		SNOMED = sampleSNOMED())
	roundtrip_codelist <- contractSNOMED(expanded_codelist,
		SNOMED = sampleSNOMED())
	
	# Check that the roundtrip codelist is same as the original
	# (ignore indices)
	data.table::setindex(my_codelist, NULL)
	data.table::setindex(roundtrip_codelist, NULL)
	expect_true(attr(expanded_codelist, 'Expanded'))
	expect_equal(all.equal(my_codelist, roundtrip_codelist), TRUE)
	
	# Check that attributes are as expected
	expect_false(attr(my_codelist, 'Expanded'))
	expect_false(attr(roundtrip_codelist, 'Expanded'))
	expect_true(attr(expanded_codelist, 'Expanded'))
	# If the attribute is changed, expanded is equal to original
	data.table::setattr(expanded_codelist, 'Expanded', FALSE)
	data.table::setindex(my_codelist, NULL)
	data.table::setindex(expanded_codelist, NULL)
	expect_equal(all.equal(my_codelist, expanded_codelist), TRUE)
})

test_that('Safely contract codelist', {
	my_codelist <- as.SNOMEDcodelist(data.frame(
		conceptId = SNOMEDconcept(c('Heart failure', 'Is a'),
		SNOMED = sampleSNOMED()),
		include_desc = c(TRUE, NA)), SNOMED = sampleSNOMED())
	expanded_codelist <- expandSNOMED(my_codelist,
		SNOMED = sampleSNOMED())
	roundtrip_codelist <- contractSNOMED(expanded_codelist,
		SNOMED = sampleSNOMED())
	data.table::setindex(my_codelist, NULL)
	data.table::setindex(roundtrip_codelist, NULL)
	expect_equal(all.equal(my_codelist, roundtrip_codelist), TRUE)
})
