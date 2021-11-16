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
	concept_codelist <- SNOMEDcodelist(myconcepts, SNOMED = sampleSNOMED(),
		include_desc = TRUE)
	# Converting from a data.frame
	table_codelist <- as.SNOMEDcodelist(data.frame(conceptId = myconcepts,
		include_desc = TRUE), SNOMED = sampleSNOMED())
	# Converting from a data.table
	table_codelist3 <- as.SNOMEDcodelist(data.table(conceptId = myconcepts,
		include_desc = TRUE), SNOMED = sampleSNOMED())
	# Converting from a data.table with extra columns
	table_codelist4 <- as.SNOMEDcodelist(data.table(conceptId = myconcepts,
		nice = 1, include_desc = TRUE), SNOMED = sampleSNOMED())
	# Comparisions
	expect_equal(all.equal(concept_codelist, table_codelist), TRUE)
	expect_equal(all.equal(concept_codelist, table_codelist3), TRUE)
	# Test for euality ignoring attributes (which are dropped by subsetting)
	expect_equal(all.equal(
		concept_codelist[, .(conceptId, term)],
		table_codelist4[, .(conceptId, term)]), TRUE)
	# Expect an error if the column is not named conceptId
	expect_error(table_codelist <- as.SNOMEDcodelist(
		data.frame(x = myconcepts, include_desc = TRUE),
		SNOMED = sampleSNOMED()))
})

test_that('Codelist with missing descriptions', {
	my_codelist <- SNOMEDcodelist('1234', SNOMED = sampleSNOMED())
	expect_equal(nrow(my_codelist), 1)
	expect_equal(my_codelist$conceptId, as.SNOMEDconcept('1234'))
	expect_equal(my_codelist$term, as.character(NA))
})

test_that('Expand and contract codelists', {
	my_concepts <- SNOMEDconcept('Heart failure',
		SNOMED = sampleSNOMED())
	orig <- SNOMEDcodelist(data.frame(conceptId = my_concepts,
		include_desc = TRUE), SNOMED = sampleSNOMED(),
		format = 'simple')[1:50]
	e1 <- expandSNOMED(orig, SNOMED = sampleSNOMED())
	e2 <- SNOMEDcodelist(orig, format = 'tree',
		SNOMED = sampleSNOMED(), show_excluded_descendants = TRUE)
	e3 <- SNOMEDcodelist(orig, format = 'exptree',
		SNOMED = sampleSNOMED(), show_excluded_descendants = TRUE)
	e4 <- contractSNOMED(e2, SNOMED = sampleSNOMED())
	e5 <- contractSNOMED(e3, SNOMED = sampleSNOMED())
	e1a <- SNOMEDcodelist(e1, format = 'simple', SNOMED = sampleSNOMED())
	e2a <- SNOMEDcodelist(e2, format = 'simple', SNOMED = sampleSNOMED())
	e3a <- SNOMEDcodelist(e3, format = 'simple', SNOMED = sampleSNOMED())
	e4a <- SNOMEDcodelist(e4, format = 'simple', SNOMED = sampleSNOMED())
	e5a <- SNOMEDcodelist(e5, format = 'simple', SNOMED = sampleSNOMED())
	
	expect_equal(all.equal(e4, e5), TRUE) # contracted tree
	expect_equal(all.equal(orig, e1a), TRUE) # exptree
	expect_equal(all.equal(orig, e2a), TRUE) # tree, incl excluded
	expect_equal(all.equal(orig, e3a), TRUE) # exptree, incl excluded
	expect_equal(all.equal(orig, e4a), TRUE) # tree, incl excluded -> tree
	expect_equal(all.equal(orig, e5a), TRUE) # exptree, incl excluded -> tree
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
		include_desc = FALSE), SNOMED = sampleSNOMED(), format = 'tree')
	expanded_codelist <- expandSNOMED(my_codelist,
		SNOMED = sampleSNOMED())
	roundtrip_codelist <- contractSNOMED(expanded_codelist,
		SNOMED = sampleSNOMED())
	
	# Check that the roundtrip codelist is same as the original
	# (ignore indices)
	data.table::setindex(my_codelist, NULL)
	data.table::setindex(roundtrip_codelist, NULL)
	expect_equal(all.equal(my_codelist, roundtrip_codelist), TRUE)
	
	# If the attribute is changed, expanded is equal to original
	data.table::setattr(expanded_codelist, 'format', 'tree')
	data.table::setindex(my_codelist, NULL)
	data.table::setindex(expanded_codelist, NULL)
	expect_equal(all.equal(my_codelist, expanded_codelist), TRUE)
})

test_that('Safely contract codelist', {
	my_codelist <- as.SNOMEDcodelist(data.frame(
		conceptId = SNOMEDconcept(c('Heart failure', 'Is a'),
		SNOMED = sampleSNOMED()), include_desc = c(TRUE, NA)),
		format = 'tree', SNOMED = sampleSNOMED())
	expanded_codelist <- expandSNOMED(my_codelist,
		SNOMED = sampleSNOMED())
	roundtrip_codelist <- contractSNOMED(expanded_codelist,
		SNOMED = sampleSNOMED())
	data.table::setindex(my_codelist, NULL)
	data.table::setindex(roundtrip_codelist, NULL)
	expect_equal(all.equal(my_codelist, roundtrip_codelist), TRUE)
})

test_that('Codelist with some concepts not in dictionary', {
	myconcepts <- SNOMEDconcept(c('78408007',
		'78643003', '9999999999'))
	expect_equal(nrow(SNOMEDcodelist(myconcepts,
		SNOMED = sampleSNOMED())), 3)
})
