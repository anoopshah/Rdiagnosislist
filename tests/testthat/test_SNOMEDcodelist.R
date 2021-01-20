context('SNOMED codelists')

test_that('Expand and contract codelists', {
	SNOMED <- sampleSNOMED()
	my_concepts <- conceptId('Heart failure')
	my_codelist <- SNOMEDcodelist(data.table(conceptId = my_concepts,
		include_desc = TRUE))
	expanded_codelist <- expandSNOMED(my_codelist)
	roundtrip_codelist <- contractSNOMED(expanded_codelist)
	setindex(my_codelist, NULL)
	setindex(roundtrip_codelist, NULL)
	expect_equal(all.equal(my_codelist, roundtrip_codelist), TRUE)
})
