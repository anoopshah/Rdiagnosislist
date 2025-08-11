require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('CDB generation and decomposition using sample SNOMED')

test_that('Testing createCDB and decomposition', {
	data.table::setDTthreads(threads = 1)
	data(MANUAL_SYNONYMS)
	miniSNOMED <- sampleSNOMED()
	miniCDB <- createCDB(SNOMED = miniSNOMED,
		MANUAL_SYNONYMS = MANUAL_SYNONYMS)
	expect_true(SNOMEDconcept('80891009', SNOMED = miniSNOMED) %in%
		miniCDB$BODY$conceptId)
	expect_true(SNOMEDconcept('84114007', SNOMED = miniSNOMED) %in%
		miniCDB$FINDINGS$conceptId)
	expect_identical(sort(tables(env = miniCDB)$NAME),
		c('BODY', 'CAUSES', 'FINDINGS',
		'LATERALITY', 'MORPH', 'OTHERCAUSE', 'OVERLAP',
		'QUAL', 'SEMTYPE', 'SEVERITY', 'STAGE', 'TRANSITIVE'))
	D <- decompose(SNOMEDconcept('83291003', SNOMED = miniSNOMED),
		CDB = miniCDB, SNOMED = miniSNOMED)
	expect_true(SNOMEDconcept('128404006', SNOMED = miniSNOMED) %in%
		D$rootId)
	expect_true(SNOMEDconcept('367363000', SNOMED = miniSNOMED) %in%
		D$rootId)
	expect_true(SNOMEDconcept('19829001', SNOMED = miniSNOMED) %in%
		D$due_to)
#~ 	--------------------------------------------------------------------------------
#~ 83291003 | Cor pulmonale (disorder)
#~ --------------------------------------------------------------------------------
#~ Root : 128404006 | Right heart failure (disorder)
#~ - Due to : 19829001 | Disorder of lung (disorder)
#
#~ --------------------------------------------------------------------------------
#~ 83291003 | Cor pulmonale (disorder)
#~ --------------------------------------------------------------------------------
#~ Root : 367363000 | Right ventricular failure (disorder)
#~ - Due to : 19829001 | Disorder of lung (disorder)
	miniCDB <- addComposeLookupToCDB(D, CDB = miniCDB)
	expect_equal(
		compose(SNOMEDconcept('128404006', SNOMED = miniSNOMED),
		due_to_conceptIds = SNOMEDconcept('19829001', SNOMED = miniSNOMED),
		CDB = miniCDB, SNOMED = miniSNOMED),
		SNOMEDconcept('83291003', SNOMED = miniSNOMED))#
	expect_false(miniCDB$metadata$UMLS_included)
})

test_that('Test createCDB with UMLS', {
	data.table::setDTthreads(threads = 1)
	miniSNOMED <- sampleSNOMED()
	# Export UMLS to a temporary file
	umlsfile <- paste0(tempdir(), '/testumls.rrf')
	# Sample UMLS format file with two rows
	write(c('C0000005|ENG|P|L0000005|PF|S0007492|Y|A26634265||M0019694|D012711|SNOMEDCT_US|PEP|127337006|Acute heart failure|0|N|256|',
		'C0000005|ENG|S|L0270109|PF|S0007491|Y|A26634266||M0019694|D012711|MSH|ET|D012711|Rapid onset cardiac insufficiency|0|N|256|',
		'C0000005|ENG|S|L0270109|PF|S0007491|Y|A26634266||M0019694|D012711|MSH|ET|D012712|Rapid onset CI (diagnosis)|0|N|256|'),
		file = umlsfile)
	# Create miniCDB
	miniCDB <- createCDB(SNOMED = miniSNOMED,
		UMLS = umlsfile)
	# Delete temporary file
	unlink(paste0(tempdir(), '/testumls.rrf'))
	
	# Test for inclusion of UMLS concepts
	expect_equal(nrow(miniCDB$FINDINGS[
		conceptId == bit64::as.integer64('127337006') &
		term == ' rapid onset cardiac insufficiency ']), 1)
	expect_true(miniCDB$metadata$UMLS_included)
})
