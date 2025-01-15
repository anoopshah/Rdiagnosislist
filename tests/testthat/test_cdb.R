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
		c('BODY', 'BODY_LATERALITY', 'CAUSES', 'FINDINGS',
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
	expect_equal(compose(SNOMEDconcept('128404006', SNOMED = miniSNOMED),
		due_to_conceptIds = SNOMEDconcept('19829001', SNOMED = miniSNOMED),
		CDB = miniCDB, SNOMED = miniSNOMED),
		SNOMEDconcept('83291003', SNOMED = miniSNOMED))
})
