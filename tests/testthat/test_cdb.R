require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('CDB generation and decomposition using sample SNOMED')

test_that('Testing createCDB and decomposition', {
	data(MANUAL_SYNONYMS)
	miniCDB <- createCDB(SNOMED = sampleSNOMED(),
		MANUAL_SYNONYMS = MANUAL_SYNONYMS)
	expect_true(SNOMEDconcept('80891009', SNOMED = sampleSNOMED()) %in%
		miniCDB$BODY$conceptId)
	expect_true(SNOMEDconcept('84114007', SNOMED = sampleSNOMED()) %in%
		miniCDB$FINDINGS$conceptId)
	expect_identical(sort(tables(env = miniCDB)$NAME),
		c('BODY', 'BODY_LATERALITY', 'CAUSES', 'FINDINGS',
		'LATERALITY', 'MORPH', 'OTHERCAUSE', 'OVERLAP',
		'QUAL', 'SEMTYPE', 'SEVERITY', 'STAGE', 'TRANSITIVE'))
	D <- decompose(SNOMEDconcept('83291003'), CDB = miniCDB,
		SNOMED = sampleSNOMED())
	expect_true(SNOMEDconcept('128404006', SNOMED = sampleSNOMED()) %in%
		D$rootId)
	expect_true(SNOMEDconcept('367363000', SNOMED = sampleSNOMED()) %in%
		D$rootId)
	expect_true(SNOMEDconcept('70995007', SNOMED = sampleSNOMED()) %in%
		D$due_to)
#~ 	--------------------------------------------------------------------------------
#~ 83291003 | Cor pulmonale (disorder)
#~ --------------------------------------------------------------------------------
#~ Root : 128404006 | Right heart failure (disorder)
#~ - Due to : 70995007 | Pulmonary hypertension (disorder)
#
#~ --------------------------------------------------------------------------------
#~ 83291003 | Cor pulmonale (disorder)
#~ --------------------------------------------------------------------------------
#~ Root : 367363000 | Right ventricular failure (disorder)
#~ - Due to : 70995007 | Pulmonary hypertension (disorder)
})
