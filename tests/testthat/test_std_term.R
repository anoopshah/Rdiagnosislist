require(testthat)
require(Rdiagnosislist)
require(bit64)
require(data.table)

context('Term standardisation')

test_that('std_term function', {
	expect_equal(std_term('HbA1c'), ' HbA1c ')
	expect_equal(std_term('nSTEMI'), ' nSTEMI ')
	expect_equal(std_term('Fever'), ' fever ')
	expect_equal(std_term('MCA stroke'), ' MCA stroke ')
	expect_equal(std_term('fever-pitch',
		hyphens_to_space = TRUE), ' fever pitch ')
	expect_equal(std_term('fracture of femur',
		remove_stopwords= TRUE), ' fracture femur ')
	expect_equal(std_term('ACS (acute coronary syndrome)',
		remove_words_in_parentheses = TRUE), ' ACS ')
})
