testthat::test_that("cusip_check NULL", {
  testthat::expect_equal(cusip_check(character(0)), logical(0))
})

testthat::test_that("cusip_check NA_character_", {
  testthat::expect_false(cusip_check(NA_character_))
})

test_that("cusip_check correct CUSIP", {
  testthat::expect_true(cusip_check("052800109"))
  testthat::expect_true(cusip_check("87162M409"))
  testthat::expect_true(cusip_check("500750104"))
  testthat::expect_true(cusip_check("037833100"))
  testthat::expect_true(cusip_check("17275R102"))
  testthat::expect_true(cusip_check("38259P508"))
  testthat::expect_true(cusip_check("594918104"))
  testthat::expect_true(cusip_check("68389X105"))
})

test_that("cusip_check long or short", {
  # length should be exactly 9 characters
  testthat::expect_false(cusip_check("0528001091"))
  testthat::expect_false(cusip_check("05280010"))
})

test_that("cusip_check no lowercase", {
  testthat::expect_false(cusip_check("87162m409"))
})

test_that("cusip_check bad checksum digit", {
  testthat::expect_true(!cusip_check("052800100"))
  testthat::expect_true(!cusip_check("052800101"))
  testthat::expect_true(!cusip_check("052800102"))
  testthat::expect_true(!cusip_check("052800103"))
  testthat::expect_true(!cusip_check("052800104"))
  testthat::expect_true(!cusip_check("052800105"))
  testthat::expect_true(!cusip_check("052800106"))
  testthat::expect_true(!cusip_check("052800107"))
  testthat::expect_true(!cusip_check("052800108"))

  testthat::expect_true(!cusip_check("17275R100"))
  testthat::expect_true(!cusip_check("17275R101"))
  testthat::expect_true(!cusip_check("17275R103"))
  testthat::expect_true(!cusip_check("17275R104"))
  testthat::expect_true(!cusip_check("17275R105"))
  testthat::expect_true(!cusip_check("17275R106"))
  testthat::expect_true(!cusip_check("17275R107"))
  testthat::expect_true(!cusip_check("17275R108"))
  testthat::expect_true(!cusip_check("17275R109"))
})
