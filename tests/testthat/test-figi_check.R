testthat::test_that("figi_check NULL", {
  testthat::expect_equal(figi_check(character(0)), logical(0))
})

testthat::test_that("figi_check NA_character_", {
  testthat::expect_false(figi_check(NA_character_))
})

test_that("figi_check correct FIGI", {
  expect_true(figi_check("NRG92C84SB39"))
  expect_true(figi_check("BBG000BLNQ16"))
})

test_that("figi_check long or short", {
  # length should be exactly 12 characters
  expect_true(!figi_check("NRG92C84SB390"))
  expect_true(!figi_check("BBG000BLNQ1"))
})

test_that("figi_check bad prefix", {
  # bad prefix in: c("BS", "BM", "GG", "GB", "GH", "KY", "VG"))
  expect_true(!figi_check("BSG92C84SB39"))
  expect_true(!figi_check("BMG000BLNQ12"))
  expect_true(!figi_check("GGG92C84SB39"))
  expect_true(!figi_check("GHG000BLNQ12"))
  expect_true(!figi_check("KYG92C84SB39"))
  expect_true(!figi_check("VGG000BLNQ12"))
})

test_that("figi_check number prefix", {
  # no digit in the first two chars
  expect_true(!figi_check("0SG92C84SB39"))
  expect_true(!figi_check("B1G000BLNQ12"))
  expect_true(!figi_check("23G92C84SB39"))
})

test_that("figi_check no vowels", {
  # no vowels allowed
  expect_true(!figi_check("ABG92C84SB39"))
  expect_true(!figi_check("BBG0E0BLNQ12"))
  expect_true(!figi_check("BBG92C84SBU9"))
  expect_true(!figi_check("BBG0I0BLNQ12"))
  expect_true(!figi_check("BBG92COOSB39"))
  expect_true(!figi_check("AAG000BLNO12"))
})

test_that("figi_check 3rd character is G", {
  # 3rd character should be G
  expect_true(!figi_check("NRD92C84SB39"))
})

test_that("figi_check no lowercase", {
  expect_true(!figi_check("NRG92C84Sb39"))
})

test_that("figi_check bad checksum digit", {
  expect_true(!figi_check("NRG92C84SB30"))
  expect_true(!figi_check("NRG92C84SB31"))
  expect_true(!figi_check("NRG92C84SB32"))
  expect_true(!figi_check("NRG92C84SB33"))
  expect_true(!figi_check("NRG92C84SB34"))
  expect_true(!figi_check("NRG92C84SB35"))
  expect_true(!figi_check("NRG92C84SB36"))
  expect_true(!figi_check("NRG92C84SB37"))
  expect_true(!figi_check("NRG92C84SB38"))

  expect_true(!figi_check("BBG000BLNQ10"))
  expect_true(!figi_check("BBG000BLNQ11"))
  expect_true(!figi_check("BBG000BLNQ12"))
  expect_true(!figi_check("BBG000BLNQ13"))
  expect_true(!figi_check("BBG000BLNQ14"))
  expect_true(!figi_check("BBG000BLNQ15"))
  expect_true(!figi_check("BBG000BLNQ17"))
  expect_true(!figi_check("BBG000BLNQ18"))
  expect_true(!figi_check("BBG000BLNQ19"))
})

test_that("figi_compute_checksum", {
  expect_equal(figi_compute_checksum("BBG000BLNQ1"), "6")
  expect_equal(figi_compute_checksum("NRG92C84SB3"), "9")
})
