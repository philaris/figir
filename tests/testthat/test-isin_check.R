testthat::test_that("isin_check NULL", {
  testthat::expect_equal(isin_check(character(0)), logical(0))
})

testthat::test_that("isin_check NA_character_", {
  testthat::expect_false(isin_check(NA_character_))
})

test_that("isin_check correct ISIN", {
  expect_true(isin_check("GRS003003027"))
  expect_true(isin_check("GRS087103008"))
  expect_true(isin_check("FR0010479956"))
  expect_true(isin_check("GB00BHJYC057"))
  expect_true(isin_check("BE0974293251"))
  expect_true(isin_check("DE0005190003"))
  expect_true(isin_check("IT0001052049"))
  expect_true(isin_check("GB00BH0P3Z91"))
  expect_true(isin_check("US0378331005"))
})

test_that("isin_check long or short", {
  # length should be exactly 12 characters
  expect_true(!isin_check("GB00BHJYC0570"))
  expect_true(!isin_check("GB00BHJYC05"))
})

test_that("isin_check bad prefix", {
  expect_true(!isin_check("gRS003003027"))
  expect_true(!isin_check("GrS003003027"))
  expect_true(!isin_check("fr0010479956"))
  expect_true(!isin_check("##0010479956"))
  expect_true(!isin_check("g#0010479956"))
  expect_true(!isin_check("#e0010479956"))
})

test_that("isin_check number prefix", {
  # no digit in the first two chars
  expect_true(!isin_check("0RS003003027"))
  expect_true(!isin_check("G1S003003027"))
  expect_true(!isin_check("23S003003027"))
})

test_that("isin_check no lowercase", {
  expect_true(!isin_check("GB00BHjYC057"))
})

test_that("isin_check bad checksum digit", {
  expect_true(!isin_check("GRS003003020"))
  expect_true(!isin_check("GRS003003021"))
  expect_true(!isin_check("GRS003003022"))
  expect_true(!isin_check("GRS003003023"))
  expect_true(!isin_check("GRS003003024"))
  expect_true(!isin_check("GRS003003025"))
  expect_true(!isin_check("GRS003003026"))
  expect_true(!isin_check("GRS003003028"))
  expect_true(!isin_check("GRS003003029"))

  expect_true(!isin_check("GB00BHJYC050"))
  expect_true(!isin_check("GB00BHJYC051"))
  expect_true(!isin_check("GB00BHJYC052"))
  expect_true(!isin_check("GB00BHJYC053"))
  expect_true(!isin_check("GB00BHJYC054"))
  expect_true(!isin_check("GB00BHJYC055"))
  expect_true(!isin_check("GB00BHJYC056"))
  expect_true(!isin_check("GB00BHJYC058"))
  expect_true(!isin_check("GB00BHJYC059"))
})
