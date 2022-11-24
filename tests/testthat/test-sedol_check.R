testthat::test_that("sedol_check NULL", {
  testthat::expect_equal(sedol_check(character(0)), logical(0))
})

testthat::test_that("sedol_check NA_character_", {
  testthat::expect_false(sedol_check(NA_character_))
})

test_that("sedol_check correct SEDOL", {
  testthat::expect_true(sedol_check("5437078"))
  testthat::expect_true(sedol_check("B923935"))
  testthat::expect_true(sedol_check("BF8K6K8"))
  testthat::expect_true(sedol_check("4846288"))
  testthat::expect_true(sedol_check("0263494"))
  testthat::expect_true(sedol_check("2515182"))
  testthat::expect_true(sedol_check("5748521"))
  testthat::expect_true(sedol_check("2064253"))
  testthat::expect_true(sedol_check("B28XP76"))
})

test_that("sedol_check long or short", {
  # length should be exactly 7 characters
  testthat::expect_true(!sedol_check("54370780"))
  testthat::expect_true(!sedol_check("543707"))
})

test_that("sedol_check vowels", {
  testthat::expect_true(!sedol_check("A437078"))
  testthat::expect_true(!sedol_check("E437078"))
  testthat::expect_true(!sedol_check("I437078"))
  testthat::expect_true(!sedol_check("U437078"))
})

test_that("sedol_check no lowercase", {
  testthat::expect_true(!sedol_check("b923935"))
})

test_that("sedol_check bad checksum digit", {
  testthat::expect_true(!sedol_check("5437070"))
  testthat::expect_true(!sedol_check("5437071"))
  testthat::expect_true(!sedol_check("5437072"))
  testthat::expect_true(!sedol_check("5437073"))
  testthat::expect_true(!sedol_check("5437074"))
  testthat::expect_true(!sedol_check("5437075"))
  testthat::expect_true(!sedol_check("5437076"))
  testthat::expect_true(!sedol_check("5437077"))
  testthat::expect_true(!sedol_check("5437079"))

  testthat::expect_true(!sedol_check("B923930"))
  testthat::expect_true(!sedol_check("B923931"))
  testthat::expect_true(!sedol_check("B923932"))
  testthat::expect_true(!sedol_check("B923933"))
  testthat::expect_true(!sedol_check("B923934"))
  testthat::expect_true(!sedol_check("B923936"))
  testthat::expect_true(!sedol_check("B923937"))
  testthat::expect_true(!sedol_check("B923938"))
  testthat::expect_true(!sedol_check("B923939"))
})
