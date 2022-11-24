figi_utf8_to_code <- function(int_v) {
  code_0 <- base::utf8ToInt("0")
  code_a <- base::utf8ToInt("A")
  pmax(base::ifelse(int_v < code_a,
                    int_v - code_0,
                    int_v - code_a + 10L),
       0L)
}

figi_sum_digits <- function(int_v) {
  (int_v %/% 10L) + (int_v %% 10L)
}

figi_char_to_utf8 <- function(s) {
  base::lapply(s, FUN = base::utf8ToInt)
}

mul_f <- function(v, mul_v) {
  v * rep(mul_v, length.out = length(v))
}

gen_compute_checksum <- function(s, mul_v) {
  u8_l <- figi_char_to_utf8(s)
  figi_code_l <- base::lapply(u8_l, FUN = figi_utf8_to_code)
  mul12_l <- base::lapply(figi_code_l, FUN = mul_f, mul_v)
  figi_sum2_l <- base::lapply(mul12_l, FUN = figi_sum_digits)
  sum_digits_v <- base::vapply(figi_sum2_l, FUN = sum, FUN.VALUE = NA_integer_)
  check_digit_v <- -sum_digits_v %% 10L
  as.character(check_digit_v)
}

#' Compute the FIGI check digit
#'
#' Given a character vector, compute for each string the
#' FIGI (Financial Instrument Global Identifier) checksum digit.
#'
#' @param v a character vector for whose elements the FIGI
#'   (Financial Instrument Global Identifier) checksum digit is computed.
#'
#' @return A character vector of single character strings.
#'
#' @examples
#' figi_compute_checksum("BBG000BLNQ1")
#' figi_compute_checksum("NRG92C84SB3")
#' figi_compute_checksum(c("BBG000BLNQ1", "NRG92C84SB3"))
#'
#' @export
figi_compute_checksum <- function(v) {
  gen_compute_checksum(v, c(1L, 2L))
}

figi_has_correct_checksum <- function(s) {
  figi_compute_checksum(substr(s, 1L, 11L)) == substr(s, 12L, 12L)
}

#' Check validity of FIGI
#'
#' Given a character vector, check the validity of FIGI (Financial
#' Instrument Global Identifier) for each of its elements.
#'
#' @param s a character vector for whose elements validity of FIGI
#'   (Financial Instrument Global Identifier) is checked.
#'
#' @return A logical vector.
#'
#' @examples
#' figi_check("BBG000BLNQ16")
#' figi_check("NRG92C84SB39")
#' figi_check(c("BBG000BLNQ16", "NRG92C84SB39"))
#'
#' @export
figi_check <- function(s) {
  !is.na(s) &
  base::nchar(s) == 12L &
    (! base::substr(s, 1L, 2L) %in%
       c("BS", "BM", "GG", "GB", "GH", "KY", "VG")) &
    grepl("^[B-DF-HJ-NP-TV-Z]{2}G[B-DF-HJ-NP-TV-Z0-9]{8}[0-9]",
          s) &
    figi_has_correct_checksum(s)
}
