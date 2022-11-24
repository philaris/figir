expand_helper <- function(x) {
  if (is.na(x)) {
    integer(0L)
  } else {
    as.integer(substring(x, seq(nchar(x)), seq(nchar(x))))
  }
}

expand_digits <- function(v) {
  base::Reduce(c, base::lapply(v, expand_helper))
}

mul_isin_f <- function(v) {
  v * rev(rep(c(2L, 1L), length.out = length(v)))
}

isin_compute_checksum <- function(s) {
  u8_l <- figi_char_to_utf8(s)
  figi_code_l <- base::lapply(u8_l, FUN = figi_utf8_to_code)
  digi_code_l <- base::lapply(figi_code_l, FUN = expand_digits)
  mul12_l <- base::lapply(digi_code_l, FUN = mul_isin_f)
  figi_sum2_l <- base::lapply(mul12_l, FUN = figi_sum_digits)
  sum_digits_v <- base::vapply(figi_sum2_l, FUN = sum, FUN.VALUE = NA_integer_)
  check_digit_v <- -sum_digits_v %% 10L
  as.character(check_digit_v)
}

isin_has_correct_checksum <- function(s) {
  isin_compute_checksum(substr(s, 1L, 11L)) == substr(s, 12L, 12L)
}

#' Check validity of ISIN
#'
#' Given a character vector, check the validity of ISIN
#' (International Securities Identification Number)
#' for each of its elements.
#'
#' @param s a character vector for whose elements validity of ISIN
#'   (International Securities Identification Number) is checked.
#'
#' @return A logical vector.
#'
#' @examples
#' isin_check("BBG000BLNQ16")
#' isin_check("NRG92C84SB39")
#' isin_check(c("BBG000BLNQ16", "NRG92C84SB39"))
#'
#' @export
isin_check <- function(s) {
  !is.na(s) &
  base::nchar(s) == 12L &
    grepl("^[A-Z]{2}[A-Z0-9]{9}[0-9]$",
          s) &
    isin_has_correct_checksum(s)
}
